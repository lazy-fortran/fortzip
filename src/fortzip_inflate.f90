module fortzip_inflate
    !! DEFLATE decoding (RFC 1951) with table lookup for codes up to FAST_BITS long.
    use iso_fortran_env, only: int8, int64
    use fortzip_tables, only: LEN_BASE, LEN_EXTRA, DIST_BASE, DIST_EXTRA, CL_ORDER
    implicit none
    private
    public :: inflate_raw, ZIP_OK, ZIP_ERR_DATA, ZIP_ERR_TRUNCATED, ZIP_ERR_CHECKSUM, &
        ZIP_ERR_HEADER

    integer, parameter :: ZIP_OK = 0, ZIP_ERR_DATA = 1, ZIP_ERR_TRUNCATED = 2, &
        ZIP_ERR_CHECKSUM = 3, ZIP_ERR_HEADER = 4

    integer, parameter :: FAST_BITS = 10, FAST_SIZE = 2**FAST_BITS

    type :: huffman_t
        !! fast(i): 16 * symbol + code length for the reversed FAST_BITS-bit
        !! prefix i, 0 when the code is longer (count/symbol decode it then).
        integer :: fast(0:FAST_SIZE - 1) = 0
        integer :: count(0:15) = 0
        integer :: symbol(0:319) = 0
    end type huffman_t

    type :: reader_t
        integer :: pos = 1 ! next byte of the input
        integer(int64) :: buf = 0 ! bits not yet consumed, lowest first
        integer :: nbits = 0
        integer :: pad = 0 ! zero bytes added past the end of the input
        integer :: status = ZIP_OK
        integer(int8), allocatable :: out(:)
        integer :: n = 0
    end type reader_t

contains

    subroutine inflate_raw(src, out, status, used)
        !! Raw DEFLATE data in src into out (exactly sized). `used` is the number
        !! of input bytes the stream occupied, for the framing that follows it.
        integer(int8), intent(in) :: src(:)
        integer(int8), allocatable, intent(out) :: out(:)
        integer, intent(out) :: status
        integer, intent(out), optional :: used
        type(reader_t) :: r
        integer :: last, kind
        allocate (r%out(max(4096, 4 * size(src))))
        do
            last = getbits(r, src, 1)
            kind = getbits(r, src, 2)
            if (r%status /= ZIP_OK) exit
            select case (kind)
            case (0)
                call stored(r, src)
            case (1)
                call fixed(r, src)
            case (2)
                call dynamic(r, src)
            case default
                r%status = ZIP_ERR_DATA
            end select
            if (r%status /= ZIP_OK) exit
            if (last == 1) exit
        end do
        status = r%status
        ! Bytes whose bits were consumed, the last one only partly.
        if (present(used)) used = min(size(src), (8 * (r%pos - 1 + r%pad) - r%nbits + 7) / 8)
        allocate (out(r%n))
        out = r%out(1:r%n)
    end subroutine inflate_raw

    ! --- bits ---------------------------------------------------------------

    subroutine refill(r, src, need)
        !! At least `need` (<= 56) bits in the buffer; zeros past the input's end.
        type(reader_t), intent(inout) :: r
        integer(int8), intent(in) :: src(:)
        integer, intent(in) :: need
        integer(int64) :: b
        do while (r%nbits < need)
            if (r%pos <= size(src)) then
                b = iand(int(src(r%pos), int64), 255_int64)
                r%pos = r%pos + 1
            else
                b = 0
                r%pad = r%pad + 1
            end if
            r%buf = ior(r%buf, ishft(b, r%nbits))
            r%nbits = r%nbits + 8
        end do
    end subroutine refill

    subroutine consume(r, n)
        type(reader_t), intent(inout) :: r
        integer, intent(in) :: n
        r%buf = ishft(r%buf, -n)
        r%nbits = r%nbits - n
        ! Consumed bits that came from the zero padding: the input was cut short.
        if (8 * r%pad > r%nbits) then
            if (r%status == ZIP_OK) r%status = ZIP_ERR_TRUNCATED
        end if
    end subroutine consume

    integer function getbits(r, src, n) result(v)
        type(reader_t), intent(inout) :: r
        integer(int8), intent(in) :: src(:)
        integer, intent(in) :: n
        if (n == 0) then
            v = 0
            return
        end if
        call refill(r, src, n)
        v = int(iand(r%buf, ishft(1_int64, n) - 1))
        call consume(r, n)
    end function getbits

    integer function decode(r, src, h) result(sym)
        !! Next symbol of code h, or -1 for an invalid code.
        type(reader_t), intent(inout) :: r
        integer(int8), intent(in) :: src(:)
        type(huffman_t), intent(in) :: h
        integer :: entry, code, first, index, len, cnt, peek
        call refill(r, src, 15)
        entry = h%fast(int(iand(r%buf, int(FAST_SIZE - 1, int64))))
        if (entry /= 0) then
            sym = ishft(entry, -4)
            call consume(r, iand(entry, 15))
            return
        end if
        ! Longer code: walk the canonical code one bit at a time (as in zlib's puff).
        peek = int(iand(r%buf, 32767_int64))
        code = 0
        first = 0
        index = 0
        do len = 1, 15
            code = ior(code, iand(ishft(peek, -(len - 1)), 1))
            cnt = h%count(len)
            if (code - cnt < first) then
                sym = h%symbol(index + (code - first))
                call consume(r, len)
                return
            end if
            index = index + cnt
            first = ishft(first + cnt, 1)
            code = ishft(code, 1)
        end do
        sym = -1
        r%status = ZIP_ERR_DATA
    end function decode

    ! --- output -------------------------------------------------------------

    subroutine reserve(r, extra)
        type(reader_t), intent(inout) :: r
        integer, intent(in) :: extra
        integer(int8), allocatable :: grown(:)
        if (r%n + extra <= size(r%out)) return
        allocate (grown(max(2 * size(r%out), r%n + extra)))
        grown(1:r%n) = r%out(1:r%n)
        call move_alloc(grown, r%out)
    end subroutine reserve

    ! --- blocks -------------------------------------------------------------

    subroutine stored(r, src)
        type(reader_t), intent(inout) :: r
        integer(int8), intent(in) :: src(:)
        integer :: length, nlength, k
        call consume(r, iand(r%nbits, 7))
        length = getbits(r, src, 16)
        nlength = getbits(r, src, 16)
        if (r%status /= ZIP_OK) return
        if (ieor(length, nlength) /= 65535) then
            r%status = ZIP_ERR_DATA
            return
        end if
        call reserve(r, length)
        ! Whole bytes still in the bit buffer come first, then the input itself.
        k = 0
        do while (k < length .and. r%nbits >= 8)
            r%out(r%n + k + 1) = to_byte(getbits(r, src, 8))
            k = k + 1
        end do
        if (length - k > size(src) - r%pos + 1) then
            r%status = ZIP_ERR_TRUNCATED
            return
        end if
        r%out(r%n + k + 1:r%n + length) = src(r%pos:r%pos + length - k - 1)
        r%pos = r%pos + length - k
        r%n = r%n + length
    end subroutine stored

    subroutine fixed(r, src)
        type(reader_t), intent(inout) :: r
        integer(int8), intent(in) :: src(:)
        type(huffman_t), save :: lit, dist
        logical, save :: ready = .false.
        integer :: lengths(0:287)
        if (.not. ready) then
            lengths(0:143) = 8
            lengths(144:255) = 9
            lengths(256:279) = 7
            lengths(280:287) = 8
            ready = build(lit, lengths, 288)
            lengths(0:29) = 5
            ready = ready .and. build(dist, lengths, 30)
        end if
        call codes(r, src, lit, dist)
    end subroutine fixed

    subroutine dynamic(r, src)
        type(reader_t), intent(inout) :: r
        integer(int8), intent(in) :: src(:)
        type(huffman_t) :: lencode, lit, dist
        integer :: nlen, ndist, ncode, k, sym, rep, prev, lengths(0:319)
        nlen = getbits(r, src, 5) + 257
        ndist = getbits(r, src, 5) + 1
        ncode = getbits(r, src, 4) + 4
        if (nlen > 286 .or. ndist > 30) then
            r%status = ZIP_ERR_DATA
            return
        end if
        lengths = 0
        do k = 1, ncode
            lengths(CL_ORDER(k)) = getbits(r, src, 3)
        end do
        if (.not. build(lencode, lengths, 19)) then
            r%status = ZIP_ERR_DATA
            return
        end if
        lengths = 0
        k = 0
        do while (k < nlen + ndist)
            sym = decode(r, src, lencode)
            if (r%status /= ZIP_OK) return
            if (sym < 16) then
                lengths(k) = sym
                k = k + 1
                cycle
            end if
            prev = 0
            select case (sym)
            case (16)
                if (k == 0) then
                    r%status = ZIP_ERR_DATA
                    return
                end if
                prev = lengths(k - 1)
                rep = 3 + getbits(r, src, 2)
            case (17)
                rep = 3 + getbits(r, src, 3)
            case default
                rep = 11 + getbits(r, src, 7)
            end select
            if (k + rep > nlen + ndist) then
                r%status = ZIP_ERR_DATA
                return
            end if
            lengths(k:k + rep - 1) = prev
            k = k + rep
        end do
        if (lengths(256) == 0) then
            r%status = ZIP_ERR_DATA
            return
        end if
        if (.not. build(lit, lengths(0:nlen - 1), nlen)) r%status = ZIP_ERR_DATA
        if (.not. build(dist, lengths(nlen:nlen + ndist - 1), ndist)) r%status = ZIP_ERR_DATA
        if (r%status /= ZIP_OK) return
        call codes(r, src, lit, dist)
    end subroutine dynamic

    subroutine codes(r, src, lit, dist)
        type(reader_t), intent(inout) :: r
        integer(int8), intent(in) :: src(:)
        type(huffman_t), intent(in) :: lit, dist
        integer :: sym, length, d, k
        do
            sym = decode(r, src, lit)
            if (r%status /= ZIP_OK) return
            if (sym < 256) then
                call reserve(r, 1)
                r%n = r%n + 1
                r%out(r%n) = to_byte(sym)
            else if (sym == 256) then
                return
            else
                sym = sym - 257
                if (sym >= 29) then
                    r%status = ZIP_ERR_DATA
                    return
                end if
                length = LEN_BASE(sym) + getbits(r, src, LEN_EXTRA(sym))
                sym = decode(r, src, dist)
                if (r%status /= ZIP_OK) return
                if (sym >= 30) then
                    r%status = ZIP_ERR_DATA
                    return
                end if
                d = DIST_BASE(sym) + getbits(r, src, DIST_EXTRA(sym))
                if (d > r%n) then
                    r%status = ZIP_ERR_DATA
                    return
                end if
                call reserve(r, length)
                ! Byte by byte: a match may overlap the bytes it is producing.
                do k = r%n + 1, r%n + length
                    r%out(k) = r%out(k - d)
                end do
                r%n = r%n + length
            end if
        end do
    end subroutine codes

    logical function build(h, lengths, n) result(ok)
        !! Canonical code from code lengths; false for an over-subscribed set.
        !! Incomplete sets are accepted (a single code of length 1 is legal).
        type(huffman_t), intent(out) :: h
        integer, intent(in) :: lengths(0:), n
        integer :: offs(0:15), next_code(0:16), k, len, code, rev, step, left
        h%count = 0
        do k = 0, n - 1
            h%count(lengths(k)) = h%count(lengths(k)) + 1
        end do
        h%count(0) = 0
        left = 1
        do len = 1, 15
            left = 2 * left - h%count(len)
            if (left < 0) then
                ok = .false.
                return
            end if
        end do
        offs(1) = 0
        do len = 1, 14
            offs(len + 1) = offs(len) + h%count(len)
        end do
        do k = 0, n - 1
            if (lengths(k) /= 0) then
                h%symbol(offs(lengths(k))) = k
                offs(lengths(k)) = offs(lengths(k)) + 1
            end if
        end do
        ! Canonical codes, entered bit-reversed into the lookup table.
        code = 0
        next_code(0) = 0
        do len = 1, 15
            code = ishft(code + h%count(len - 1), 1)
            next_code(len) = code
        end do
        next_code(0) = 0
        do k = 0, n - 1
            len = lengths(k)
            if (len == 0) cycle
            code = next_code(len)
            next_code(len) = code + 1
            if (len > FAST_BITS) cycle
            rev = reverse(code, len)
            step = ishft(1, len)
            do while (rev < FAST_SIZE)
                h%fast(rev) = 16 * k + len
                rev = rev + step
            end do
        end do
        ok = .true.
    end function build

    pure integer(int8) function to_byte(v)
        integer, intent(in) :: v
        to_byte = int(v - 256 * (v / 128), int8)
    end function to_byte

    pure integer function reverse(code, len) result(rev)
        integer, intent(in) :: code, len
        integer :: k
        rev = 0
        do k = 0, len - 1
            rev = ior(ishft(rev, 1), iand(ishft(code, -k), 1))
        end do
    end function reverse

end module fortzip_inflate
