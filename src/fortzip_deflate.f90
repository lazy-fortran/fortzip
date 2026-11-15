module fortzip_deflate
    !! DEFLATE encoding (RFC 1951): LZ77 over a 32 KiB window with hash chains,
    !! then each block as stored, fixed or dynamic Huffman, whichever is shortest.
    use iso_fortran_env, only: int8, int64
    use fortzip_tables, only: LEN_BASE, LEN_EXTRA, DIST_BASE, DIST_EXTRA, CL_ORDER
    implicit none
    private
    public :: deflate_raw

    integer, parameter :: WSIZE = 32768, HASH_BITS = 15, HASH_SIZE = 2**HASH_BITS
    integer, parameter :: MIN_MATCH = 3, MAX_MATCH = 258
    ! Tokens per block: enough to amortise the dynamic header.
    integer, parameter :: BLOCK_TOKENS = 32768
    ! Hash chain lengths and lazy matching per level 1..9.
    integer, parameter :: CHAIN(9) = [4, 8, 16, 32, 64, 128, 256, 1024, 4096]
    integer, parameter :: NICE(9) = [16, 32, 32, 64, 128, 128, 192, 258, 258]

    type :: writer_t
        integer(int8), allocatable :: out(:)
        integer :: n = 0
        integer(int64) :: buf = 0
        integer :: nbits = 0
    end type writer_t

    type :: code_t
        !! Code lengths and the bit-reversed codes ready to be written.
        integer, allocatable :: len(:), rev(:)
    end type code_t

contains

    subroutine deflate_raw(src, out, level)
        !! src compressed into raw DEFLATE data; level 0 stores, 1..9 trade speed
        !! for size as in zlib (default 6).
        integer(int8), intent(in) :: src(:)
        integer(int8), allocatable, intent(out) :: out(:)
        integer, intent(in), optional :: level
        type(writer_t) :: w
        integer :: lvl, start, finish, nsrc
        integer, allocatable :: head(:), prev(:), tok_len(:), tok_dist(:)
        integer :: ntok, i, best_len, best_dist, next_len, next_dist

        lvl = 6
        if (present(level)) lvl = max(0, min(9, level))
        nsrc = size(src)
        allocate (w%out(max(1024, nsrc + nsrc / 8 + 64)))
        if (lvl == 0) then
            call store_all(w, src)
            call finish_bits(w)
            allocate (out(w%n))
            out = w%out(1:w%n)
            return
        end if

        allocate (head(0:HASH_SIZE - 1), prev(0:WSIZE - 1))
        allocate (tok_len(BLOCK_TOKENS), tok_dist(BLOCK_TOKENS))
        head = 0
        prev = 0
        ntok = 0
        start = 1
        i = 1
        do while (i <= nsrc)
            call longest(src, i, head, prev, CHAIN(lvl), NICE(lvl), best_len, best_dist)
            call insert(src, i, head, prev)
            ! One-step lazy matching: a longer match at the next byte wins.
            if (lvl >= 4 .and. best_len >= MIN_MATCH .and. best_len < NICE(lvl) .and. &
                i + 1 <= nsrc) then
                call longest(src, i + 1, head, prev, CHAIN(lvl) / 2, NICE(lvl), next_len, &
                    next_dist)
                if (next_len > best_len) best_len = 0
            end if
            if (best_len >= MIN_MATCH) then
                ntok = ntok + 1
                tok_len(ntok) = best_len
                tok_dist(ntok) = best_dist
                call insert_run(src, i + 1, i + best_len - 1, head, prev)
                i = i + best_len
            else
                ntok = ntok + 1
                tok_len(ntok) = byte(src(i))
                tok_dist(ntok) = 0
                i = i + 1
            end if
            if (ntok == BLOCK_TOKENS) then
                finish = i - 1
                call emit_block(w, src(start:finish), tok_len(1:ntok), tok_dist(1:ntok), &
                    finish == nsrc)
                start = i
                ntok = 0
            end if
        end do
        if (ntok > 0 .or. nsrc == 0 .or. start <= nsrc) &
            call emit_block(w, src(start:nsrc), tok_len(1:ntok), tok_dist(1:ntok), .true.)
        call finish_bits(w)
        allocate (out(w%n))
        out = w%out(1:w%n)
    end subroutine deflate_raw

    ! --- matching -----------------------------------------------------------

    pure integer function hash3(src, i) result(h)
        !! Multiplicative hash of the three bytes at i.
        integer(int8), intent(in) :: src(:)
        integer, intent(in) :: i
        integer(int64) :: v
        v = ior(ior(ishft(int(byte(src(i)), int64), 16), ishft(int(byte(src(i + 1)), int64), &
            8)), int(byte(src(i + 2)), int64))
        v = iand(v * 2654435761_int64, 4294967295_int64)
        h = int(ishft(v, -(32 - HASH_BITS)))
    end function hash3

    subroutine insert(src, i, head, prev)
        integer(int8), intent(in) :: src(:)
        integer, intent(in) :: i
        integer, intent(inout) :: head(0:), prev(0:)
        integer :: h
        if (i + 2 > size(src)) return
        h = hash3(src, i)
        prev(iand(i, WSIZE - 1)) = head(h)
        head(h) = i
    end subroutine insert

    subroutine insert_run(src, first, last, head, prev)
        integer(int8), intent(in) :: src(:)
        integer, intent(in) :: first, last
        integer, intent(inout) :: head(0:), prev(0:)
        integer :: k
        do k = first, last
            call insert(src, k, head, prev)
        end do
    end subroutine insert_run

    subroutine longest(src, i, head, prev, chain, nice, best_len, best_dist)
        !! Longest earlier match for the bytes at i within the window.
        integer(int8), intent(in) :: src(:)
        integer, intent(in) :: i, chain, nice
        integer, intent(in) :: head(0:), prev(0:)
        integer, intent(out) :: best_len, best_dist
        integer :: cand, limit, len, tries, maxlen, older
        best_len = 0
        best_dist = 0
        if (i + 2 > size(src)) return
        maxlen = min(MAX_MATCH, size(src) - i + 1)
        cand = head(hash3(src, i))
        limit = max(1, i - WSIZE)
        tries = chain
        do while (cand >= limit .and. cand < i .and. tries > 0)
            if (src(cand + best_len) == src(i + best_len)) then
                len = 0
                do while (len < maxlen)
                    if (src(cand + len) /= src(i + len)) exit
                    len = len + 1
                end do
                if (len > best_len) then
                    best_len = len
                    best_dist = i - cand
                    if (len >= nice .or. len == maxlen) exit
                end if
            end if
            older = prev(iand(cand, WSIZE - 1))
            ! A slot reused by a newer position ends the chain.
            if (older >= cand) exit
            cand = older
            tries = tries - 1
        end do
        if (best_len < MIN_MATCH) best_len = 0
    end subroutine longest

    ! --- blocks -------------------------------------------------------------

    subroutine emit_block(w, raw, tlen, tdist, last)
        !! One block of tokens covering the bytes `raw`, in its cheapest form.
        type(writer_t), intent(inout) :: w
        integer(int8), intent(in) :: raw(:)
        integer, intent(in) :: tlen(:), tdist(:)
        logical, intent(in) :: last
        integer :: lfreq(0:285), dfreq(0:29), k, lc, dc
        type(code_t) :: lit, dist, fixlit, fixdist
        integer, allocatable :: rle(:), rle_extra(:)
        integer :: nrle, hlit, hdist, hclen, cl_len(0:18), cl_freq(0:18)
        integer(int64) :: cost_dyn, cost_fix, cost_stored, extra_bits
        type(code_t) :: clcode

        lfreq = 0
        dfreq = 0
        extra_bits = 0
        do k = 1, size(tlen)
            if (tdist(k) == 0) then
                lfreq(tlen(k)) = lfreq(tlen(k)) + 1
            else
                lc = len_code(tlen(k))
                dc = dist_code(tdist(k))
                lfreq(257 + lc) = lfreq(257 + lc) + 1
                dfreq(dc) = dfreq(dc) + 1
                extra_bits = extra_bits + LEN_EXTRA(lc) + DIST_EXTRA(dc)
            end if
        end do
        lfreq(256) = 1

        ! Explicit bounds: a function result in an expression starts at index 1.
        allocate (lit%len(0:285), dist%len(0:29))
        lit%len(0:285) = huffman_lengths(lfreq, 15)
        dist%len(0:29) = huffman_lengths(dfreq, 15)
        hlit = 257
        do k = 285, 257, -1
            if (lit%len(k) /= 0) then
                hlit = k + 1
                exit
            end if
        end do
        hdist = 1
        do k = 29, 1, -1
            if (dist%len(k) /= 0) then
                hdist = k + 1
                exit
            end if
        end do
        call run_lengths([lit%len(0:hlit - 1), dist%len(0:hdist - 1)], rle, rle_extra, nrle)
        cl_freq = 0
        do k = 1, nrle
            cl_freq(rle(k)) = cl_freq(rle(k)) + 1
        end do
        cl_len = huffman_lengths(cl_freq, 7)
        hclen = 19
        do while (hclen > 4)
            if (cl_len(CL_ORDER(hclen)) /= 0) exit
            hclen = hclen - 1
        end do

        cost_dyn = 3 + 14 + 3 * hclen + extra_bits + sum(int(lfreq, int64) * lit%len) + &
            sum(int(dfreq, int64) * dist%len) + sum(int(cl_freq, int64) * cl_len) + &
            2 * count(rle(1:nrle) == 16) + 3 * count(rle(1:nrle) == 17) + &
            7 * count(rle(1:nrle) == 18)
        call fixed_codes(fixlit, fixdist)
        cost_fix = 3 + extra_bits + sum(int(lfreq, int64) * fixlit%len(0:285)) + &
            sum(int(dfreq, int64) * fixdist%len)
        cost_stored = 3 + 7 + 40_int64 * (size(raw) / 65535 + 1) + 8_int64 * size(raw)

        if (cost_stored <= min(cost_dyn, cost_fix)) then
            call store(w, raw, last)
        else if (cost_fix <= cost_dyn) then
            call put(w, merge(1, 0, last), 1)
            call put(w, 1, 2)
            call write_tokens(w, tlen, tdist, fixlit, fixdist)
        else
            call put(w, merge(1, 0, last), 1)
            call put(w, 2, 2)
            call put(w, hlit - 257, 5)
            call put(w, hdist - 1, 5)
            call put(w, hclen - 4, 4)
            do k = 1, hclen
                call put(w, cl_len(CL_ORDER(k)), 3)
            end do
            call make_codes(clcode, cl_len)
            do k = 1, nrle
                call put(w, clcode%rev(rle(k)), clcode%len(rle(k)))
                select case (rle(k))
                case (16)
                    call put(w, rle_extra(k), 2)
                case (17)
                    call put(w, rle_extra(k), 3)
                case (18)
                    call put(w, rle_extra(k), 7)
                end select
            end do
            call make_codes(lit, lit%len)
            call make_codes(dist, dist%len)
            call write_tokens(w, tlen, tdist, lit, dist)
        end if
    end subroutine emit_block

    subroutine write_tokens(w, tlen, tdist, lit, dist)
        type(writer_t), intent(inout) :: w
        integer, intent(in) :: tlen(:), tdist(:)
        type(code_t), intent(in) :: lit, dist
        integer :: k, lc, dc
        do k = 1, size(tlen)
            if (tdist(k) == 0) then
                call put(w, lit%rev(tlen(k)), lit%len(tlen(k)))
            else
                lc = len_code(tlen(k))
                call put(w, lit%rev(257 + lc), lit%len(257 + lc))
                call put(w, tlen(k) - LEN_BASE(lc), LEN_EXTRA(lc))
                dc = dist_code(tdist(k))
                call put(w, dist%rev(dc), dist%len(dc))
                call put(w, tdist(k) - DIST_BASE(dc), DIST_EXTRA(dc))
            end if
        end do
        call put(w, lit%rev(256), lit%len(256))
    end subroutine write_tokens

    subroutine store(w, raw, last)
        !! raw as stored blocks of at most 65535 bytes.
        type(writer_t), intent(inout) :: w
        integer(int8), intent(in) :: raw(:)
        logical, intent(in) :: last
        integer :: first, n
        first = 1
        do
            n = min(65535, size(raw) - first + 1)
            call put(w, merge(1, 0, last .and. first + n > size(raw)), 1)
            call put(w, 0, 2)
            call align(w)
            call put(w, n, 16)
            call put(w, ieor(n, 65535), 16)
            call reserve(w, n)
            w%out(w%n + 1:w%n + n) = raw(first:first + n - 1)
            w%n = w%n + n
            first = first + n
            if (first > size(raw)) exit
        end do
    end subroutine store

    subroutine store_all(w, src)
        type(writer_t), intent(inout) :: w
        integer(int8), intent(in) :: src(:)
        call store(w, src, .true.)
    end subroutine store_all

    subroutine run_lengths(lengths, sym, extra, n)
        !! Code lengths as code length symbols: 0..15 literally, 16 repeats the
        !! previous 3..6 times, 17 and 18 give 3..10 and 11..138 zeros.
        integer, intent(in) :: lengths(:)
        integer, allocatable, intent(out) :: sym(:), extra(:)
        integer, intent(out) :: n
        integer :: k, run, v
        allocate (sym(size(lengths)), extra(size(lengths)))
        n = 0
        k = 1
        do while (k <= size(lengths))
            v = lengths(k)
            run = 1
            do while (k + run <= size(lengths))
                if (lengths(k + run) /= v) exit
                run = run + 1
            end do
            k = k + run
            if (v == 0) then
                do while (run >= 11)
                    call add(18, min(run, 138) - 11)
                    run = run - min(run, 138)
                end do
                if (run >= 3) then
                    call add(17, run - 3)
                    run = 0
                end if
            else
                call add(v, 0)
                run = run - 1
                do while (run >= 3)
                    call add(16, min(run, 6) - 3)
                    run = run - min(run, 6)
                end do
            end if
            do while (run > 0)
                call add(v, 0)
                run = run - 1
            end do
        end do
    contains
        subroutine add(s, e)
            integer, intent(in) :: s, e
            n = n + 1
            sym(n) = s
            extra(n) = e
        end subroutine add
    end subroutine run_lengths

    ! --- Huffman codes ------------------------------------------------------

    function huffman_lengths(freq, limit) result(lengths)
        !! Code lengths for the frequencies, none longer than `limit`. Rare
        !! symbols are made more frequent until the tree is shallow enough.
        integer, intent(in) :: freq(0:), limit
        integer :: lengths(0:size(freq) - 1)
        integer :: f(0:size(freq) - 1), used, k
        f = freq
        used = count(f > 0)
        lengths = 0
        if (used == 0) return
        if (used == 1) then
            ! A single code of length 1 is a valid (incomplete) DEFLATE code.
            do k = 0, size(f) - 1
                if (f(k) > 0) lengths(k) = 1
            end do
            return
        end if
        do
            call tree_depths(f, lengths)
            if (maxval(lengths) <= limit) return
            where (f > 0) f = max(1, f / 2)
        end do
    end function huffman_lengths

    subroutine tree_depths(freq, depth)
        !! Depths of the leaves in a Huffman tree of the nonzero frequencies.
        integer, intent(in) :: freq(0:)
        integer, intent(out) :: depth(0:)
        integer :: n, m, k, a, b, nodes
        integer(int64), allocatable :: weight(:)
        integer, allocatable :: parent(:), leaf(:)
        logical, allocatable :: active(:)
        n = count(freq > 0)
        allocate (weight(2 * n), parent(2 * n), leaf(n), active(2 * n))
        m = 0
        do k = 0, size(freq) - 1
            if (freq(k) > 0) then
                m = m + 1
                leaf(m) = k
                weight(m) = freq(k)
            end if
        end do
        parent = 0
        active = .false.
        active(1:n) = .true.
        nodes = n
        do while (nodes < 2 * n - 1)
            a = lightest()
            active(a) = .false.
            b = lightest()
            active(b) = .false.
            nodes = nodes + 1
            weight(nodes) = weight(a) + weight(b)
            parent(a) = nodes
            parent(b) = nodes
            active(nodes) = .true.
        end do
        depth = 0
        do k = 1, n
            m = k
            do while (parent(m) /= 0)
                depth(leaf(k)) = depth(leaf(k)) + 1
                m = parent(m)
            end do
        end do
    contains
        integer function lightest() result(best)
            integer :: j
            best = 0
            do j = 1, nodes
                if (.not. active(j)) cycle
                if (best == 0) then
                    best = j
                else if (weight(j) < weight(best)) then
                    best = j
                end if
            end do
        end function lightest
    end subroutine tree_depths

    subroutine make_codes(c, lengths)
        !! Canonical codes for the lengths, stored bit-reversed for writing.
        type(code_t), intent(inout) :: c
        integer, intent(in) :: lengths(0:)
        integer :: bl_count(0:15), next(0:15), k, code, len
        integer :: l(0:size(lengths) - 1)
        l = lengths
        bl_count = 0
        do k = 0, size(l) - 1
            bl_count(l(k)) = bl_count(l(k)) + 1
        end do
        bl_count(0) = 0
        code = 0
        next = 0
        do len = 1, 15
            code = ishft(code + bl_count(len - 1), 1)
            next(len) = code
        end do
        if (allocated(c%rev)) deallocate (c%rev)
        allocate (c%rev(0:size(l) - 1))
        c%rev = 0
        do k = 0, size(l) - 1
            if (l(k) == 0) cycle
            c%rev(k) = reverse(next(l(k)), l(k))
            next(l(k)) = next(l(k)) + 1
        end do
        if (allocated(c%len)) deallocate (c%len)
        allocate (c%len(0:size(l) - 1))
        c%len = l
    end subroutine make_codes

    subroutine fixed_codes(lit, dist)
        type(code_t), intent(out) :: lit, dist
        integer :: lengths(0:287)
        lengths(0:143) = 8
        lengths(144:255) = 9
        lengths(256:279) = 7
        lengths(280:287) = 8
        call make_codes(lit, lengths)
        call make_codes(dist, spread(5, 1, 30))
    end subroutine fixed_codes

    pure integer function len_code(length) result(code)
        integer, intent(in) :: length
        integer :: k
        code = 28
        do k = 0, 27
            if (LEN_BASE(k + 1) > length) then
                code = k
                return
            end if
        end do
    end function len_code

    pure integer function dist_code(d) result(code)
        integer, intent(in) :: d
        integer :: lo, hi, mid
        lo = 0
        hi = 29
        do while (lo < hi)
            mid = (lo + hi + 1) / 2
            if (DIST_BASE(mid) <= d) then
                lo = mid
            else
                hi = mid - 1
            end if
        end do
        code = lo
    end function dist_code

    ! --- bit output ---------------------------------------------------------

    subroutine reserve(w, extra)
        type(writer_t), intent(inout) :: w
        integer, intent(in) :: extra
        integer(int8), allocatable :: grown(:)
        if (w%n + extra <= size(w%out)) return
        allocate (grown(max(2 * size(w%out), w%n + extra)))
        grown(1:w%n) = w%out(1:w%n)
        call move_alloc(grown, w%out)
    end subroutine reserve

    subroutine put(w, value, nbits)
        !! The low nbits of value, lowest bit first.
        type(writer_t), intent(inout) :: w
        integer, intent(in) :: value, nbits
        if (nbits == 0) return
        w%buf = ior(w%buf, ishft(iand(int(value, int64), ishft(1_int64, nbits) - 1), w%nbits))
        w%nbits = w%nbits + nbits
        if (w%nbits >= 32) call flush_bytes(w, 32)
    end subroutine put

    subroutine flush_bytes(w, bits)
        type(writer_t), intent(inout) :: w
        integer, intent(in) :: bits
        integer :: k
        call reserve(w, bits / 8)
        do k = 1, bits / 8
            w%n = w%n + 1
            w%out(w%n) = to_byte(int(iand(w%buf, 255_int64)))
            w%buf = ishft(w%buf, -8)
        end do
        w%nbits = w%nbits - 8 * (bits / 8)
    end subroutine flush_bytes

    subroutine align(w)
        !! Pad to a byte boundary and write out every whole byte.
        type(writer_t), intent(inout) :: w
        w%nbits = 8 * ((w%nbits + 7) / 8)
        call flush_bytes(w, w%nbits)
    end subroutine align

    subroutine finish_bits(w)
        type(writer_t), intent(inout) :: w
        call align(w)
    end subroutine finish_bits

    pure integer function byte(b)
        integer(int8), intent(in) :: b
        byte = iand(int(b), 255)
    end function byte

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

end module fortzip_deflate
