program test_fortzip
    !! Against the system zlib on inputs generated here: zlib's streams of every
    !! level decode, ours of every level are decoded by zlib, checksums agree,
    !! gzip members (with every optional header field) decode, and damaged
    !! streams are refused.
    use iso_fortran_env, only: int8, int32
    use fortzip
    use zlib_reference, only: zlib_open, ref_compress, ref_uncompress, ref_crc32, ref_adler32
    implicit none
    integer :: failures, checked, k
    character(*), parameter :: NAMES(6) = [character(8) :: "empty", "byte", "text", &
        "random", "runs", "far"]

    failures = 0
    checked = 0
    if (.not. zlib_open()) error stop "the reference zlib (libz.so.1) is missing"
    call check_known_checksums()
    do k = 1, size(NAMES)
        call check_input(trim(NAMES(k)), make_input(k))
    end do
    call check_gzip_members()
    call check_damage()
    write (*, '(a, i0, a, i0, a)') "fortzip: ", checked, " checks, ", failures, " failures"
    if (failures > 0) error stop 1

contains

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(*), intent(in) :: what
        checked = checked + 1
        if (ok) return
        failures = failures + 1
        write (*, '(a)') "FAIL: " // what
    end subroutine check

    function make_input(which) result(data)
        !! Deterministic inputs: empty, one byte, word salad, random bytes, runs
        !! of few symbols, and random bytes repeated at a distance over 16 KiB.
        integer, intent(in) :: which
        integer(int8), allocatable :: data(:)
        character(*), parameter :: WORDS = "plasma field line flux surface torus " // &
            "stellarator coil current pressure gradient drift orbit particle magnetic "
        integer :: state, n, k, run, v
        state = 20261115
        select case (which)
        case (1)
            allocate (data(0))
        case (2)
            data = [int(65, int8)]
        case (3)
            allocate (data(40000))
            n = 0
            do while (n < size(data))
                k = 1 + mod(next(state), len(WORDS) - 12)
                do while (k > 1)
                    if (WORDS(k - 1:k - 1) == " ") exit
                    k = k - 1
                end do
                do while (k <= len(WORDS) .and. n < size(data))
                    n = n + 1
                    data(n) = int(iachar(WORDS(k:k)), int8)
                    if (WORDS(k:k) == " ") exit
                    k = k + 1
                end do
            end do
        case (4)
            allocate (data(8192))
            do k = 1, size(data)
                data(k) = to_byte(next(state))
            end do
        case (5)
            allocate (data(40000))
            n = 0
            do while (n < size(data))
                v = mod(next(state), 4)
                run = 1 + mod(next(state), 300)
                data(n + 1:min(size(data), n + run)) = int(v, int8)
                n = n + run
            end do
        case default
            allocate (data(2 * 20000))
            do k = 1, 20000
                data(k) = to_byte(next(state))
            end do
            data(20001:) = data(1:20000)
        end select
    end function make_input

    integer function next(state)
        !! A small linear congruential generator (Numerical Recipes constants).
        integer, intent(inout) :: state
        state = int(modulo(1664525_8 * state + 1013904223_8, 2_8**31))
        next = ishft(state, -8)
    end function next

    subroutine check_known_checksums()
        ! Check values of "123456789" from the CRC catalogue.
        integer(int8) :: digits(9)
        integer :: k
        digits = [(int(iachar("123456789"(k:k)), int8), k = 1, 9)]
        ! CRC-32 0xCBF43926 and Adler-32 0x091E01DE, as signed int32.
        call check(crc32(digits) == -873187034_int32, "crc32 check value")
        call check(adler32(digits) == 152961502_int32, "adler32 check value")
        call check(crc32(digits) == ref_crc32(digits), "crc32 of 123456789 as zlib")
        call check(adler32(digits) == ref_adler32(digits), "adler32 of 123456789 as zlib")
        call check(crc32(digits(5:), crc32(digits(:4))) == crc32(digits), "crc32 continued")
        call check(adler32(digits(5:), adler32(digits(:4))) == adler32(digits), &
            "adler32 continued")
    end subroutine check_known_checksums

    subroutine check_input(name, data)
        character(*), intent(in) :: name
        integer(int8), intent(in) :: data(:)
        integer(int8), allocatable :: theirs(:), ours(:), back(:), gz(:), as_zlib(:)
        integer :: level, status, size_theirs9, size_ours9
        call check(crc32(data) == ref_crc32(data), name // ": crc32 as zlib")
        call check(adler32(data) == ref_adler32(data), name // ": adler32 as zlib")
        do level = 0, 9
            theirs = ref_compress(data, level)
            call zlib_decompress(theirs, back, status)
            call check(status == ZIP_OK .and. same(back, data), name // &
                ": zlib's stream decodes, level " // str(level))
            call zlib_compress(data, ours, level)
            call check(ref_uncompress(ours, size(data), back) .and. same(back, data), &
                name // ": zlib decodes our stream, level " // str(level))
            call zlib_decompress(ours, back, status)
            call check(status == ZIP_OK .and. same(back, data), name // &
                ": our stream round trip, level " // str(level))
            if (level == 9) then
                size_theirs9 = size(theirs)
                size_ours9 = size(ours)
            end if
        end do
        ! Our gzip member: the DEFLATE body with a zlib frame is what zlib reads.
        call gzip_compress(data, gz)
        call gzip_decompress(gz, back, status)
        call check(status == ZIP_OK .and. same(back, data), name // ": gzip round trip")
        as_zlib = [int(120, int8), int(-100, int8), gz(11:size(gz) - 8), be32(adler32(data))]
        call check(ref_uncompress(as_zlib, size(data), back) .and. same(back, data), &
            name // ": zlib decodes our gzip body")
        write (*, '(a, t10, a, i7, a, i7, a, i7)') name, "original", size(data), &
            "  zlib -9", size_theirs9, "  fortzip -9", size_ours9
    end subroutine check_input

    subroutine check_gzip_members()
        ! Two members, the first with FEXTRA, FNAME, FCOMMENT and FHCRC.
        integer(int8), allocatable :: a(:), b(:), gz(:), out(:)
        integer :: status
        a = make_input(3)
        b = make_input(5)
        gz = [member(a, .true.), member(b, .false.)]
        call gzip_decompress(gz, out, status)
        call check(status == ZIP_OK .and. same(out, [a, b]), "gzip: two members, all fields")
        call inflate_raw(raw_of(a), out, status)
        call check(status == ZIP_OK .and. same(out, a), "raw DEFLATE from zlib")
    end subroutine check_gzip_members

    function member(data, fields) result(gz)
        integer(int8), intent(in) :: data(:)
        logical, intent(in) :: fields
        integer(int8), allocatable :: gz(:), header(:)
        if (fields) then
            ! FLG = FHCRC | FEXTRA | FNAME | FCOMMENT; the header CRC is not checked.
            header = [int([31, -117, 8, 30, 0, 0, 0, 0, 0, 3, 3, 0], int8), &
                int([1, 2, 3], int8), int([110, 97, 109, 101, 0], int8), &
                int([104, 105, 0], int8), int([0, 0], int8)]
        else
            header = int([31, -117, 8, 0, 0, 0, 0, 0, 0, 3], int8)
        end if
        gz = [header, raw_of(data), le32(crc32(data)), le32(int(size(data), int32))]
    end function member

    function raw_of(data) result(raw)
        !! zlib's level-6 DEFLATE data without the zlib frame.
        integer(int8), intent(in) :: data(:)
        integer(int8), allocatable :: raw(:), z(:)
        z = ref_compress(data, 6)
        raw = z(3:size(z) - 4)
    end function raw_of

    subroutine check_damage()
        integer(int8), allocatable :: good(:), bad(:), out(:)
        integer :: status
        good = ref_compress(make_input(3), 6)
        bad = good
        bad(size(bad)) = int(ieor(int(bad(size(bad))), 1), int8)
        call zlib_decompress(bad, out, status)
        call check(status == ZIP_ERR_CHECKSUM, "zlib: wrong Adler-32 refused")
        call zlib_decompress(good(:size(good) / 2), out, status)
        call check(status == ZIP_ERR_TRUNCATED, "zlib: truncated stream refused")
        bad = good
        bad(2) = int(ieor(int(bad(2)), 1), int8)
        call zlib_decompress(bad, out, status)
        call check(status == ZIP_ERR_HEADER, "zlib: header check bits refused")
        call gzip_compress(make_input(3), good)
        bad = good
        bad(size(bad) - 5) = int(ieor(int(bad(size(bad) - 5)), 1), int8)
        call gzip_decompress(bad, out, status)
        call check(status == ZIP_ERR_CHECKSUM, "gzip: wrong CRC-32 refused")
        bad = good
        bad(1) = 0
        call gzip_decompress(bad, out, status)
        call check(status == ZIP_ERR_HEADER, "gzip: magic refused")
    end subroutine check_damage

    logical function same(a, b)
        integer(int8), intent(in) :: a(:), b(:)
        same = size(a) == size(b)
        if (same) same = all(a == b)
    end function same

    pure function be32(v) result(b)
        integer(int32), intent(in) :: v
        integer(int8) :: b(4)
        b = [to_byte(ibits(v, 24, 8)), to_byte(ibits(v, 16, 8)), to_byte(ibits(v, 8, 8)), &
            to_byte(ibits(v, 0, 8))]
    end function be32

    pure function le32(v) result(b)
        integer(int32), intent(in) :: v
        integer(int8) :: b(4)
        b = [to_byte(ibits(v, 0, 8)), to_byte(ibits(v, 8, 8)), to_byte(ibits(v, 16, 8)), &
            to_byte(ibits(v, 24, 8))]
    end function le32

    pure integer(int8) function to_byte(v)
        integer, intent(in) :: v
        to_byte = int(iand(v, 255) - 256 * (iand(v, 255) / 128), int8)
    end function to_byte

    function str(i) result(s)
        integer, intent(in) :: i
        character(:), allocatable :: s
        character(12) :: buf
        write (buf, '(i0)') i
        s = trim(buf)
    end function str

end program test_fortzip
