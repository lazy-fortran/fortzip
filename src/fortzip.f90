module fortzip
    !! DEFLATE (RFC 1951), zlib (RFC 1950) and gzip (RFC 1952) streams and their
    !! checksums, in Fortran only. Data are byte arrays (integer(int8)).
    use iso_fortran_env, only: int8, int32, int64
    use fortzip_checksum, only: crc32, adler32
    use fortzip_inflate, only: inflate_raw, ZIP_OK, ZIP_ERR_DATA, ZIP_ERR_TRUNCATED, &
        ZIP_ERR_CHECKSUM, ZIP_ERR_HEADER
    use fortzip_deflate, only: deflate_raw
    implicit none
    private
    public :: crc32, adler32, inflate_raw, deflate_raw
    public :: zlib_compress, zlib_decompress, gzip_compress, gzip_decompress
    public :: ZIP_OK, ZIP_ERR_DATA, ZIP_ERR_TRUNCATED, ZIP_ERR_CHECKSUM, ZIP_ERR_HEADER

contains

    subroutine zlib_compress(src, out, level)
        !! A zlib stream: header, DEFLATE data, Adler-32 of src (big-endian).
        integer(int8), intent(in) :: src(:)
        integer(int8), allocatable, intent(out) :: out(:)
        integer, intent(in), optional :: level
        integer(int8), allocatable :: body(:)
        integer :: lvl, flevel, cmf, flg
        lvl = 6
        if (present(level)) lvl = max(0, min(9, level))
        call deflate_raw(src, body, lvl)
        ! FLEVEL is informative only; it follows zlib's convention.
        flevel = 2
        if (lvl <= 1) flevel = 0
        if (lvl >= 7) flevel = 3
        if (lvl > 1 .and. lvl < 6) flevel = 1
        cmf = 120
        flg = flevel * 64
        flg = flg + (31 - modulo(cmf * 256 + flg, 31))
        allocate (out(size(body) + 6))
        out(1) = to_byte(cmf)
        out(2) = to_byte(flg)
        out(3:2 + size(body)) = body
        out(3 + size(body):6 + size(body)) = be32(adler32(src))
    end subroutine zlib_compress

    subroutine zlib_decompress(src, out, status)
        !! A zlib stream into out; status ZIP_OK or the first defect found.
        integer(int8), intent(in) :: src(:)
        integer(int8), allocatable, intent(out) :: out(:)
        integer, intent(out) :: status
        integer :: cmf, flg, used
        if (size(src) < 6) then
            allocate (out(0))
            status = ZIP_ERR_TRUNCATED
            return
        end if
        cmf = byte(src(1))
        flg = byte(src(2))
        ! Method 8 (deflate), window at most 32 KiB, check bits, no preset dictionary.
        if (iand(cmf, 15) /= 8 .or. ishft(cmf, -4) > 7 .or. modulo(cmf * 256 + flg, 31) /= 0 &
            .or. iand(flg, 32) /= 0) then
            allocate (out(0))
            status = ZIP_ERR_HEADER
            return
        end if
        call inflate_raw(src(3:), out, status, used)
        if (status /= ZIP_OK) return
        if (2 + used + 4 > size(src)) then
            status = ZIP_ERR_TRUNCATED
            return
        end if
        if (any(src(3 + used:6 + used) /= be32(adler32(out)))) status = ZIP_ERR_CHECKSUM
    end subroutine zlib_decompress

    subroutine gzip_compress(src, out, level)
        !! A single-member gzip file without name or time stamp.
        integer(int8), intent(in) :: src(:)
        integer(int8), allocatable, intent(out) :: out(:)
        integer, intent(in), optional :: level
        integer(int8), allocatable :: body(:)
        integer :: lvl, xfl
        lvl = 6
        if (present(level)) lvl = max(0, min(9, level))
        call deflate_raw(src, body, lvl)
        xfl = 0
        if (lvl == 9) xfl = 2
        if (lvl == 1) xfl = 4
        allocate (out(18 + size(body)))
        ! ID1 ID2 CM FLG MTIME(4) XFL OS (255: unknown)
        out(1:10) = [to_byte(31), to_byte(139), to_byte(8), 0_int8, 0_int8, 0_int8, 0_int8, &
            0_int8, to_byte(xfl), to_byte(255)]
        out(11:10 + size(body)) = body
        out(11 + size(body):14 + size(body)) = le32(crc32(src))
        out(15 + size(body):18 + size(body)) = le32(size_mod32(size(src)))
    end subroutine gzip_compress

    subroutine gzip_decompress(src, out, status)
        !! Every member of a gzip file, concatenated; checks CRC-32 and length.
        integer(int8), intent(in) :: src(:)
        integer(int8), allocatable, intent(out) :: out(:)
        integer, intent(out) :: status
        integer(int8), allocatable :: member(:), joined(:)
        integer :: pos, flg, n, used
        allocate (out(0))
        status = ZIP_OK
        pos = 1
        do
            if (pos + 17 > size(src)) then
                status = ZIP_ERR_TRUNCATED
                return
            end if
            if (byte(src(pos)) /= 31 .or. byte(src(pos + 1)) /= 139 .or. &
                byte(src(pos + 2)) /= 8) then
                status = ZIP_ERR_HEADER
                return
            end if
            flg = byte(src(pos + 3))
            if (iand(flg, 224) /= 0) then
                status = ZIP_ERR_HEADER
                return
            end if
            pos = pos + 10
            if (iand(flg, 4) /= 0) then ! FEXTRA: two-byte length, then the field
                if (pos + 1 > size(src)) then
                    status = ZIP_ERR_TRUNCATED
                    return
                end if
                pos = pos + 2 + byte(src(pos)) + 256 * byte(src(pos + 1))
            end if
            if (iand(flg, 8) /= 0) call skip_string(pos) ! FNAME
            if (iand(flg, 16) /= 0) call skip_string(pos) ! FCOMMENT
            if (iand(flg, 2) /= 0) pos = pos + 2 ! FHCRC
            if (status /= ZIP_OK) return
            if (pos > size(src)) then
                status = ZIP_ERR_TRUNCATED
                return
            end if
            call inflate_raw(src(pos:), member, status, used)
            if (status /= ZIP_OK) return
            pos = pos + used
            if (pos + 7 > size(src)) then
                status = ZIP_ERR_TRUNCATED
                return
            end if
            if (any(src(pos:pos + 3) /= le32(crc32(member)))) then
                status = ZIP_ERR_CHECKSUM
                return
            end if
            if (any(src(pos + 4:pos + 7) /= le32(size_mod32(size(member))))) then
                status = ZIP_ERR_CHECKSUM
                return
            end if
            n = size(out)
            allocate (joined(n + size(member)))
            joined(1:n) = out
            joined(n + 1:) = member
            call move_alloc(joined, out)
            pos = pos + 8
            if (pos > size(src)) exit
        end do
    contains
        subroutine skip_string(p)
            integer, intent(inout) :: p
            do while (p <= size(src))
                if (src(p) == 0) exit
                p = p + 1
            end do
            if (p > size(src)) status = ZIP_ERR_TRUNCATED
            p = p + 1
        end subroutine skip_string
    end subroutine gzip_decompress

    pure integer(int32) function size_mod32(n)
        !! gzip's ISIZE: the length modulo 2**32, as a signed int32.
        integer, intent(in) :: n
        integer(int64) :: w
        w = modulo(int(n, int64), 2_int64**32)
        if (w >= 2_int64**31) w = w - 2_int64**32
        size_mod32 = int(w, int32)
    end function size_mod32

    pure function be32(v) result(b)
        integer(int32), intent(in) :: v
        integer(int8) :: b(4)
        b = [to_byte(int(ibits(v, 24, 8))), to_byte(int(ibits(v, 16, 8))), &
            to_byte(int(ibits(v, 8, 8))), to_byte(int(ibits(v, 0, 8)))]
    end function be32

    pure function le32(v) result(b)
        integer(int32), intent(in) :: v
        integer(int8) :: b(4)
        b = [to_byte(int(ibits(v, 0, 8))), to_byte(int(ibits(v, 8, 8))), &
            to_byte(int(ibits(v, 16, 8))), to_byte(int(ibits(v, 24, 8)))]
    end function le32

    pure integer function byte(b)
        integer(int8), intent(in) :: b
        byte = iand(int(b), 255)
    end function byte

    pure integer(int8) function to_byte(v)
        integer, intent(in) :: v
        to_byte = int(v - 256 * (v / 128), int8)
    end function to_byte

end module fortzip
