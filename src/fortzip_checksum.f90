module fortzip_checksum
    !! CRC-32 (ISO 3309, as in gzip and PNG) and Adler-32 (RFC 1950).
    use iso_fortran_env, only: int8, int32, int64
    implicit none
    private
    public :: crc32, adler32

    integer(int64), save :: crc_table(0:255) = 0
    logical, save :: crc_ready = .false.

contains

    integer(int32) function crc32(data, previous) result(crc)
        !! CRC-32 of data, continuing from `previous` (the CRC of the bytes before).
        integer(int8), intent(in) :: data(:)
        integer(int32), intent(in), optional :: previous
        integer(int64) :: c
        integer :: k
        if (.not. crc_ready) call make_table()
        c = 0
        if (present(previous)) c = iand(int(previous, int64), int(z'FFFFFFFF', int64))
        c = ieor(c, int(z'FFFFFFFF', int64))
        do k = 1, size(data)
            c = ieor(crc_table(iand(ieor(c, int(data(k), int64)), 255_int64)), ishft(c, -8))
        end do
        crc = to_int32(ieor(c, int(z'FFFFFFFF', int64)))
    end function crc32

    integer(int32) function adler32(data, previous) result(adler)
        !! Adler-32 of data, continuing from `previous` (1 for an empty prefix).
        integer(int8), intent(in) :: data(:)
        integer(int32), intent(in), optional :: previous
        integer, parameter :: BASE = 65521, NMAX = 5552
        integer(int64) :: a, b, start
        integer :: k, last
        start = 1
        if (present(previous)) start = iand(int(previous, int64), int(z'FFFFFFFF', int64))
        a = iand(start, 65535_int64)
        b = ishft(start, -16)
        k = 1
        ! Sums stay below 2**63 for NMAX bytes before the modulo is needed.
        do while (k <= size(data))
            last = min(size(data), k + NMAX - 1)
            do while (k <= last)
                a = a + iand(int(data(k), int64), 255_int64)
                b = b + a
                k = k + 1
            end do
            a = modulo(a, int(BASE, int64))
            b = modulo(b, int(BASE, int64))
        end do
        adler = to_int32(ior(ishft(b, 16), a))
    end function adler32

    subroutine make_table()
        integer(int64) :: c
        integer :: n, k
        do n = 0, 255
            c = n
            do k = 1, 8
                if (iand(c, 1_int64) /= 0) then
                    c = ieor(int(z'EDB88320', int64), ishft(c, -1))
                else
                    c = ishft(c, -1)
                end if
            end do
            crc_table(n) = c
        end do
        crc_ready = .true.
    end subroutine make_table

    pure integer(int32) function to_int32(v)
        !! The low 32 bits of v as a signed int32 (two's complement).
        integer(int64), intent(in) :: v
        integer(int64) :: w
        w = iand(v, int(z'FFFFFFFF', int64))
        if (w >= 2_int64**31) w = w - 2_int64**32
        to_int32 = int(w, int32)
    end function to_int32

end module fortzip_checksum
