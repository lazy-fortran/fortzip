module zlib_reference
    !! The system zlib, opened at run time, as the independent reference of the
    !! tests: wherever git runs, libz is installed.
    use iso_c_binding
    use iso_fortran_env, only: int8, int32, int64
    implicit none
    private
    public :: zlib_open, ref_compress, ref_uncompress, ref_crc32, ref_adler32

    abstract interface
        integer(c_int) function fn_compress2(dest, dest_len, src, src_len, level) bind(C)
            import :: c_int, c_long, c_ptr
            type(c_ptr), value :: dest, src
            integer(c_long), intent(inout) :: dest_len
            integer(c_long), value :: src_len
            integer(c_int), value :: level
        end function
        integer(c_int) function fn_uncompress(dest, dest_len, src, src_len) bind(C)
            import :: c_int, c_long, c_ptr
            type(c_ptr), value :: dest, src
            integer(c_long), intent(inout) :: dest_len
            integer(c_long), value :: src_len
        end function
        integer(c_long) function fn_checksum(start, buf, len) bind(C)
            import :: c_long, c_ptr, c_int
            integer(c_long), value :: start
            type(c_ptr), value :: buf
            integer(c_int), value :: len
        end function
    end interface

    interface
        type(c_ptr) function dlopen(name, flags) bind(C)
            import :: c_ptr, c_char, c_int
            character(kind=c_char), intent(in) :: name(*)
            integer(c_int), value :: flags
        end function
        type(c_funptr) function dlsym(handle, name) bind(C)
            import :: c_ptr, c_funptr, c_char
            type(c_ptr), value :: handle
            character(kind=c_char), intent(in) :: name(*)
        end function
    end interface

    procedure(fn_compress2), pointer :: compress2 => null()
    procedure(fn_uncompress), pointer :: uncompress => null()
    procedure(fn_checksum), pointer :: crc32_c => null(), adler32_c => null()

contains

    logical function zlib_open()
        type(c_ptr) :: lib
        lib = dlopen("libz.so.1" // c_null_char, 2_c_int)
        if (.not. c_associated(lib)) lib = dlopen("libz.dylib" // c_null_char, 2_c_int)
        zlib_open = c_associated(lib)
        if (.not. zlib_open) return
        call c_f_procpointer(dlsym(lib, "compress2" // c_null_char), compress2)
        call c_f_procpointer(dlsym(lib, "uncompress" // c_null_char), uncompress)
        call c_f_procpointer(dlsym(lib, "crc32" // c_null_char), crc32_c)
        call c_f_procpointer(dlsym(lib, "adler32" // c_null_char), adler32_c)
    end function zlib_open

    function ref_compress(src, level) result(out)
        integer(int8), intent(in), target :: src(:)
        integer, intent(in) :: level
        integer(int8), allocatable :: out(:)
        integer(int8), allocatable, target :: buf(:)
        integer(c_long) :: n
        integer(int8), target :: dummy(1)
        n = int(size(src), c_long) + size(src) / 8 + 1024
        allocate (buf(n))
        dummy = 0
        if (compress2(c_loc(buf), n, merge_ptr(src, dummy), int(size(src), c_long), &
            int(level, c_int)) /= 0) error stop "zlib compress2 failed"
        out = buf(1:n)
    end function ref_compress

    logical function ref_uncompress(src, expected_size, out) result(ok)
        integer(int8), intent(in), target :: src(:)
        integer, intent(in) :: expected_size
        integer(int8), allocatable, intent(out) :: out(:)
        integer(int8), allocatable, target :: buf(:)
        integer(c_long) :: n
        n = expected_size + 1
        allocate (buf(n))
        ok = uncompress(c_loc(buf), n, c_loc(src), int(size(src), c_long)) == 0
        out = buf(1:n)
    end function ref_uncompress

    integer(int32) function ref_crc32(data)
        integer(int8), intent(in), target :: data(:)
        integer(int8), target :: dummy(1)
        dummy = 0
        ref_crc32 = low32(crc32_c(0_c_long, merge_ptr(data, dummy), int(size(data), c_int)))
    end function ref_crc32

    integer(int32) function ref_adler32(data)
        integer(int8), intent(in), target :: data(:)
        integer(int8), target :: dummy(1)
        dummy = 0
        ref_adler32 = low32(adler32_c(1_c_long, merge_ptr(data, dummy), int(size(data), c_int)))
    end function ref_adler32

    type(c_ptr) function merge_ptr(data, dummy)
        !! A valid pointer also for an empty array.
        integer(int8), intent(in), target :: data(:), dummy(:)
        if (size(data) > 0) then
            merge_ptr = c_loc(data)
        else
            merge_ptr = c_loc(dummy)
        end if
    end function merge_ptr

    pure integer(int32) function low32(v)
        integer(c_long), intent(in) :: v
        integer(int64) :: w
        w = iand(int(v, int64), int(z'FFFFFFFF', int64))
        if (w >= 2_int64**31) w = w - 2_int64**32
        low32 = int(w, int32)
    end function low32

end module zlib_reference
