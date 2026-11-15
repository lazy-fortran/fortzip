# fortzip

DEFLATE (RFC 1951), zlib (RFC 1950) and gzip (RFC 1952) compression and
decompression with CRC-32 and Adler-32, in Fortran only. It builds with
gfortran and LFortran, so the same code runs natively and in WebAssembly.

```fortran
use fortzip
integer(int8), allocatable :: packed(:), back(:)
integer :: status
call zlib_compress(data, packed, level=6)    ! level 0 stores, 1..9 as in zlib
call zlib_decompress(packed, back, status)   ! status == ZIP_OK, or the defect found
```

`gzip_compress`/`gzip_decompress` (all members, CRC-32 and length checked),
`deflate_raw`/`inflate_raw` and `crc32`/`adler32` (with an optional running
value) complete the interface.

The compressor matches with hash chains over the 32 KiB window and writes each
block as stored, fixed or dynamic Huffman, whichever is shortest; its output is
within a fraction of a percent of zlib's at levels 6 and 9. Both directions are
still several times slower than zlib-ng.

## Tests

```sh
fo test        # or: fpm test
```

The tests generate their inputs and use the system zlib (`libz.so.1`, opened
at run time) as the reference: zlib's streams of every level decode, zlib
decodes ours, every input round-trips at every level and as gzip, and damaged
streams are refused. The library itself needs no C library.
