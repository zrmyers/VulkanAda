with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions;

package stdint_h is
    pragma Preelaborate;
    pragma Pure;
  --*
  -- * This file has no copyright assigned and is placed in the Public Domain.
  -- * This file is part of the mingw-w64 runtime package.
  -- * No warranty is given; refer to the file DISCLAIMER.PD within this package.
  --

  -- ISO C9x  7.18  Integer types <stdint.h>
  -- * Based on ISO/IEC SC22/WG14 9899 Committee draft (SC22 N2794)
  -- *
  -- *  THIS SOFTWARE IS NOT COPYRIGHTED
  -- *
  -- *  Contributor: Danny Smith <danny_r_smith_2001@yahoo.co.nz>
  -- *
  -- *  This source code is offered for use in the public domain. You may
  -- *  use, modify or distribute it freely.
  -- *
  -- *  This code is distributed in the hope that it will be useful but
  -- *  WITHOUT ANY WARRANTY. ALL WARRANTIES, EXPRESS OR IMPLIED ARE HEREBY
  -- *  DISCLAIMED. This includes but is not limited to warranties of
  -- *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  -- *
  -- *  Date: 2000-12-02
  --

  -- 7.18.1.1  Exact-width integer types
   subtype int8_t is signed_char;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/stdint.h:35

   subtype uint8_t is unsigned_char;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/stdint.h:36

   subtype int16_t is short;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/stdint.h:37

   subtype uint16_t is unsigned_short;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/stdint.h:38

   subtype int32_t is int;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/stdint.h:39

   subtype uint32_t is unsigned;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/stdint.h:40

   subtype int64_t is Long_Long_Integer;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/stdint.h:41

   subtype uint64_t is Extensions.unsigned_long_long;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/stdint.h:42

  -- 7.18.1.2  Minimum-width integer types
   subtype int_least8_t is signed_char;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/stdint.h:45

   subtype uint_least8_t is unsigned_char;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/stdint.h:46

   subtype int_least16_t is short;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/stdint.h:47

   subtype uint_least16_t is unsigned_short;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/stdint.h:48

   subtype int_least32_t is int;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/stdint.h:49

   subtype uint_least32_t is unsigned;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/stdint.h:50

   subtype int_least64_t is Long_Long_Integer;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/stdint.h:51

   subtype uint_least64_t is Extensions.unsigned_long_long;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/stdint.h:52

  --  7.18.1.3  Fastest minimum-width integer types
  -- *  Not actually guaranteed to be fastest for all purposes
  -- *  Here we use the exact-width types for 8 and 16-bit ints.
  --

   subtype int_fast8_t is signed_char;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/stdint.h:58

   subtype uint_fast8_t is unsigned_char;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/stdint.h:59

   subtype int_fast16_t is short;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/stdint.h:60

   subtype uint_fast16_t is unsigned_short;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/stdint.h:61

   subtype int_fast32_t is int;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/stdint.h:62

   subtype uint_fast32_t is unsigned;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/stdint.h:63

   subtype int_fast64_t is Long_Long_Integer;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/stdint.h:64

   subtype uint_fast64_t is Extensions.unsigned_long_long;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/stdint.h:65

  -- 7.18.1.5  Greatest-width integer types
   subtype intmax_t is Long_Long_Integer;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/stdint.h:68

   subtype uintmax_t is Extensions.unsigned_long_long;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/stdint.h:69

  -- 7.18.2  Limits of specified-width integer types
  -- 7.18.2.1  Limits of exact-width integer types
  -- 7.18.2.2  Limits of minimum-width integer types
  -- 7.18.2.3  Limits of fastest minimum-width integer types
  -- 7.18.2.4  Limits of integer types capable of holding
  --    object pointers

  -- 7.18.2.5  Limits of greatest-width integer types
  -- 7.18.3  Limits of other integer types
  -- * wint_t is unsigned short for compatibility with MS runtime
  --

  -- 7.18.4  Macros for integer constants
  -- 7.18.4.1  Macros for minimum-width integer constants
  --    Accoding to Douglas Gwyn <gwyn@arl.mil>:
  --	"This spec was changed in ISO/IEC 9899:1999 TC1; in ISO/IEC
  --	9899:1999 as initially published, the expansion was required
  --	to be an integer constant of precisely matching type, which
  --	is impossible to accomplish for the shorter types on most
  --	platforms, because C99 provides no standard way to designate
  --	an integer constant with width less than that of type int.
  --	TC1 changed this to require just an integer constant
  --	*expression* with *promoted* type."
  --	The trick used here is from Clive D W Feather.
  --

  --  The 'trick' doesn't work in C89 for long long because, without
  --    suffix, (val) will be evaluated as int, not intmax_t

  -- 7.18.4.2  Macros for greatest-width integer constants
end stdint_h;
