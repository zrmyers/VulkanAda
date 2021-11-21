with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions;
with System;
with Interfaces.C.Strings;

package crtdefs_h is
    pragma Preelaborate;

  --*
  -- * This file has no copyright assigned and is placed in the Public Domain.
  -- * This file is part of the mingw-w64 runtime package.
  -- * No warranty is given; refer to the file DISCLAIMER.PD within this package.
  --

  -- #define __ERRCODE_DEFINED_MS
   subtype size_t is Extensions.unsigned_long_long;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/crtdefs.h:35

   subtype ssize_t is Long_Long_Integer;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/crtdefs.h:45

   subtype rsize_t is size_t;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/crtdefs.h:52

   subtype intptr_t is Long_Long_Integer;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/crtdefs.h:62

   subtype uintptr_t is Extensions.unsigned_long_long;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/crtdefs.h:75

   subtype ptrdiff_t is Long_Long_Integer;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/crtdefs.h:88

   subtype wint_t is unsigned_short;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/crtdefs.h:106

   subtype wctype_t is unsigned_short;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/crtdefs.h:107

   subtype errno_t is int;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/crtdefs.h:113

   subtype uu_time32_t is long;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/crtdefs.h:118

   subtype uu_time64_t is Long_Long_Integer;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/crtdefs.h:123

   subtype time_t is uu_time64_t;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/crtdefs.h:138

   --  skipped empty struct threadmbcinfostruct

   type pthreadlocinfo is new System.Address;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/crtdefs.h:424

   type pthreadmbcinfo is new System.Address;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/crtdefs.h:425

   --  skipped empty struct uu_lc_time_data

   type localeinfo_struct is record
      locinfo : pthreadlocinfo;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/crtdefs.h:429
      mbcinfo : pthreadmbcinfo;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/crtdefs.h:430
   end record;
   pragma Convention (C_Pass_By_Copy, localeinfo_struct);  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/crtdefs.h:428

   subtype u_locale_tstruct is localeinfo_struct;

   type u_locale_t is access all localeinfo_struct;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/crtdefs.h:431

   type tagLC_ID is record
      wLanguage : aliased unsigned_short;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/crtdefs.h:436
      wCountry : aliased unsigned_short;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/crtdefs.h:437
      wCodePage : aliased unsigned_short;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/crtdefs.h:438
   end record;
   pragma Convention (C_Pass_By_Copy, tagLC_ID);  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/crtdefs.h:435

   subtype LC_ID is tagLC_ID;

   type LPLC_ID is access all tagLC_ID;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/crtdefs.h:439

   type threadlocaleinfostruct_lc_handle_array is array (0 .. 5) of aliased unsigned_long;
   type threadlocaleinfostruct_lc_id_array is array (0 .. 5) of aliased LC_ID;

   type anon_0 is record
      locale : Interfaces.C.Strings.chars_ptr;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/crtdefs.h:451
      wlocale : access wchar_t;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/crtdefs.h:452
      refcount : access int;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/crtdefs.h:453
      wrefcount : access int;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/crtdefs.h:454
   end record;
   type threadlocaleinfostruct_lc_category_array is array (0 .. 5) of aliased anon_0;
   type threadlocaleinfostruct;
   pragma Convention (C_Pass_By_Copy, anon_0);
   type threadlocaleinfostruct is record
      refcount : aliased int;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/crtdefs.h:445
      lc_codepage : aliased unsigned;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/crtdefs.h:446
      lc_collate_cp : aliased unsigned;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/crtdefs.h:447
      lc_handle : aliased threadlocaleinfostruct_lc_handle_array;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/crtdefs.h:448
      lc_id : aliased threadlocaleinfostruct_lc_id_array;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/crtdefs.h:449
      lc_category : aliased threadlocaleinfostruct_lc_category_array;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/crtdefs.h:455
      lc_clike : aliased int;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/crtdefs.h:456
      mb_cur_max : aliased int;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/crtdefs.h:457
      lconv_intl_refcount : access int;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/crtdefs.h:458
      lconv_num_refcount : access int;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/crtdefs.h:459
      lconv_mon_refcount : access int;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/crtdefs.h:460
      the_lconv : System.Address;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/crtdefs.h:461
      ctype1_refcount : access int;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/crtdefs.h:462
      ctype1 : access unsigned_short;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/crtdefs.h:463
      pctype : access unsigned_short;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/crtdefs.h:464
      pclmap : access unsigned_char;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/crtdefs.h:465
      pcumap : access unsigned_char;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/crtdefs.h:466
      lc_time_curr : System.Address;  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/crtdefs.h:467
   end record;
   pragma Convention (C_Pass_By_Copy, threadlocaleinfostruct);  -- C:/Program Files/mingw-w64/x86_64-5.3.0-posix-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/include/crtdefs.h:444

   --  skipped empty struct lconv

   subtype threadlocinfo is threadlocaleinfostruct;

end crtdefs_h;
