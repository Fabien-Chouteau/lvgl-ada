with Interfaces.C; use Interfaces.C;
with Interfaces;

package LV is

   subtype int8_t is Interfaces.Integer_8;
   subtype uint8_t is Interfaces.Unsigned_8;
   subtype int16_t is Interfaces.Integer_16;
   subtype uint16_t is Interfaces.Unsigned_16;
   subtype int32_t is Interfaces.Integer_32;
   subtype uint32_t is Interfaces.Unsigned_32;
   subtype int64_t is Interfaces.Integer_64;
   subtype uint64_t is Interfaces.Unsigned_64;

--     subtype int_least8_t is signed_char;
--     subtype uint_least8_t is unsigned_char;
--     subtype int_least16_t is short;
--     subtype uint_least16_t is unsigned_short;
--     subtype int_least32_t is long;
--     subtype uint_least32_t is unsigned_long;
--     subtype int_least64_t is Long_Long_Integer;
--     subtype uint_least64_t is Extensions.unsigned_long_long;
   subtype intptr_t is int;
   subtype uintptr_t is unsigned;

   subtype u_Bool is int;

   procedure Init;  -- lv_obj.h:219
   pragma Import (C, Init, "lv_init");

end LV;
