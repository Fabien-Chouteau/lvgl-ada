with Interfaces.C; use Interfaces.C;
with Interfaces;

package Lv is

   subtype Int8_T is Interfaces.Integer_8;
   subtype Uint8_T is Interfaces.Unsigned_8;
   subtype Int16_T is Interfaces.Integer_16;
   subtype Uint16_T is Interfaces.Unsigned_16;
   subtype Int32_T is Interfaces.Integer_32;
   subtype Uint32_T is Interfaces.Unsigned_32;
   subtype Int64_T is Interfaces.Integer_64;
   subtype Uint64_T is Interfaces.Unsigned_64;

   subtype U_Bool is int;

   procedure Init;
   pragma Import (C, Init, "lv_init");
end Lv;
