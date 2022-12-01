with System;
with Lv.Mem;

package body Lv.Strings is

   ----------------
   -- New_String --
   ----------------

   function New_String (Str : String) return Lv.C_String_Ptr is
      use System;
      use Interfaces;

      Addr : constant System.Address := Lv.Mem.Alloc (Str'Length + 1);

      Out_Str : String (1 .. Str'Length + 1) with Address => Addr;
   begin
      if Addr = System.Null_Address then
         return System.Null_Address;
      else
         Out_Str (1 .. Str'Length) := Str;
         Out_Str (Out_Str'Last) := ASCII.NUL;
         return Addr;
      end if;
   end New_String;

   ----------
   -- Free --
   ----------

   procedure Free (Ptr : in out Lv.C_String_Ptr) is
   begin
      Lv.Mem.Free (Ptr);
      Ptr := System.Null_Address;
   end Free;

   -------------------------
   -- To_String_Array_Ptr --
   -------------------------

   function To_String_Array_Ptr (Arr : access constant String_Array)
                                 return String_Array_Ptr
   is
   begin
      if Arr'Length = 0 then
         return String_Array_Ptr (System.Null_Address);
      else
         return String_Array_Ptr (Arr (Arr'First)'Address);
      end if;
   end To_String_Array_Ptr;

end Lv.Strings;
