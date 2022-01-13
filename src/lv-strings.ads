
package Lv.Strings is

   function New_String (Str : String) return Lv.C_String_Ptr;

   procedure Free (Ptr : in out Lv.C_String_Ptr);

end Lv.Strings;
