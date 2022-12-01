
package Lv.Strings is

   function New_String (Str : String) return Lv.C_String_Ptr;

   procedure Free (Ptr : in out Lv.C_String_Ptr);

   function To_String_Array_Ptr (Arr : access constant String_Array)
                                 return String_Array_Ptr;

end Lv.Strings;
