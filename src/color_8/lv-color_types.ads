with Interfaces; use Interfaces;
with Interfaces.C.Extensions;

package Lv.Color_Types is

   subtype Color_Int_T is Uint8_T;

   type Color_T_Comp is record
      Blue  : Extensions.Unsigned_2;
      Green : Extensions.Unsigned_3;
      Red   : Extensions.Unsigned_3;
   end record
     with Pack, Object_Size => 8;
   pragma Convention (C_Pass_By_Copy, Color_T_Comp);

   type Color_T (Discr : unsigned := 0) is record
      case Discr is
         when 0 =>
            Comp : aliased Color_T_Comp;
         when others =>
            Full : aliased Color_Int_T;
      end case;
   end record
     with Pack, Object_Size => 8;
   pragma Convention (C_Pass_By_Copy, Color_T);
   pragma Unchecked_Union (Color_T);

   function Color_Make
     (R8, G8, B8 : Uint8_T) return Color_T is
     (Discr => 0,
      Comp => (Extensions.Unsigned_2 (Shift_Right (B8, 6)),
               Extensions.Unsigned_3 (Shift_Right (G8, 5)),
               Extensions.Unsigned_3 (Shift_Right (R8, 5))))
   with Inline_Always;

end Lv.Color_Types;

