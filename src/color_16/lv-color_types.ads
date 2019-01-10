with Interfaces; use Interfaces;
with Interfaces.C.Extensions;

package Lv.Color_Types is

   type Color_T_Comp is record
      Blue  : Extensions.Unsigned_5;
      Green : Extensions.Unsigned_6;
      Red   : Extensions.Unsigned_5;
   end record
     with Pack, Object_Size => 16;
   pragma Convention (C_Pass_By_Copy, Color_T_Comp);

   subtype Color_Int_T is Uint16_T;

   type Color_T (Discr : unsigned := 0) is record
      case Discr is
         when 0 =>
            Comp : aliased Color_T_Comp;
         when others =>
            Full : aliased Color_Int_T;
      end case;
   end record
     with Pack, Object_Size => 16;
   pragma Convention (C_Pass_By_Copy, Color_T);
   pragma Unchecked_Union (Color_T);

   function Color_Make
     (R8, G8, B8 : Uint8_T) return Color_T is
     (Discr => 0,
      Comp => (Extensions.Unsigned_5 (Shift_Right (B8, 3)),
               Extensions.Unsigned_6 (Shift_Right (G8, 2)),
               Extensions.Unsigned_5 (Shift_Right (R8, 3))))
   with Inline_Always;

end Lv.Color_Types;
