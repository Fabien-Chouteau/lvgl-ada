with Interfaces; use Interfaces;
with Interfaces.C.Extensions;

package Lv.Color_Types is

   use type Extensions.Unsigned_1;

   subtype Color_Int_T is Uint8_T;

   type Color_T_Comp (Discr : unsigned := 0) is record
      case Discr is
         when 0 =>
            Blue  : Extensions.Unsigned_1;
         when 1 =>
            Green : Extensions.Unsigned_1;
         when others =>
            Red   : Extensions.Unsigned_1;
      end case;
   end record
      with Pack, Object_Size => 8;
   pragma Convention (C_Pass_By_Copy, Color_T_Comp);
   pragma Unchecked_Union (Color_T_Comp);

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
     (Discr => 1,
      Full => (UInt8_T (Shift_Right (B8, 7)) or
               UInt8_T (Shift_Right (G8, 7)) or
               UInt8_T (Shift_Right (R8, 7))))
   with Inline_Always;

end Lv.Color_Types;

