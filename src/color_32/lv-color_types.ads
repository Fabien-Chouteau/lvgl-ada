with System; use System;

package Lv.Color_Types is

   type Color_T_Comp is record
      Blue  : aliased Uint8_T;
      Green : aliased Uint8_T;
      Red   : aliased Uint8_T;
      Alpha : aliased Uint8_T;
   end record
   with Pack, Object_Size => 32;
   pragma Convention (C_Pass_By_Copy, Color_T_Comp);

   subtype Color_Int_T is Uint32_T;

   type Color_T (Discr : unsigned := 0) is record
      case Discr is
         when 0 =>
            Comp : aliased Color_T_Comp;
         when others =>
            Full : aliased Color_Int_T;
      end case;
   end record
     with Pack, Object_Size => 32;
   pragma Convention (C_Pass_By_Copy, Color_T);
   pragma Unchecked_Union (Color_T);

   function Color_Make
     (R8, G8, B8 : Uint8_T) return Color_T
   is (if System.Default_Bit_Order = System.Low_Order_First
       then (Discr => 0, Comp => (R8, G8, B8, 16#ff#))
       else (Discr => 0, Comp => (B8, G8, R8, 16#ff#)))
   with Inline_Always;

end Lv.Color_Types;
