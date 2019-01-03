with Interfaces.C; use Interfaces.C;
with Lv.Style;

package Lv.Objx.Led is

   subtype Instance is Obj_T;

   function Create (Parent : Obj_T; Copy : Instance) return Instance;
   pragma Import (C, Create, "lv_led_create");

   procedure Set_Bright (Self : Instance; Arg2 : Uint8_T);
   pragma Import (C, Set_Bright, "lv_led_set_bright");

   procedure On (Self : Instance);
   pragma Import (C, On, "lv_led_on");

   procedure Off (Self : Instance);
   pragma Import (C, Off, "lv_led_off");

   procedure Toggle (Self : Instance);
   pragma Import (C, Toggle, "lv_led_toggle");

   procedure Set_Style (Self : Instance; Style : access Lv.Style.Style);
   pragma Import (C, Set_Style, "lv_led_set_style_inline");

   function Get_Bright (Self : Instance) return Uint8_T;
   pragma Import (C, Get_Bright, "lv_led_get_bright");

   function Get_Style (Self : Instance) return access Lv.Style.Style;
   pragma Import (C, Get_Style, "lv_led_get_style_inline");

end Lv.Objx.Led;
