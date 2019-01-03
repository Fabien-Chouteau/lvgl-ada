with Interfaces.C; use Interfaces.C;
with LV.Style;

package LV.Objx.LED is

   subtype Instance is Obj_T;

   function create (Parent : Obj_T; Copy : Instance) return Instance;
   pragma Import (C, create, "lv_led_create");

   procedure set_bright (Self : Instance; arg2 : uint8_t);  -- lv_led.h:59
   pragma Import (C, set_bright, "lv_led_set_bright");

   procedure on (Self : Instance);  -- lv_led.h:65
   pragma Import (C, on, "lv_led_on");

   procedure off (Self : Instance);  -- lv_led.h:71
   pragma Import (C, off, "lv_led_off");

   procedure toggle (Self : Instance);  -- lv_led.h:77
   pragma Import (C, toggle, "lv_led_toggle");

   procedure set_style (Self : Instance; style : access LV.Style.Style);  -- lv_led.h:84
   pragma Import (C, set_style, "lv_led_set_style_inline");

   function get_bright (Self : Instance) return uint8_t;  -- lv_led.h:94
   pragma Import (C, get_bright, "lv_led_get_bright");

   function get_style (Self : Instance) return access LV.Style.Style;  -- lv_led.h:101
   pragma Import (C, get_style, "lv_led_get_style_inline");

--  private
--     type lv_led_ext_t is record
--        bright : aliased sys_ustdint_h.uint8_t;  -- lv_led.h:39
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_led_ext_t);  -- lv_led.h:40

end LV.Objx.LED;
