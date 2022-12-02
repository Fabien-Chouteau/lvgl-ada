with Lv.Style;

package Lv.Objx.Led is

   subtype Instance is Obj_T;

   subtype Brightness is Uint8_T range 0 .. 255;

   --  Create a led objects
   --  @param par pointer to an object, it will be the parent of the new led
   --  @param copy pointer to a led object, if not NULL then the new object will be copied from it
   --  @return pointer to the created led
   function Create (Parent : Obj_T; Copy : Instance) return Instance;

   ----------------------
   -- Setter functions --
   ----------------------

   --  Set the brightness of a LED object
   --  @param self pointer to a LED object
   --  @param bright 0 (max. dark) ... 255 (max. light)
   procedure Set_Bright (Self : Instance; Bright : Brightness);

   --  Light on a LED
   --  @param self pointer to a LED object
   procedure On (Self : Instance);

   --  Light off a LED
   --  @param self pointer to a LED object
   procedure Off (Self : Instance);

   --  Toggle the state of a LED
   --  @param self pointer to a LED object
   procedure Toggle (Self : Instance);

   --  Set the style of a led
   --  @param self pointer to a led object
   --  @param style pointer to a style
   procedure Set_Style (Self : Instance; Style : Lv.Style.Style);

   ----------------------
   -- Getter functions --
   ----------------------

   --  Get the brightness of a LEd object
   --  @param self pointer to LED object
   --  @return bright 0 (max. dark) ... 255 (max. light)
   function Bright (Self : Instance) return Brightness;

   --  Get the style of an led object
   --  @param self pointer to an led object
   --  @return pointer to the led's style
   function Style (Self : Instance) return Lv.Style.Style;

   -------------
   -- Imports --
   -------------

   pragma Import (C, Create, "lv_led_create");
   pragma Import (C, Set_Bright, "lv_led_set_bright");
   pragma Import (C, On, "lv_led_on");
   pragma Import (C, Off, "lv_led_off");
   pragma Import (C, Toggle, "lv_led_toggle");
   pragma Import (C, Set_Style, "lv_led_set_style_inline");
   pragma Import (C, Bright, "lv_led_get_bright");
   pragma Import (C, Style, "lv_led_get_style_inline");

end Lv.Objx.Led;
