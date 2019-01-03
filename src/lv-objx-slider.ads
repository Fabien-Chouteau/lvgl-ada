pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Lv.Style;
with Interfaces.C.Extensions;

package Lv.Objx.Slider is

   subtype Instance is Obj_T;

   subtype Lv_Slider_Style_T is Uint8_T;

   function Create (Par : Obj_T; Copy : Obj_T) return Instance;
   pragma Import (C, Create, "lv_slider_create");

   procedure Set_Value (Self : Instance; Value : Int16_T);
   pragma Import (C, Set_Value, "lv_slider_set_value_inline");

   procedure Set_Value_Anim
     (Self      : Instance;
      Value     : Int16_T;
      Anim_Time : Uint16_T);
   pragma Import (C, Set_Value_Anim, "lv_slider_set_value_anim_inline");

   procedure Set_Range (Self : Instance; Min : Int16_T; Max : Int16_T);
   pragma Import (C, Set_Range, "lv_slider_set_range_inline");

   procedure Set_Action (Self : Instance; Arg2 : Lv_Action_T);
   pragma Import (C, Set_Action, "lv_slider_set_action");

   procedure Set_Knob_In (Self : Instance; Arg2 : U_Bool);
   pragma Import (C, Set_Knob_In, "lv_slider_set_knob_in");

   procedure Set_Style
     (Self : Instance;
      Arg2 : Lv_Slider_Style_T;
      Arg3 : access Lv.Style.Style);
   pragma Import (C, Set_Style, "lv_slider_set_style");

   function Get_Value (Self : Instance) return Int16_T;
   pragma Import (C, Get_Value, "lv_slider_get_value");

   function Get_Min_Value (Self : Instance) return Int16_T;
   pragma Import (C, Get_Min_Value, "lv_slider_get_min_value_inline");

   function Get_Max_Value (Self : Instance) return Int16_T;
   pragma Import (C, Get_Max_Value, "lv_slider_get_max_value_inline");

   function Get_Action (Self : Instance) return Lv_Action_T;
   pragma Import (C, Get_Action, "lv_slider_get_action");

   function Is_Dragged (Self : Instance) return U_Bool;
   pragma Import (C, Is_Dragged, "lv_slider_is_dragged");

   function Get_Knob_In (Self : Instance) return U_Bool;
   pragma Import (C, Get_Knob_In, "lv_slider_get_knob_in");

   function Get_Style
     (Self : Instance;
      Arg2 : Lv_Slider_Style_T) return access Lv.Style.Style;
   pragma Import (C, Get_Style, "lv_slider_get_style");

end Lv.Objx.Slider;
