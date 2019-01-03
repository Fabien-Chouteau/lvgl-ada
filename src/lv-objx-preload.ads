with Interfaces.C; use Interfaces.C;
with Lv.Style;
with System;

package Lv.Objx.Preload is

   subtype Instance is Obj_T;

   subtype Lv_Preloader_Type_T is Uint8_T;

   subtype Style_T is Uint8_T;

   function Create (Parent : Obj_T; Copy : Instance) return Instance;
   pragma Import (C, Create, "lv_preload_create");

   procedure Set_Arc_Length (Self : Instance; Arg2 : Uint16_T);
   pragma Import (C, Set_Arc_Length, "lv_preload_set_arc_length");

   procedure Set_Spin_Time (Self : Instance; Arg2 : Uint16_T);
   pragma Import (C, Set_Spin_Time, "lv_preload_set_spin_time");

   procedure Set_Style
     (Self : Instance;
      Arg2 : Style_T;
      Arg3 : access Lv.Style.Style);
   pragma Import (C, Set_Style, "lv_preload_set_style");

   function Get_Arc_Length (Self : Instance) return Uint16_T;
   pragma Import (C, Get_Arc_Length, "lv_preload_get_arc_length");

   function Get_Spin_Time (Self : Instance) return Uint16_T;
   pragma Import (C, Get_Spin_Time, "lv_preload_get_spin_time");

   function Get_Style
     (Self : Instance;
      Arg2 : Style_T) return access Lv.Style.Style;
   pragma Import (C, Get_Style, "lv_preload_get_style");

   procedure Spinner_Animation (Arg1 : System.Address; Arg2 : Int32_T);
   pragma Import (C, Spinner_Animation, "lv_preload_spinner_animation");

end Lv.Objx.Preload;
