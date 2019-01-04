with Interfaces.C; use Interfaces.C;
with Lv.Hal.Indev;
with Lv.Objx;
with Lv.Area;

package Lv.Indev is

   procedure Init;  -- lv_indev.h:35
   pragma Import (C, Init, "lv_indev_init");

   function Get_Act return Lv.Hal.Indev.Indev_T;  -- lv_indev.h:41
   pragma Import (C, Get_Act, "lv_indev_get_act");

   function Get_Type
     (Arg1 : Lv.Hal.Indev.Indev_T)
     return Lv.Hal.Indev.Indev_Type_T;  -- lv_indev.h:49
   pragma Import (C, Get_Type, "lv_indev_get_type");

   procedure Reset (Arg1 : Lv.Hal.Indev.Indev_T);  -- lv_indev.h:55
   pragma Import (C, Reset, "lv_indev_reset");

   procedure Reset_Lpr (Arg1 : Lv.Hal.Indev.Indev_T);  -- lv_indev.h:61
   pragma Import (C, Reset_Lpr, "lv_indev_reset_lpr");

--     procedure enable (arg1 : lv_hal_indev_h.lv_hal_indev_type_t; arg2 : u_Bool);  -- lv_indev.h:68
--     pragma Import (C, enable, "lv_indev_enable");

   procedure Set_Cursor (Arg1 : Lv.Hal.Indev.Indev_T; Arg2 : Lv.Objx.Obj_T);
   pragma Import (C, Set_Cursor, "lv_indev_set_cursor");

--     procedure set_group (arg1 : LV.HAL.Indev.Indev_T; arg2 : access lv_group_h.lv_group_t);  -- lv_indev.h:83
--     pragma Import (C, set_group, "lv_indev_set_group");

   procedure Set_Button_Points
     (Arg1 : Lv.Hal.Indev.Indev_T;
      Arg2 : access Lv.Area.Point_T);  -- lv_indev.h:92
   pragma Import (C, Set_Button_Points, "lv_indev_set_button_points");

   procedure Get_Point
     (Arg1 : Lv.Hal.Indev.Indev_T;
      Arg2 : access Lv.Area.Point_T);  -- lv_indev.h:99
   pragma Import (C, Get_Point, "lv_indev_get_point");

   function Get_Key
     (Arg1 : Lv.Hal.Indev.Indev_T) return Uint32_T;  -- lv_indev.h:106
   pragma Import (C, Get_Key, "lv_indev_get_key");

   function Is_Dragging
     (Arg1 : Lv.Hal.Indev.Indev_T) return U_Bool;  -- lv_indev.h:113
   pragma Import (C, Is_Dragging, "lv_indev_is_dragging");

   procedure Get_Vect
     (Arg1 : Lv.Hal.Indev.Indev_T;
      Arg2 : access Lv.Area.Point_T);  -- lv_indev.h:120
   pragma Import (C, Get_Vect, "lv_indev_get_vect");

   function Get_Inactive_Time
     (Arg1 : Lv.Hal.Indev.Indev_T) return Uint32_T;  -- lv_indev.h:126
   pragma Import (C, Get_Inactive_Time, "lv_indev_get_inactive_time");

   procedure Wait_Release (Arg1 : Lv.Hal.Indev.Indev_T);  -- lv_indev.h:132
   pragma Import (C, Wait_Release, "lv_indev_wait_release");

end Lv.Indev;
