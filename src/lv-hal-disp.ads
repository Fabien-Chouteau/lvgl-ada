with Lv.Color;
with Lv.Area;

private with System;

package Lv.Hal.Disp is

   type Color_Array is
     array (0 .. Natural'Last) of aliased Lv.Color.Color_T with
        Convention => C;

   type Disp_Drv_T is record
      Disp_Flush : access procedure
        (X1    : Int32_T;
         Y1    : Int32_T;
         X2    : Int32_T;
         Y2    : Int32_T;
         Color : access constant Color_Array);  -- lv_hal_disp.h:37
      Disp_Fill : access procedure
        (X1    : Int32_T;
         Y1    : Int32_T;
         X2    : Int32_T;
         Y2    : Int32_T;
         Color : Lv.Color.Color_T);  -- lv_hal_disp.h:40
      Disp_Map : access procedure
        (X1    : Int32_T;
         Y1    : Int32_T;
         X2    : Int32_T;
         Y2    : Int32_T;
         Color : access constant Color_Array);  -- lv_hal_disp.h:43
      Mem_Blend : access procedure
        (Dest   : access Color_Array;
         Src    : access constant Color_Array;
         Length : Uint32_T;
         Opa    : Lv.Color.Opa_T);  -- lv_hal_disp.h:48
      Mem_Fill : access procedure
        (Dest   : access Color_Array;
         Length : Uint32_T;
         Color  : Lv.Color.Color_T);  -- lv_hal_disp.h:51
      Vdb_Wr : access procedure
        (Arg1 : access Uint8_T;
         Arg2 : Lv.Area.Coord_T;
         Arg3 : Lv.Area.Coord_T;
         Arg4 : Lv.Area.Coord_T;
         Arg5 : Lv.Color.Color_T;
         Arg6 : Lv.Color.Opa_T);  -- lv_hal_disp.h:56
   end record;
   pragma Convention (C_Pass_By_Copy, Disp_Drv_T);  -- lv_hal_disp.h:35

   type Disp_T is private;

   procedure Init (Arg1 : access Disp_Drv_T);  -- hal_disp.h:75
   pragma Import (C, Init, "lv_disp_drv_init");

   function Register
     (Arg1 : access Disp_Drv_T) return Disp_T;  -- hal_disp.h:83
   pragma Import (C, Register, "lv_disp_drv_register");

   procedure Set_Active (Arg1 : Disp_T);  -- hal_disp.h:89
   pragma Import (C, Set_Active, "lv_disp_set_active");

   function Get_Active return Disp_T;  -- hal_disp.h:95
   pragma Import (C, Get_Active, "lv_disp_get_active");

   function Next (Arg1 : Disp_T) return Disp_T;  -- hal_disp.h:102
   pragma Import (C, Next, "lv_disp_next");

   procedure Flush
     (Arg1 : Int32_T;
      Arg2 : Int32_T;
      Arg3 : Int32_T;
      Arg4 : Int32_T;
      Arg5 : access Lv.Color.Color_T);  -- hal_disp.h:112
   pragma Import (C, Flush, "lv_disp_flush");

   procedure Fill
     (Arg1 : Int32_T;
      Arg2 : Int32_T;
      Arg3 : Int32_T;
      Arg4 : Int32_T;
      Arg5 : Lv.Color.Color_T);  -- hal_disp.h:122
   pragma Import (C, Fill, "lv_disp_fill");

   procedure Map
     (Arg1 : Int32_T;
      Arg2 : Int32_T;
      Arg3 : Int32_T;
      Arg4 : Int32_T;
      Arg5 : access constant Lv.Color.Color_T);  -- hal_disp.h:132
   pragma Import (C, Map, "lv_disp_map");

   procedure Mem_Blend
     (Arg1 : access Lv.Color.Color_T;
      Arg2 : access constant Lv.Color.Color_T;
      Arg3 : Uint32_T;
      Arg4 : Lv.Color.Opa_T);  -- hal_disp.h:143
   pragma Import (C, Mem_Blend, "lv_disp_mem_blend");

   procedure Mem_Fill
     (Arg1 : access Lv.Color.Color_T;
      Arg2 : Uint32_T;
      Arg3 : Lv.Color.Color_T);  -- hal_disp.h:153
   pragma Import (C, Mem_Fill, "lv_disp_mem_fill");

   function Is_Mem_Blend_Supported return U_Bool;  -- hal_disp.h:158
   pragma Import (C, Is_Mem_Blend_Supported, "lv_disp_is_mem_blend_supported");

   function Is_Mem_Fill_Supported return U_Bool;  -- hal_disp.h:164
   pragma Import (C, Is_Mem_Fill_Supported, "lv_disp_is_mem_fill_supported");

private
   type Disp_T is new System.Address;
end Lv.Hal.Disp;
