with Lv.Color;
with Lv.Area;

private with System;

package Lv.Hal.Disp is

   type Color_Array is
     array (0 .. Natural'Last) of aliased Lv.Color.Color_T with
        Convention => C;

   type Disp_Drv_T is record
      --  Write the internal buffer (VDB) to the display. 'lv_flush_ready()' has
      --  to be called when finished.
      Disp_Flush : access procedure
        (X1    : Int32_T;
         Y1    : Int32_T;
         X2    : Int32_T;
         Y2    : Int32_T;
         Color : access constant Color_Array);

      --  Fill an area with a color on the display
      Disp_Fill : access procedure
        (X1    : Int32_T;
         Y1    : Int32_T;
         X2    : Int32_T;
         Y2    : Int32_T;
         Color : Lv.Color.Color_T);

      --  Write pixel map (e.g. image) to the display
      Disp_Map : access procedure
        (X1    : Int32_T;
         Y1    : Int32_T;
         X2    : Int32_T;
         Y2    : Int32_T;
         Color : access constant Color_Array);

      --  Optional interface functions to use GPU
      Mem_Blend : access procedure
        (Dest   : access Color_Array;
         Src    : access constant Color_Array;
         Length : Uint32_T;
         Opa    : Lv.Color.Opa_T);

      --  Fill a memory with a color (GPU only)
      Mem_Fill : access procedure
        (Dest   : access Color_Array;
         Length : Uint32_T;
         Color  : Lv.Color.Color_T);

      --  Optional: Set a pixel in a buffer according to the requirements of the
      --  display.
      Vdb_Wr : access procedure
        (Arg1 : access Uint8_T;
         Arg2 : Lv.Area.Coord_T;
         Arg3 : Lv.Area.Coord_T;
         Arg4 : Lv.Area.Coord_T;
         Arg5 : Lv.Color.Color_T;
         Arg6 : Lv.Color.Opa_T);
   end record;
   pragma Convention (C_Pass_By_Copy, Disp_Drv_T);

   type Disp_T is private;

   --  Initialize a display driver with default values.
   --  It is used to surly have known values in the fields ant not memory junk.
   --  After it you can set the fields.
   --  @param driver pointer to driver variable to initialize
   procedure Init_Drv (Driver : access Disp_Drv_T);

   --  Register an initialized display driver.
   --  Automatically set the first display as active.
   --  @param driver pointer to an initialized 'lv_disp_drv_t' variable (can be local variable)
   --  @return pointer to the new display or NULL on error
   function Register
     (Driver : access Disp_Drv_T) return Disp_T;

   --  Set the active display
   --  @param disp pointer to a display (return value of 'lv_disp_register')
   procedure Set_Active (Disp : Disp_T);

   --  Get a pointer to the active display
   --  @return pointer to the active display
   function Get_Active return Disp_T;

   --  Get the next display.
   --  @param disp pointer to the current display. NULL to initialize.
   --  @return the next display or NULL if no more. Give the first display when the parameter is NULL
   function Next (Disp : Disp_T) return Disp_T;

   --  Fill a rectangular area with a color on the active display
   --  @param x1 left coordinate of the rectangle
   --  @param x2 right coordinate of the rectangle
   --  @param y1 top coordinate of the rectangle
   --  @param y2 bottom coordinate of the rectangle
   --  @param color_p pointer to an array of colors
   procedure Flush
     (X1    : Int32_T;
      X2    : Int32_T;
      Y1    : Int32_T;
      Y2    : Int32_T;
      Color : access Color_Array);

   --  Fill a rectangular area with a color on the active display
   --  @param x1 left coordinate of the rectangle
   --  @param x2 right coordinate of the rectangle
   --  @param y1 top coordinate of the rectangle
   --  @param y2 bottom coordinate of the rectangle
   --  @param color fill color
   procedure Fill
     (X1    : Int32_T;
      X2    : Int32_T;
      Y1    : Int32_T;
      Y2    : Int32_T;
      Color : Lv.Color.Color_T);

   --  Put a color map to a rectangular area on the active display
   --  @param x1 left coordinate of the rectangle
   --  @param x2 right coordinate of the rectangle
   --  @param y1 top coordinate of the rectangle
   --  @param y2 bottom coordinate of the rectangle
   --  @param color_map pointer to an array of colors
   procedure Map
     (X1        : Int32_T;
      X2        : Int32_T;
      Y1        : Int32_T;
      Y2        : Int32_T;
      Color_Map : access constant Color_Array);

   --  Blend pixels to a destination memory from a source memory
   --  In 'lv_disp_drv_t' 'mem_blend' is optional. (NULL if not available)
   --  @param dest a memory address. Blend 'src' here.
   --  @param src pointer to pixel map. Blend it to 'dest'.
   --  @param length number of pixels in 'src'
   --  @param opa opacity (0, LV_OPA_TRANSP: transparent ... 255, LV_OPA_COVER, fully cover)
   procedure Mem_Blend
     (Dest   : access Color_Array;
      Src    : access constant Color_Array;
      Length : Uint32_T;
      Opa    : Lv.Color.Opa_T);

   --  Fill a memory with a color (GPUs may support it)
   --  In 'lv_disp_drv_t' 'mem_fill' is optional. (NULL if not available)
   --  @param dest a memory address. Copy 'src' here.
   --  @param src pointer to pixel map. Copy it to 'dest'.
   --  @param length number of pixels in 'src'
   --  @param opa opacity (0, LV_OPA_TRANSP: transparent ... 255, LV_OPA_COVER, fully cover)
   procedure Mem_Fill
     (Src    : access Color_Array;
      Length : Uint32_T;
      Opa    : Lv.Color.Color_T);

   --  Shows if memory blending (by GPU) is supported or not
   --  @return false: 'mem_blend' is not supported in the driver; true: 'mem_blend' is supported in the driver
   function Is_Mem_Blend_Supported return U_Bool;

   --  Shows if memory fill (by GPU) is supported or not
   --  @return false: 'mem_fill' is not supported in the drover; true: 'mem_fill' is supported in the driver
   function Is_Mem_Fill_Supported return U_Bool;

private

   type Disp_T is new System.Address;

   -------------
   -- Imports --
   -------------

   pragma Import (C, Init_Drv, "lv_disp_drv_init");
   pragma Import (C, Register, "lv_disp_drv_register");
   pragma Import (C, Set_Active, "lv_disp_set_active");
   pragma Import (C, Get_Active, "lv_disp_get_active");
   pragma Import (C, Next, "lv_disp_next");
   pragma Import (C, Flush, "lv_disp_flush");
   pragma Import (C, Fill, "lv_disp_fill");
   pragma Import (C, Map, "lv_disp_map");
   pragma Import (C, Mem_Blend, "lv_disp_mem_blend");
   pragma Import (C, Mem_Fill, "lv_disp_mem_fill");
   pragma Import (C, Is_Mem_Blend_Supported, "lv_disp_is_mem_blend_supported");
   pragma Import (C, Is_Mem_Fill_Supported, "lv_disp_is_mem_fill_supported");


end Lv.Hal.Disp;
