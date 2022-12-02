with Lv.Style;
with System;

package Lv.Objx.Preload is

   subtype Instance is Obj_T;

   type Type_T is (Type_Spinning_Arc);

   type Style_T is (Style_Main);

   --  Create a pre loader objects
   --  @param par pointer to an object, it will be the parent of the new pre loader
   --  @param copy pointer to a pre loader object, if not NULL then the new object will be copied from it
   --  @return pointer to the created pre loader
   function Create (Parent : Obj_T; Copy : Instance) return Instance;

   ----------------------
   -- Setter functions --
   ----------------------

   --  Set the length of the spinning  arc in degrees
   --  @param self pointer to a preload object
   --  @param deg length of the arc
   procedure Set_Arc_Length (Self : Instance; Deg : Uint16_T);

   --  Set the spin time of the arc
   --  @param self pointer to a preload object
   --  @param time time of one round in milliseconds
   procedure Set_Spin_Time (Self : Instance; Time : Uint16_T);

   --  Set a style of a pre loader.
   --  @param self pointer to pre loader object
   --  @param type which style should be set
   --  @param style pointer to a style
   procedure Set_Style
     (Self   : Instance;
      Type_P : Style_T;
      Style  : Lv.Style.Style);

   ----------------------
   -- Getter functions --
   ----------------------

   --  Get the arc length [degree] of the a pre loader
   --  @param self pointer to a pre loader object
   function Get_Arc_Length (Self : Instance) return Uint16_T;

   --  Get the spin time of the arc
   --  @param self pointer to a pre loader object [milliseconds]
   function Get_Spin_Time (Self : Instance) return Uint16_T;

   --  Get style of a pre loader.
   --  @param self pointer to pre loader object
   --  @param type which style should be get
   --  @return style pointer to the style
   function Get_Style
     (Self   : Instance;
      Type_P : Style_T) return Lv.Style.Style;

   ---------------------
   -- Other functions --
   ---------------------

   procedure Spinner_Animation (Arg1 : System.Address; Arg2 : Int32_T);

   -------------
   -- Imports --
   -------------

   pragma Import (C, Create, "lv_preload_create");
   pragma Import (C, Set_Arc_Length, "lv_preload_set_arc_length");
   pragma Import (C, Set_Spin_Time, "lv_preload_set_spin_time");
   pragma Import (C, Set_Style, "lv_preload_set_style");
   pragma Import (C, Get_Arc_Length, "lv_preload_get_arc_length");
   pragma Import (C, Get_Spin_Time, "lv_preload_get_spin_time");
   pragma Import (C, Get_Style, "lv_preload_get_style");
   pragma Import (C, Spinner_Animation, "lv_preload_spinner_animation");

   for Type_T'Size use 8;
   for Type_T use (Type_Spinning_Arc => 0);

   for Style_T'Size use 8;
   for Style_T use (Style_Main => 0);

end Lv.Objx.Preload;
