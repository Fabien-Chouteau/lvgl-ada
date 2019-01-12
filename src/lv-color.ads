with LV.Color_Types;

package Lv.Color is

   subtype Color_T is LV.Color_Types.Color_T;
   subtype Color_Int_T is LV.Color_Types.Color_Int_T;

   subtype Opa_T is Uint8_T;

   type Color_Hsv_T is record
      H : aliased Uint16_T;
      S : aliased Uint8_T;
      V : aliased Uint8_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Color_Hsv_T);

   function Color_Make
     (R8, G8, B8 : Uint8_T)
      return Color_T
      renames LV.Color_Types.Color_Make
   with Inline_Always;

   Color_White   : constant Color_T := Color_Make (16#FF#, 16#FF#, 16#FF#);
   Color_Silver  : constant Color_T := Color_Make (16#C0#, 16#C0#, 16#C0#);
   Color_Gray    : constant Color_T := Color_Make (16#80#, 16#80#, 16#80#);
   Color_Black   : constant Color_T := Color_Make (16#00#, 16#00#, 16#00#);
   Color_Red     : constant Color_T := Color_Make (16#FF#, 16#00#, 16#00#);
   Color_Maroon  : constant Color_T := Color_Make (16#80#, 16#00#, 16#00#);
   Color_Yellow  : constant Color_T := Color_Make (16#FF#, 16#FF#, 16#00#);
   Color_Olive   : constant Color_T := Color_Make (16#80#, 16#80#, 16#00#);
   Color_Lime    : constant Color_T := Color_Make (16#00#, 16#FF#, 16#00#);
   Color_Green   : constant Color_T := Color_Make (16#00#, 16#80#, 16#00#);
   Color_Cyan    : constant Color_T := Color_Make (16#00#, 16#FF#, 16#FF#);
   Color_Aqua    : constant Color_T := Color_Cyan;
   Color_Teal    : constant Color_T := Color_Make (16#00#, 16#80#, 16#80#);
   Color_Blue    : constant Color_T := Color_Make (16#00#, 16#00#, 16#FF#);
   Color_Navy    : constant Color_T := Color_Make (16#00#, 16#00#, 16#80#);
   Color_Magenta : constant Color_T := Color_Make (16#FF#, 16#00#, 16#FF#);
   Color_Purple  : constant Color_T := Color_Make (16#80#, 16#00#, 16#80#);
   Color_Orange  : constant Color_T := Color_Make (16#FF#, 16#A5#, 16#00#);
   Opa_Transp    : constant         := 0;
   Opa_0         : constant         := 0;
   Opa_10        : constant         := 25;
   Opa_20        : constant         := 51;
   Opa_30        : constant         := 76;
   Opa_40        : constant         := 102;
   Opa_50        : constant         := 127;
   Opa_60        : constant         := 153;
   Opa_70        : constant         := 178;
   Opa_80        : constant         := 204;
   Opa_90        : constant         := 229;
   Opa_100       : constant         := 255;
   Opa_Cover     : constant         := 255;

   Opa_Min : constant := 16;
   Opa_Max : constant := 251;

   --  In color conversations:
   --   - When converting to bigger color type the LSB weight of 1 LSB is calculated
   --     E.g. 16 bit Red has 5 bits
   --           8 bit Red has 2 bits
   --          ----------------------
   --          8 bit red LSB = (2^5 - 1) / (2^2 - 1) = 31 / 3 = 10
   --
   --   - When calculating to smaller color type simply shift out the LSBs
   --     E.g.  8 bit Red has 2 bits
   --          16 bit Red has 5 bits
   --          ----------------------
   --           Shift right with 5 - 3 = 2
   function Color_To1 (Color : Color_T) return Uint8_T;

   function Color_To8 (Color : Color_T) return Uint8_T;

   function Color_To16 (Color : Color_T) return Uint16_T;

   function Color_To32 (Color : Color_T) return Uint32_T;

   function Color_Mix
     (C1  : Color_T;
      C2  : Color_T;
      Mix : Uint8_T) return Color_T;

   --  Get the brightness of a color
   --  @param color a color
   --  @return the brightness [0..255]
   function Color_Brightness (Color : Color_T) return Uint8_T;

   --  Convert a HSV color to RGB
   --  @param h hue [0..359]
   --  @param s saturation [0..100]
   --  @param v value [0..100]
   --  @return the given RGB color in RGB (with LV_COLOR_DEPTH depth)
   function Color_Hsv_To_Rgb
     (H : Uint16_T;
      S : Uint8_T;
      V : Uint8_T) return Color_T;

   --  Convert an RGB color to HSV
   --  @param r red
   --  @param g green
   --  @param b blue
   --  @return the given RGB color n HSV
   function Color_Rgb_To_Hsv
     (R : Uint8_T;
      G : Uint8_T;
      B : Uint8_T) return Color_Hsv_T;

   -------------
   -- Imports --
   -------------

   pragma Import (C, Color_To1, "lv_color_to1_inline");
   pragma Import (C, Color_To8, "lv_color_to8_inline");
   pragma Import (C, Color_To16, "lv_color_to16_inline");
   pragma Import (C, Color_To32, "lv_color_to32_inline");
   pragma Import (C, Color_Mix, "lv_color_mix_inline");
   pragma Import (C, Color_Brightness, "lv_color_brightness_inline");
   pragma Import (C, Color_Hsv_To_Rgb, "lv_color_hsv_to_rgb");
   pragma Import (C, Color_Rgb_To_Hsv, "lv_color_rgb_to_hsv");

end Lv.Color;
