pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions;

package Lv.Color is

   type Color8_T;
   type Color8_T_Anon3468_Struct is record
      Blue  : Extensions.Unsigned_2;
      Green : Extensions.Unsigned_3;
      Red   : Extensions.Unsigned_3;
   end record;
   pragma Convention (C_Pass_By_Copy, Color8_T_Anon3468_Struct);
   type Color8_T (Discr : unsigned := 0) is record
      case Discr is
         when 0 =>
            Anon4136 : aliased Color8_T_Anon3468_Struct;
         when others =>
            Full : aliased Uint8_T;
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, Color8_T);
   pragma Unchecked_Union (Color8_T);

   type Color16_T;
   type Color16_T_Anon3473_Struct is record
      Blue  : Extensions.Unsigned_5;
      Green : Extensions.Unsigned_6;
      Red   : Extensions.Unsigned_5;
   end record;
   pragma Convention (C_Pass_By_Copy, Color16_T_Anon3473_Struct);
   type Color16_T (Discr : unsigned := 0) is record
      case Discr is
         when 0 =>
            Anon4145 : aliased Color16_T_Anon3473_Struct;
         when others =>
            Full : aliased Uint16_T;
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, Color16_T);
   pragma Unchecked_Union (Color16_T);

   type Color32_T;
   type Color32_T_Anon3478_Struct is record
      Blue  : aliased Uint8_T;
      Green : aliased Uint8_T;
      Red   : aliased Uint8_T;
      Alpha : aliased Uint8_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Color32_T_Anon3478_Struct);
   type Color32_T (Discr : unsigned := 0) is record
      case Discr is
         when 0 =>
            Comp : aliased Color32_T_Anon3478_Struct;
         when others =>
            Full : aliased Uint32_T;
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, Color32_T);
   pragma Unchecked_Union (Color32_T);

   subtype Color_Int_T is Uint32_T;

   subtype Color_T is Color32_T;

   subtype Opa_T is Uint8_T;

   type Color_Hsv_T is record
      H : aliased Uint16_T;
      S : aliased Uint8_T;
      V : aliased Uint8_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Color_Hsv_T);

   function Color_Make
     (R8, G8, B8 : Uint8_T) return Color_T is
     (Discr => 0, Comp => (B8, G8, R8, 16#ff#));
   --  arg-macro: procedure COLOR_HEX (c)
   --    COLOR_MAKE(((uint32_t)((uint32_t)c >> 16) and 16#FF#), ((uint32_t)((uint32_t)c >> 8) and 16#FF#), ((uint32_t) c and 16#FF#))
   --  arg-macro: procedure COLOR_HEX3 (c)
   --    COLOR_MAKE((((c >> 4) and 16#F0#) or ((c >> 8) and 16#F#)), ((uint32_t)(c and 16#F0#) or ((c and 16#F0#) >> 4)), ((uint32_t)(c and 16#F#) or ((c and 16#F#) << 4)))

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
   Opa_Transp : constant := 0;
   Opa_0      : constant := 0;
   Opa_10     : constant := 25;
   Opa_20     : constant := 51;
   Opa_30     : constant := 76;
   Opa_40     : constant := 102;
   Opa_50     : constant := 127;
   Opa_60     : constant := 153;
   Opa_70     : constant := 178;
   Opa_80     : constant := 204;
   Opa_90     : constant := 229;
   Opa_100    : constant := 255;
   Opa_Cover  : constant := 255;

   Opa_Min : constant := 16;
   Opa_Max : constant := 251;

   Color_Size : constant := 32;

   function Color_To1 (Color : Color_T) return Uint8_T;
   pragma Import (C, Color_To1, "lv_color_to1_inline");

   function Color_To8 (Color : Color_T) return Uint8_T;
   pragma Import (C, Color_To8, "lv_color_to8_inline");

   function Color_To16 (Color : Color_T) return Uint16_T;
   pragma Import (C, Color_To16, "lv_color_to16_inline");

   function Color_To32 (Color : Color_T) return Uint32_T;
   pragma Import (C, Color_To32, "lv_color_to32_inline");

   function Color_Mix
     (C1  : Color_T;
      C2  : Color_T;
      Mix : Uint8_T) return Color_T;
   pragma Import (C, Color_Mix, "lv_color_mix_inline");

   function Color_Brightness (Color : Color_T) return Uint8_T;
   pragma Import (C, Color_Brightness, "lv_color_brightness_inline");

   function Color_Hsv_To_Rgb
     (Arg1 : Uint16_T;
      Arg2 : Uint8_T;
      Arg3 : Uint8_T) return Color_T;
   pragma Import (C, Color_Hsv_To_Rgb, "lv_color_hsv_to_rgb");

   function Color_Rgb_To_Hsv
     (Arg1 : Uint8_T;
      Arg2 : Uint8_T;
      Arg3 : Uint8_T) return Color_Hsv_T;
   pragma Import (C, Color_Rgb_To_Hsv, "lv_color_rgb_to_hsv");

end Lv.Color;
