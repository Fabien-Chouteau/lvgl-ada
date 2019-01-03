pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions;

package LV.Color is

   type lv_color8_t;
   type lv_color8_t_anon3468_struct is record
      blue : Extensions.Unsigned_2;  -- ../lv_misc/lv_color.h:105
      green : Extensions.Unsigned_3;  -- ../lv_misc/lv_color.h:106
      red : Extensions.Unsigned_3;  -- ../lv_misc/lv_color.h:107
   end record;
   pragma Convention (C_Pass_By_Copy, lv_color8_t_anon3468_struct);
   type lv_color8_t (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            anon4136 : aliased lv_color8_t_anon3468_struct;  -- ../lv_misc/lv_color.h:108
         when others =>
            full : aliased uint8_t;  -- ../lv_misc/lv_color.h:109
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, lv_color8_t);
   pragma Unchecked_Union (lv_color8_t);  -- ../lv_misc/lv_color.h:110

   type lv_color16_t;
   type lv_color16_t_anon3473_struct is record
      blue : Extensions.Unsigned_5;  -- ../lv_misc/lv_color.h:117
      green : Extensions.Unsigned_6;  -- ../lv_misc/lv_color.h:118
      red : Extensions.Unsigned_5;  -- ../lv_misc/lv_color.h:119
   end record;
   pragma Convention (C_Pass_By_Copy, lv_color16_t_anon3473_struct);
   type lv_color16_t (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            anon4145 : aliased lv_color16_t_anon3473_struct;  -- ../lv_misc/lv_color.h:126
         when others =>
            full : aliased uint16_t;  -- ../lv_misc/lv_color.h:127
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, lv_color16_t);
   pragma Unchecked_Union (lv_color16_t);  -- ../lv_misc/lv_color.h:128

   type lv_color32_t;
   type lv_color32_t_anon3478_struct is record
      blue : aliased uint8_t;  -- ../lv_misc/lv_color.h:134
      green : aliased uint8_t;  -- ../lv_misc/lv_color.h:135
      red : aliased uint8_t;  -- ../lv_misc/lv_color.h:136
      alpha : aliased uint8_t;  -- ../lv_misc/lv_color.h:137
   end record;
   pragma Convention (C_Pass_By_Copy, lv_color32_t_anon3478_struct);
   type lv_color32_t (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            comp : aliased lv_color32_t_anon3478_struct;  -- ../lv_misc/lv_color.h:138
         when others =>
            full : aliased uint32_t;  -- ../lv_misc/lv_color.h:139
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, lv_color32_t);
   pragma Unchecked_Union (lv_color32_t);  -- ../lv_misc/lv_color.h:140

   subtype lv_color_int_t is uint32_t;  -- ../lv_misc/lv_color.h:152

   subtype lv_color_t is lv_color32_t;  -- ../lv_misc/lv_color.h:153

   subtype lv_opa_t is uint8_t;  -- ../lv_misc/lv_color.h:158

   type lv_color_hsv_t is record
      h : aliased uint16_t;  -- ../lv_misc/lv_color.h:162
      s : aliased uint8_t;  -- ../lv_misc/lv_color.h:163
      v : aliased uint8_t;  -- ../lv_misc/lv_color.h:164
   end record;
   pragma Convention (C_Pass_By_Copy, lv_color_hsv_t);  -- ../lv_misc/lv_color.h:165

   function LV_COLOR_MAKE (r8, g8, b8 : uint8_t) return lv_color_t
   is (discr => 0, comp => (b8, g8, r8, 16#ff#));
   --  arg-macro: procedure LV_COLOR_HEX (c)
   --    LV_COLOR_MAKE(((uint32_t)((uint32_t)c >> 16) and 16#FF#), ((uint32_t)((uint32_t)c >> 8) and 16#FF#), ((uint32_t) c and 16#FF#))
   --  arg-macro: procedure LV_COLOR_HEX3 (c)
   --    LV_COLOR_MAKE((((c >> 4) and 16#F0#) or ((c >> 8) and 16#F#)), ((uint32_t)(c and 16#F0#) or ((c and 16#F0#) >> 4)), ((uint32_t)(c and 16#F#) or ((c and 16#F#) << 4)))

   LV_COLOR_WHITE   : constant lv_color_t := LV_COLOR_MAKE (16#FF#,16#FF#,16#FF#);
   LV_COLOR_SILVER  : constant lv_color_t := LV_COLOR_MAKE (16#C0#,16#C0#,16#C0#);
   LV_COLOR_GRAY    : constant lv_color_t := LV_COLOR_MAKE (16#80#,16#80#,16#80#);
   LV_COLOR_BLACK   : constant lv_color_t := LV_COLOR_MAKE (16#00#,16#00#,16#00#);
   LV_COLOR_RED     : constant lv_color_t := LV_COLOR_MAKE (16#FF#,16#00#,16#00#);
   LV_COLOR_MAROON  : constant lv_color_t := LV_COLOR_MAKE (16#80#,16#00#,16#00#);
   LV_COLOR_YELLOW  : constant lv_color_t := LV_COLOR_MAKE (16#FF#,16#FF#,16#00#);
   LV_COLOR_OLIVE   : constant lv_color_t := LV_COLOR_MAKE (16#80#,16#80#,16#00#);
   LV_COLOR_LIME    : constant lv_color_t := LV_COLOR_MAKE (16#00#,16#FF#,16#00#);
   LV_COLOR_GREEN   : constant lv_color_t := LV_COLOR_MAKE (16#00#,16#80#,16#00#);
   LV_COLOR_CYAN    : constant lv_color_t := LV_COLOR_MAKE (16#00#,16#FF#,16#FF#);
   LV_COLOR_AQUA    : constant lv_color_t := LV_COLOR_CYAN;
   LV_COLOR_TEAL    : constant lv_color_t := LV_COLOR_MAKE (16#00#,16#80#,16#80#);
   LV_COLOR_BLUE    : constant lv_color_t := LV_COLOR_MAKE (16#00#,16#00#,16#FF#);
   LV_COLOR_NAVY    : constant lv_color_t := LV_COLOR_MAKE (16#00#,16#00#,16#80#);
   LV_COLOR_MAGENTA : constant lv_color_t := LV_COLOR_MAKE (16#FF#,16#00#,16#FF#);
   LV_COLOR_PURPLE  : constant lv_color_t := LV_COLOR_MAKE (16#80#,16#00#,16#80#);
   LV_COLOR_ORANGE  : constant lv_color_t := LV_COLOR_MAKE (16#FF#,16#A5#,16#00#);
   LV_OPA_TRANSP : constant := 0;  --  ../lv_misc/lv_color.h:60
   LV_OPA_0 : constant := 0;  --  ../lv_misc/lv_color.h:61
   LV_OPA_10 : constant := 25;  --  ../lv_misc/lv_color.h:62
   LV_OPA_20 : constant := 51;  --  ../lv_misc/lv_color.h:63
   LV_OPA_30 : constant := 76;  --  ../lv_misc/lv_color.h:64
   LV_OPA_40 : constant := 102;  --  ../lv_misc/lv_color.h:65
   LV_OPA_50 : constant := 127;  --  ../lv_misc/lv_color.h:66
   LV_OPA_60 : constant := 153;  --  ../lv_misc/lv_color.h:67
   LV_OPA_70 : constant := 178;  --  ../lv_misc/lv_color.h:68
   LV_OPA_80 : constant := 204;  --  ../lv_misc/lv_color.h:69
   LV_OPA_90 : constant := 229;  --  ../lv_misc/lv_color.h:70
   LV_OPA_100 : constant := 255;  --  ../lv_misc/lv_color.h:71
   LV_OPA_COVER : constant := 255;  --  ../lv_misc/lv_color.h:72

   LV_OPA_MIN : constant := 16;  --  ../lv_misc/lv_color.h:74
   LV_OPA_MAX : constant := 251;  --  ../lv_misc/lv_color.h:75

   LV_COLOR_SIZE : constant := 32;  --  ../lv_misc/lv_color.h:84

   function lv_color_to1 (color : lv_color_t) return uint8_t;  -- ../lv_misc/lv_color.h:185
   pragma Import (C, lv_color_to1, "lv_color_to1_inline");

   function lv_color_to8 (color : lv_color_t) return uint8_t;  -- ../lv_misc/lv_color.h:223
   pragma Import (C, lv_color_to8, "lv_color_to8_inline");

   function lv_color_to16 (color : lv_color_t) return uint16_t;  -- ../lv_misc/lv_color.h:254
   pragma Import (C, lv_color_to16, "lv_color_to16_inline");

   function lv_color_to32 (color : lv_color_t) return uint32_t;  -- ../lv_misc/lv_color.h:291
   pragma Import (C, lv_color_to32, "lv_color_to32_inline");

   function lv_color_mix
     (c1 : lv_color_t;
      c2 : lv_color_t;
      mix : uint8_t) return lv_color_t;  -- ../lv_misc/lv_color.h:324
   pragma Import (C, lv_color_mix, "lv_color_mix_inline");

   function lv_color_brightness (color : lv_color_t) return uint8_t;  -- ../lv_misc/lv_color.h:357
   pragma Import (C, lv_color_brightness, "lv_color_brightness_inline");

   function lv_color_hsv_to_rgb
     (arg1 : uint16_t;
      arg2 : uint8_t;
      arg3 : uint8_t) return lv_color_t;  -- ../lv_misc/lv_color.h:410
   pragma Import (C, lv_color_hsv_to_rgb, "lv_color_hsv_to_rgb");

   function lv_color_rgb_to_hsv
     (arg1 : uint8_t;
      arg2 : uint8_t;
      arg3 : uint8_t) return lv_color_hsv_t;  -- ../lv_misc/lv_color.h:419
   pragma Import (C, lv_color_rgb_to_hsv, "lv_color_rgb_to_hsv");

end LV.Color;
