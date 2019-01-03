pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions;
with System;

package LV.Font is

   type Font is private;

   No_Font : constant Font;

   procedure init;  -- ../lv_misc/lv_font.h:72
   pragma Import (C, init, "lv_font_init");

   procedure add (Child : Font; Parent : Font);  -- ../lv_misc/lv_font.h:79
   pragma Import (C, add, "lv_font_add");

   function is_monospace (F : Font; Letter : uint32_t) return u_Bool;  -- ../lv_misc/lv_font.h:87
   pragma Import (C, is_monospace, "lv_font_is_monospace");

   function get_bitmap (F : Font; Letter : uint32_t) return access uint8_t;  -- ../lv_misc/lv_font.h:95
   pragma Import (C, get_bitmap, "lv_font_get_bitmap");

   function get_width (F : Font; Letter : uint32_t) return uint8_t;  -- ../lv_misc/lv_font.h:103
   pragma Import (C, get_width, "lv_font_get_width");

   function get_real_width (F : Font; Letter : uint32_t) return uint8_t;  -- ../lv_misc/lv_font.h:112
   pragma Import (C, get_real_width, "lv_font_get_real_width");

   function get_height (F : Font) return uint8_t;  -- ../lv_misc/lv_font.h:119
   pragma Import (C, get_height, "lv_font_get_height_inline");

   function get_bpp (F : Font; Letter : uint32_t) return uint8_t;  -- ../lv_misc/lv_font.h:130
   pragma Import (C, get_bpp, "lv_font_get_bpp");

   function get_bitmap_continuous (F : Font; Unicode_Letter : uint32_t) return access uint8_t;  -- ../lv_misc/lv_font.h:138
   pragma Import (C, get_bitmap_continuous, "lv_font_get_bitmap_continuous");

   function get_bitmap_sparse (F : Font; Unicode_Letter : uint32_t) return access uint8_t;  -- ../lv_misc/lv_font.h:146
   pragma Import (C, get_bitmap_sparse, "lv_font_get_bitmap_sparse");

   function get_width_continuous (F : Font; Unicode_Letter : uint32_t) return int16_t;  -- ../lv_misc/lv_font.h:153
   pragma Import (C, get_width_continuous, "lv_font_get_width_continuous");

   function get_width_sparse (F : Font; Unicode_Letter : uint32_t) return int16_t;  -- ../lv_misc/lv_font.h:161
   pragma Import (C, get_width_sparse, "lv_font_get_width_sparse");

private

   type Font is new System.Address;
   No_Font : constant Font := Font (System.Null_Address);


--     --  arg-macro: procedure DECLARE (font_name)
--     --    extern t font_name;
--     type lv_font_glyph_dsc_t is record
--        w_px : aliased unsigned_char;  -- ../lv_misc/lv_font.h:39
--        glyph_index : Extensions.Unsigned_24;  -- ../lv_misc/lv_font.h:40
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_font_glyph_dsc_t);
--     pragma Pack (lv_font_glyph_dsc_t);  -- ../lv_misc/lv_font.h:41
--
--     type lv_font_unicode_map_t is record
--        unicode : Extensions.Unsigned_21;  -- ../lv_misc/lv_font.h:45
--        glyph_dsc_index : Extensions.Unsigned_11;  -- ../lv_misc/lv_font.h:46
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_font_unicode_map_t);
--     pragma Pack (lv_font_unicode_map_t);  -- ../lv_misc/lv_font.h:47
--
--     type u_lv_font_struct is record
--        unicode_first : aliased uint32_t;  -- ../lv_misc/lv_font.h:51
--        unicode_last : aliased uint32_t;  -- ../lv_misc/lv_font.h:52
--        glyph_bitmap : access uint8_t;  -- ../lv_misc/lv_font.h:53
--        glyph_dsc : access constant lv_font_glyph_dsc_t;  -- ../lv_misc/lv_font.h:54
--        unicode_list : access uint32_t;  -- ../lv_misc/lv_font.h:55
--        get_bitmap : access function (arg1 : System.Address; arg2 : uint32_t) return access uint8_t;  -- ../lv_misc/lv_font.h:56
--        get_width : access function (arg1 : System.Address; arg2 : uint32_t) return int16_t;  -- ../lv_misc/lv_font.h:57
--        next_page : access u_lv_font_struct;  -- ../lv_misc/lv_font.h:58
--        h_px : aliased unsigned_char;  -- ../lv_misc/lv_font.h:59
--        bpp : Extensions.Unsigned_4;  -- ../lv_misc/lv_font.h:60
--        monospace : aliased unsigned_char;  -- ../lv_misc/lv_font.h:61
--        glyph_cnt : aliased uint16_t;  -- ../lv_misc/lv_font.h:62
--     end record;
--     pragma Convention (C_Pass_By_Copy, u_lv_font_struct);
--     pragma Pack (u_lv_font_struct);  -- ../lv_misc/lv_font.h:49
--
--     subtype lv_font_t is u_lv_font_struct;  -- ../lv_misc/lv_font.h:63

end LV.Font;
