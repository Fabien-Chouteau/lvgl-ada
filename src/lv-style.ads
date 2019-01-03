pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions;
with System;

with LV.Color;
with LV.Anim;
with LV.Area;
with LV.Font;

package LV.Style is

   type Style is private;

   type Style_Anim is private;

   --  unsupported macro: LV_RADIUS_CIRCLE (LV_COORD_MAX)
   subtype lv_border_part_t is uint8_t;  -- lv_style.h:41

   subtype lv_shadow_type_t is uint8_t;  -- lv_style.h:49

   procedure lv_style_init;  -- lv_style.h:143
   pragma Import (C, lv_style_init, "lv_style_init");

   procedure lv_style_copy (Dest : access Style; Src : access Style);  -- lv_style.h:150
   pragma Import (C, lv_style_copy, "lv_style_copy");

   procedure lv_style_mix
     (Start  : access Style;
      End_P  : access Style;
      Result : access Style;
      Ratio  : uint16_t);  -- lv_style.h:160
   pragma Import (C, lv_style_mix, "lv_style_mix");

   function lv_style_anim_create (Anim_P : Style_Anim) return System.Address;  -- lv_style.h:169
   pragma Import (C, lv_style_anim_create, "lv_style_anim_create");

   Style_scr          : constant access constant Style;
   Style_transp       : constant access constant Style;
   Style_transp_fit   : constant access constant Style;
   Style_transp_tight : constant access constant Style;
   Style_plain        : constant access constant Style;
   Style_plain_color  : constant access constant Style;
   Style_pretty       : constant access constant Style;
   Style_pretty_color : constant access constant Style;
   Style_btn_rel      : constant access constant Style;
   Style_btn_pr       : constant access constant Style;
   Style_btn_tgl_rel  : constant access constant Style;
   Style_btn_tgl_pr   : constant access constant Style;
   Style_btn_ina      : constant access constant Style;

private
   type Style is new uint32_t; --  FIXME: proper style mapping

   lv_style_scr : aliased Style;  -- lv_style.h:175
   pragma Import (C, lv_style_scr, "lv_style_scr");
   lv_style_transp : aliased Style;  -- lv_style.h:176
   pragma Import (C, lv_style_transp, "lv_style_transp");
   lv_style_transp_fit : aliased Style;  -- lv_style.h:177
   pragma Import (C, lv_style_transp_fit, "lv_style_transp_fit");
   lv_style_transp_tight : aliased Style;  -- lv_style.h:178
   pragma Import (C, lv_style_transp_tight, "lv_style_transp_tight");
   lv_style_plain : aliased Style;  -- lv_style.h:179
   pragma Import (C, lv_style_plain, "lv_style_plain");
   lv_style_plain_color : aliased Style;  -- lv_style.h:180
   pragma Import (C, lv_style_plain_color, "lv_style_plain_color");
   lv_style_pretty : aliased Style;  -- lv_style.h:181
   pragma Import (C, lv_style_pretty, "lv_style_pretty");
   lv_style_pretty_color : aliased Style;  -- lv_style.h:182
   pragma Import (C, lv_style_pretty_color, "lv_style_pretty_color");
   lv_style_btn_rel : aliased Style;  -- lv_style.h:183
   pragma Import (C, lv_style_btn_rel, "lv_style_btn_rel");
   lv_style_btn_pr : aliased Style;  -- lv_style.h:184
   pragma Import (C, lv_style_btn_pr, "lv_style_btn_pr");
   lv_style_btn_tgl_rel : aliased Style;  -- lv_style.h:185
   pragma Import (C, lv_style_btn_tgl_rel, "lv_style_btn_tgl_rel");
   lv_style_btn_tgl_pr : aliased Style;  -- lv_style.h:186
   pragma Import (C, lv_style_btn_tgl_pr, "lv_style_btn_tgl_pr");
   lv_style_btn_ina : aliased Style;  -- lv_style.h:187
   pragma Import (C, lv_style_btn_ina, "lv_style_btn_ina");

   Style_Scr          : constant access constant Style := lv_style_scr'Access;
   Style_Transp       : constant access constant Style := lv_style_transp'Access;
   Style_Transp_Fit   : constant access constant Style := lv_style_transp_fit'Access;
   Style_Transp_Tight : constant access constant Style := lv_style_transp_tight'Access;
   Style_Plain        : constant access constant Style := lv_style_plain'Access;
   Style_Plain_Color  : constant access constant Style := lv_style_plain_color'Access;
   Style_Pretty       : constant access constant Style := lv_style_pretty'Access;
   Style_Pretty_Color : constant access constant Style := lv_style_pretty_color'Access;
   Style_Btn_Rel      : constant access constant Style := lv_style_btn_rel'Access;
   Style_Btn_Pr       : constant access constant Style := lv_style_btn_pr'Access;
   Style_Btn_Tgl_Rel  : constant access constant Style := lv_style_btn_tgl_rel'Access;
   Style_Btn_Tgl_Pr   : constant access constant Style := lv_style_btn_tgl_pr'Access;
   Style_Btn_Ina      : constant access constant Style := lv_style_btn_ina'Access;

   type Style_Anim is new System.Address;

   No_Style_Anim : constant Style_Anim := Style_Anim (System.Null_Address);

   --     type lv_style_t;
--     type lv_style_t_border_struct is record
--        color : aliased LV.Color.lv_color_t;  -- lv_style.h:62
--        width : aliased LV.Area.Coord_T;  -- lv_style.h:63
--        part : aliased lv_border_part_t;  -- lv_style.h:64
--        opa : aliased LV.Color.lv_opa_t;  -- lv_style.h:65
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_style_t_border_struct);
--     type lv_style_t_shadow_struct is record
--        color : aliased LV.Color.lv_color_t;  -- lv_style.h:69
--        width : aliased LV.Area.Coord_T;  -- lv_style.h:70
--        c_type : aliased lv_shadow_type_t;  -- lv_style.h:71
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_style_t_shadow_struct);
--     type lv_style_t_padding_struct is record
--        ver : aliased LV.Area.Coord_T;  -- lv_style.h:75
--        hor : aliased LV.Area.Coord_T;  -- lv_style.h:76
--        inner : aliased LV.Area.Coord_T;  -- lv_style.h:77
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_style_t_padding_struct);
--     type lv_style_t_c_body_struct is record
--        main_color : aliased LV.Color.lv_color_t;  -- lv_style.h:56
--        grad_color : aliased LV.Color.lv_color_t;  -- lv_style.h:57
--        radius : aliased LV.Area.Coord_T;  -- lv_style.h:58
--        opa : aliased LV.Color.lv_opa_t;  -- lv_style.h:59
--        border : aliased lv_style_t_border_struct;  -- lv_style.h:66
--        shadow : aliased lv_style_t_shadow_struct;  -- lv_style.h:72
--        padding : aliased lv_style_t_padding_struct;  -- lv_style.h:78
--        empty : Extensions.Unsigned_1;  -- lv_style.h:80
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_style_t_c_body_struct);
--     type lv_style_t_text_struct is record
--        color : aliased LV.Color.lv_color_t;  -- lv_style.h:85
--        font : access constant LV.Font.lv_font_t;  -- lv_style.h:86
--        letter_space : aliased LV.Area.Coord_T;  -- lv_style.h:87
--        line_space : aliased LV.Area.Coord_T;  -- lv_style.h:88
--        opa : aliased LV.Color.lv_opa_t;  -- lv_style.h:89
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_style_t_text_struct);
--     type lv_style_t_image_struct is record
--        color : aliased LV.Color.lv_color_t;  -- lv_style.h:93
--        intense : aliased LV.Color.lv_opa_t;  -- lv_style.h:94
--        opa : aliased LV.Color.lv_opa_t;  -- lv_style.h:95
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_style_t_image_struct);
--     type lv_style_t_line_struct is record
--        color : aliased LV.Color.lv_color_t;  -- lv_style.h:99
--        width : aliased LV.Area.Coord_T;  -- lv_style.h:100
--        opa : aliased LV.Color.lv_opa_t;  -- lv_style.h:101
--        rounded : Extensions.Unsigned_1;  -- lv_style.h:102
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_style_t_line_struct);
--     type lv_style_t is record
--        glass : Extensions.Unsigned_1;  -- lv_style.h:53
--        c_body : aliased lv_style_t_c_body_struct;  -- lv_style.h:81
--        text : aliased lv_style_t_text_struct;  -- lv_style.h:90
--        image : aliased lv_style_t_image_struct;  -- lv_style.h:96
--        line : aliased lv_style_t_line_struct;  -- lv_style.h:103
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_style_t);
--     pragma Pack (lv_style_t);  -- lv_style.h:104
--
--     type lv_style_anim_t is record
--        style_start : access constant lv_style_t;  -- lv_style.h:108
--        style_end : access constant lv_style_t;  -- lv_style.h:109
--        style_anim : access lv_style_t;  -- lv_style.h:110
--        end_cb : LV.Anim.lv_anim_cb_t;  -- lv_style.h:111
--        time : aliased int16_t;  -- lv_style.h:112
--        act_time : aliased int16_t;  -- lv_style.h:113
--        playback_pause : aliased uint16_t;  -- lv_style.h:114
--        repeat_pause : aliased uint16_t;  -- lv_style.h:115
--        playback : Extensions.Unsigned_1;  -- lv_style.h:116
--        repeat : Extensions.Unsigned_1;  -- lv_style.h:117
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_style_anim_t);
--     pragma Pack (lv_style_anim_t);  -- lv_style.h:118

end LV.Style;
