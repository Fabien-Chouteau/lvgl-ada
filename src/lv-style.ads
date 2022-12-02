with System;

package Lv.Style is

   type Style is private;

   type Style_Anim is private;

   --  unsupported macro: LV_RADIUS_CIRCLE (LV_COORD_MAX)
   subtype Border_Part_T is Uint8_T;

   subtype Shadow_Type_T is Uint8_T;

   procedure Style_Init;
   pragma Import (C, Style_Init, "lv_style_init");

   procedure Style_Copy (Dest : access Style; Src : access Style);
   pragma Import (C, Style_Copy, "lv_style_copy");

   procedure Style_Mix
     (Start  : access Style;
      End_P  : access Style;
      Result : access Style;
      Ratio  : Uint16_T);
   pragma Import (C, Style_Mix, "lv_style_mix");

   function Style_Anim_Create (Anim_P : Style_Anim) return System.Address;
   pragma Import (C, Style_Anim_Create, "lv_style_anim_create");

   Style_Scr          : constant Style;
   Style_Transp       : constant Style;
   Style_Transp_Fit   : constant Style;
   Style_Transp_Tight : constant Style;
   Style_Plain        : constant Style;
   Style_Plain_Color  : constant Style;
   Style_Pretty       : constant Style;
   Style_Pretty_Color : constant Style;
   Style_Btn_Rel      : constant Style;
   Style_Btn_Pr       : constant Style;
   Style_Btn_Tgl_Rel  : constant Style;
   Style_Btn_Tgl_Pr   : constant Style;
   Style_Btn_Ina      : constant Style;

private
   type Style is new System.Address;

   Lv_Style_Scr : aliased Style;
   pragma Import (C, Lv_Style_Scr, "lv_style_scr");
   Lv_Style_Transp : aliased Style;
   pragma Import (C, Lv_Style_Transp, "lv_style_transp");
   Lv_Style_Transp_Fit : aliased Style;
   pragma Import (C, Lv_Style_Transp_Fit, "lv_style_transp_fit");
   Lv_Style_Transp_Tight : aliased Style;
   pragma Import (C, Lv_Style_Transp_Tight, "lv_style_transp_tight");
   Lv_Style_Plain : aliased Style;
   pragma Import (C, Lv_Style_Plain, "lv_style_plain");
   Lv_Style_Plain_Color : aliased Style;
   pragma Import (C, Lv_Style_Plain_Color, "lv_style_plain_color");
   Lv_Style_Pretty : aliased Style;
   pragma Import (C, Lv_Style_Pretty, "lv_style_pretty");
   Lv_Style_Pretty_Color : aliased Style;
   pragma Import (C, Lv_Style_Pretty_Color, "lv_style_pretty_color");
   Lv_Style_Btn_Rel : aliased Style;
   pragma Import (C, Lv_Style_Btn_Rel, "lv_style_btn_rel");
   Lv_Style_Btn_Pr : aliased Style;
   pragma Import (C, Lv_Style_Btn_Pr, "lv_style_btn_pr");
   Lv_Style_Btn_Tgl_Rel : aliased Style;
   pragma Import (C, Lv_Style_Btn_Tgl_Rel, "lv_style_btn_tgl_rel");
   Lv_Style_Btn_Tgl_Pr : aliased Style;
   pragma Import (C, Lv_Style_Btn_Tgl_Pr, "lv_style_btn_tgl_pr");
   Lv_Style_Btn_Ina : aliased Style;
   pragma Import (C, Lv_Style_Btn_Ina, "lv_style_btn_ina");

   Style_Scr          : constant Style := Style (Lv_Style_Scr'Address);
   Style_Transp       : constant Style := Style (Lv_Style_Transp'Address);
   Style_Transp_Fit   : constant Style := Style (Lv_Style_Transp_Fit'Address);
   Style_Transp_Tight : constant Style := Style (Lv_Style_Transp_Tight'Address);
   Style_Plain        : constant Style := Style (Lv_Style_Plain'Address);
   Style_Plain_Color  : constant Style := Style (Lv_Style_Plain_Color'Address);
   Style_Pretty       : constant Style := Style (Lv_Style_Pretty'Address);
   Style_Pretty_Color : constant Style := Style (Lv_Style_Pretty_Color'Address);
   Style_Btn_Rel      : constant Style := Style (Lv_Style_Btn_Rel'Address);
   Style_Btn_Pr       : constant Style := Style (Lv_Style_Btn_Pr'Address);
   Style_Btn_Tgl_Rel  : constant Style := Style (Lv_Style_Btn_Tgl_Rel'Address);
   Style_Btn_Tgl_Pr   : constant Style := Style (Lv_Style_Btn_Tgl_Pr'Address);
   Style_Btn_Ina      : constant Style := Style (Lv_Style_Btn_Ina'Address);

   type Style_Anim is new System.Address;

   No_Style_Anim : constant Style_Anim := Style_Anim (System.Null_Address);

--     type lv_style_t;
--     type lv_style_t_border_struct is record
--        color : aliased LV.Color.lv_color_t;
--        width : aliased LV.Area.Coord_T;
--        part : aliased lv_border_part_t;
--        opa : aliased LV.Color.lv_opa_t;
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_style_t_border_struct);
--     type lv_style_t_shadow_struct is record
--        color : aliased LV.Color.lv_color_t;
--        width : aliased LV.Area.Coord_T;
--        c_type : aliased lv_shadow_type_t;
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_style_t_shadow_struct);
--     type lv_style_t_padding_struct is record
--        ver : aliased LV.Area.Coord_T;
--        hor : aliased LV.Area.Coord_T;
--        inner : aliased LV.Area.Coord_T;
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_style_t_padding_struct);
--     type lv_style_t_c_body_struct is record
--        main_color : aliased LV.Color.lv_color_t;
--        grad_color : aliased LV.Color.lv_color_t;
--        radius : aliased LV.Area.Coord_T;
--        opa : aliased LV.Color.lv_opa_t;
--        border : aliased lv_style_t_border_struct;
--        shadow : aliased lv_style_t_shadow_struct;
--        padding : aliased lv_style_t_padding_struct;
--        empty : Extensions.Unsigned_1;
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_style_t_c_body_struct);
--     type lv_style_t_text_struct is record
--        color : aliased LV.Color.lv_color_t;
--        font : access constant LV.Font.lv_font_t;
--        letter_space : aliased LV.Area.Coord_T;
--        line_space : aliased LV.Area.Coord_T;
--        opa : aliased LV.Color.lv_opa_t;
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_style_t_text_struct);
--     type lv_style_t_image_struct is record
--        color : aliased LV.Color.lv_color_t;
--        intense : aliased LV.Color.lv_opa_t;
--        opa : aliased LV.Color.lv_opa_t;
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_style_t_image_struct);
--     type lv_style_t_line_struct is record
--        color : aliased LV.Color.lv_color_t;
--        width : aliased LV.Area.Coord_T;
--        opa : aliased LV.Color.lv_opa_t;
--        rounded : Extensions.Unsigned_1;
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_style_t_line_struct);
--     type lv_style_t is record
--        glass : Extensions.Unsigned_1;
--        c_body : aliased lv_style_t_c_body_struct;
--        text : aliased lv_style_t_text_struct;
--        image : aliased lv_style_t_image_struct;
--        line : aliased lv_style_t_line_struct;
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_style_t);
--     pragma Pack (lv_style_t);
--
--     type lv_style_anim_t is record
--        style_start : access constant lv_style_t;
--        style_end : access constant lv_style_t;
--        style_anim : access lv_style_t;
--        end_cb : LV.Anim.lv_anim_cb_t;
--        time : aliased int16_t;
--        act_time : aliased int16_t;
--        playback_pause : aliased uint16_t;
--        repeat_pause : aliased uint16_t;
--        playback : Extensions.Unsigned_1;
--        repeat : Extensions.Unsigned_1;
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_style_anim_t);
--     pragma Pack (lv_style_anim_t);

end Lv.Style;
