private with System;

with Lv.Font;

package Lv.Theme is

   type Theme is private;

   No_Theme : constant Theme;

   subtype Hue_T is Uint16_T range 0 .. 360;

   procedure Set_Current (T : Theme);  -- lv_theme.h:280
   pragma Import (C, Set_Current, "lv_theme_set_current");

   function Get_Current return Theme;  -- lv_theme.h:286
   pragma Import (C, Get_Current, "lv_theme_get_current");

   function Material_Init
     (Hue  : Hue_T;
      Font : Lv.Font.Font) return Theme;  -- default.h:42
   pragma Import (C, Material_Init, "lv_theme_material_init");

   function Get_Material return access int;  -- material.h:48
   pragma Import (C, Get_Material, "lv_theme_get_material");

   function Mono_Init
     (Hue  : Hue_T;
      Font : Lv.Font.Font) return Theme;  -- default.h:42
   pragma Import (C, Mono_Init, "lv_theme_mono_init");

   function Get_Mono return access int;  -- mono.h:48
   pragma Import (C, Get_Mono, "lv_theme_get_mono");

   function Alien_Init
     (Hue  : Hue_T;
      Font : Lv.Font.Font) return Theme;  -- alien.h:42
   pragma Import (C, Alien_Init, "lv_theme_alien_init");

   function Get_Alien return Theme;  -- alien.h:47
   pragma Import (C, Get_Alien, "lv_theme_get_alien");

   function Default_Init
     (Hue  : Hue_T;
      Font : Lv.Font.Font) return Theme;  -- default.h:42
   pragma Import (C, Default_Init, "lv_theme_default_init");

   function Get_Default return Theme;  -- default.h:48
   pragma Import (C, Get_Default, "lv_theme_get_default");

   function Nemo_Init (Hue : Hue_T; Font : Lv.Font.Font) return Theme;
   pragma Import (C, Nemo_Init, "lv_theme_nemo_init");

   function Get_Nemo return Theme;  -- nemo.h:48
   pragma Import (C, Get_Nemo, "lv_theme_get_nemo");

   function Night_Init (Hue : Hue_T; Font : Lv.Font.Font) return Theme;
   pragma Import (C, Night_Init, "lv_theme_night_init");

   function Get_Night return Theme;  -- night.h:48
   pragma Import (C, Get_Night, "lv_theme_get_night");

   function Zen_Init (Hue : Hue_T; Font : Lv.Font.Font) return Theme;
   pragma Import (C, Zen_Init, "lv_theme_zen_init");

   function Get_Zen return Theme;  -- zen.h:48
   pragma Import (C, Get_Zen, "lv_theme_get_zen");

private

   type Theme is new System.Address;
   No_Theme : constant Theme := Theme (System.Null_Address);

--     type lv_theme_t_btn_struct is record
--        rel : access lv_style_h.lv_style_t;  -- lv_theme.h:42
--        pr : access lv_style_h.lv_style_t;  -- lv_theme.h:43
--        tgl_rel : access lv_style_h.lv_style_t;  -- lv_theme.h:44
--        tgl_pr : access lv_style_h.lv_style_t;  -- lv_theme.h:45
--        ina : access lv_style_h.lv_style_t;  -- lv_theme.h:46
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_theme_t_btn_struct);
--
--     type lv_theme_t_imgbtn_struct is record
--        rel : access lv_style_h.lv_style_t;  -- lv_theme.h:53
--        pr : access lv_style_h.lv_style_t;  -- lv_theme.h:54
--        tgl_rel : access lv_style_h.lv_style_t;  -- lv_theme.h:55
--        tgl_pr : access lv_style_h.lv_style_t;  -- lv_theme.h:56
--        ina : access lv_style_h.lv_style_t;  -- lv_theme.h:57
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_theme_t_imgbtn_struct);
--     type lv_theme_t_label_struct is record
--        prim : access lv_style_h.lv_style_t;  -- lv_theme.h:63
--        sec : access lv_style_h.lv_style_t;  -- lv_theme.h:64
--        hint : access lv_style_h.lv_style_t;  -- lv_theme.h:65
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_theme_t_label_struct);
--     type lv_theme_t_img_struct is record
--        light : access lv_style_h.lv_style_t;  -- lv_theme.h:71
--        dark : access lv_style_h.lv_style_t;  -- lv_theme.h:72
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_theme_t_img_struct);
--     type lv_theme_t_line_struct is record
--        decor : access lv_style_h.lv_style_t;  -- lv_theme.h:78
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_theme_t_line_struct);
--     type lv_theme_t_bar_struct is record
--        bg : access lv_style_h.lv_style_t;  -- lv_theme.h:88
--        indic : access lv_style_h.lv_style_t;  -- lv_theme.h:89
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_theme_t_bar_struct);
--     type lv_theme_t_slider_struct is record
--        bg : access lv_style_h.lv_style_t;  -- lv_theme.h:95
--        indic : access lv_style_h.lv_style_t;  -- lv_theme.h:96
--        knob : access lv_style_h.lv_style_t;  -- lv_theme.h:97
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_theme_t_slider_struct);
--     type lv_theme_t_sw_struct is record
--        bg : access lv_style_h.lv_style_t;  -- lv_theme.h:119
--        indic : access lv_style_h.lv_style_t;  -- lv_theme.h:120
--        knob_off : access lv_style_h.lv_style_t;  -- lv_theme.h:121
--        knob_on : access lv_style_h.lv_style_t;  -- lv_theme.h:122
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_theme_t_sw_struct);
--     type lv_theme_t_calendar_struct is record
--        bg : access lv_style_h.lv_style_t;  -- lv_theme.h:132
--        header : access lv_style_h.lv_style_t;  -- lv_theme.h:133
--        header_pr : access lv_style_h.lv_style_t;  -- lv_theme.h:134
--        day_names : access lv_style_h.lv_style_t;  -- lv_theme.h:135
--        highlighted_days : access lv_style_h.lv_style_t;  -- lv_theme.h:136
--        inactive_days : access lv_style_h.lv_style_t;  -- lv_theme.h:137
--        week_box : access lv_style_h.lv_style_t;  -- lv_theme.h:138
--        today_box : access lv_style_h.lv_style_t;  -- lv_theme.h:139
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_theme_t_calendar_struct);
--     type lv_theme_t_box_struct is record
--        rel : access lv_style_h.lv_style_t;  -- lv_theme.h:147
--        pr : access lv_style_h.lv_style_t;  -- lv_theme.h:148
--        tgl_rel : access lv_style_h.lv_style_t;  -- lv_theme.h:149
--        tgl_pr : access lv_style_h.lv_style_t;  -- lv_theme.h:150
--        ina : access lv_style_h.lv_style_t;  -- lv_theme.h:151
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_theme_t_box_struct);
--     type lv_theme_t_cb_struct is record
--        bg : access lv_style_h.lv_style_t;  -- lv_theme.h:145
--        box : aliased lv_theme_t_box_struct;  -- lv_theme.h:152
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_theme_t_cb_struct);
--     type lv_theme_t_btn_struct is record
--        rel : access lv_style_h.lv_style_t;  -- lv_theme.h:160
--        pr : access lv_style_h.lv_style_t;  -- lv_theme.h:161
--        tgl_rel : access lv_style_h.lv_style_t;  -- lv_theme.h:162
--        tgl_pr : access lv_style_h.lv_style_t;  -- lv_theme.h:163
--        ina : access lv_style_h.lv_style_t;  -- lv_theme.h:164
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_theme_t_btn_struct);
--     type lv_theme_t_btnm_struct is record
--        bg : access lv_style_h.lv_style_t;  -- lv_theme.h:158
--        btn : aliased lv_theme_t_btn_struct;  -- lv_theme.h:165
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_theme_t_btnm_struct);
--     type lv_theme_t_btn_struct is record
--        rel : access lv_style_h.lv_style_t;  -- lv_theme.h:173
--        pr : access lv_style_h.lv_style_t;  -- lv_theme.h:174
--        tgl_rel : access lv_style_h.lv_style_t;  -- lv_theme.h:175
--        tgl_pr : access lv_style_h.lv_style_t;  -- lv_theme.h:176
--        ina : access lv_style_h.lv_style_t;  -- lv_theme.h:177
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_theme_t_btn_struct);
--     type lv_theme_t_kb_struct is record
--        bg : access lv_style_h.lv_style_t;  -- lv_theme.h:171
--        btn : aliased lv_theme_t_btn_struct;  -- lv_theme.h:178
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_theme_t_kb_struct);
--     type lv_theme_t_btn_struct is record
--        bg : access lv_style_h.lv_style_t;  -- lv_theme.h:186
--        rel : access lv_style_h.lv_style_t;  -- lv_theme.h:187
--        pr : access lv_style_h.lv_style_t;  -- lv_theme.h:188
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_theme_t_btn_struct);
--     type lv_theme_t_mbox_struct is record
--        bg : access lv_style_h.lv_style_t;  -- lv_theme.h:184
--        btn : aliased lv_theme_t_btn_struct;  -- lv_theme.h:189
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_theme_t_mbox_struct);
--     type lv_theme_t_page_struct is record
--        bg : access lv_style_h.lv_style_t;  -- lv_theme.h:195
--        scrl : access lv_style_h.lv_style_t;  -- lv_theme.h:196
--        sb : access lv_style_h.lv_style_t;  -- lv_theme.h:197
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_theme_t_page_struct);
--     type lv_theme_t_ta_struct is record
--        area : access lv_style_h.lv_style_t;  -- lv_theme.h:203
--        oneline : access lv_style_h.lv_style_t;  -- lv_theme.h:204
--        cursor : access lv_style_h.lv_style_t;  -- lv_theme.h:205
--        sb : access lv_style_h.lv_style_t;  -- lv_theme.h:206
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_theme_t_ta_struct);
--     type lv_theme_t_btn_struct is record
--        rel : access lv_style_h.lv_style_t;  -- lv_theme.h:216
--        pr : access lv_style_h.lv_style_t;  -- lv_theme.h:217
--        tgl_rel : access lv_style_h.lv_style_t;  -- lv_theme.h:218
--        tgl_pr : access lv_style_h.lv_style_t;  -- lv_theme.h:219
--        ina : access lv_style_h.lv_style_t;  -- lv_theme.h:220
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_theme_t_btn_struct);
--     type lv_theme_t_list_struct is record
--        bg : access lv_style_h.lv_style_t;  -- lv_theme.h:212
--        scrl : access lv_style_h.lv_style_t;  -- lv_theme.h:213
--        sb : access lv_style_h.lv_style_t;  -- lv_theme.h:214
--        btn : aliased lv_theme_t_btn_struct;  -- lv_theme.h:221
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_theme_t_list_struct);
--     type lv_theme_t_ddlist_struct is record
--        bg : access lv_style_h.lv_style_t;  -- lv_theme.h:227
--        sel : access lv_style_h.lv_style_t;  -- lv_theme.h:228
--        sb : access lv_style_h.lv_style_t;  -- lv_theme.h:229
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_theme_t_ddlist_struct);
--     type lv_theme_t_roller_struct is record
--        bg : access lv_style_h.lv_style_t;  -- lv_theme.h:235
--        sel : access lv_style_h.lv_style_t;  -- lv_theme.h:236
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_theme_t_roller_struct);
--     type lv_theme_t_btn_struct is record
--        bg : access lv_style_h.lv_style_t;  -- lv_theme.h:245
--        rel : access lv_style_h.lv_style_t;  -- lv_theme.h:246
--        pr : access lv_style_h.lv_style_t;  -- lv_theme.h:247
--        tgl_rel : access lv_style_h.lv_style_t;  -- lv_theme.h:248
--        tgl_pr : access lv_style_h.lv_style_t;  -- lv_theme.h:249
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_theme_t_btn_struct);
--     type lv_theme_t_tabview_struct is record
--        bg : access lv_style_h.lv_style_t;  -- lv_theme.h:242
--        indic : access lv_style_h.lv_style_t;  -- lv_theme.h:243
--        btn : aliased lv_theme_t_btn_struct;  -- lv_theme.h:250
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_theme_t_tabview_struct);
--     type lv_theme_t_content_struct is record
--        bg : access lv_style_h.lv_style_t;  -- lv_theme.h:260
--        scrl : access lv_style_h.lv_style_t;  -- lv_theme.h:261
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_theme_t_content_struct);
--     type lv_theme_t_btn_struct is record
--        rel : access lv_style_h.lv_style_t;  -- lv_theme.h:264
--        pr : access lv_style_h.lv_style_t;  -- lv_theme.h:265
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_theme_t_btn_struct);
--     type lv_theme_t_win_struct is record
--        bg : access lv_style_h.lv_style_t;  -- lv_theme.h:256
--        sb : access lv_style_h.lv_style_t;  -- lv_theme.h:257
--        header : access lv_style_h.lv_style_t;  -- lv_theme.h:258
--        content : aliased lv_theme_t_content_struct;  -- lv_theme.h:262
--        btn : aliased lv_theme_t_btn_struct;  -- lv_theme.h:266
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_theme_t_win_struct);
--     type lv_theme_t is record
--        bg : access lv_style_h.lv_style_t;  -- lv_theme.h:33
--        panel : access lv_style_h.lv_style_t;  -- lv_theme.h:34
--        cont : access lv_style_h.lv_style_t;  -- lv_theme.h:37
--        btn : aliased lv_theme_t_btn_struct;  -- lv_theme.h:47
--        imgbtn : aliased lv_theme_t_imgbtn_struct;  -- lv_theme.h:58
--        label : aliased lv_theme_t_label_struct;  -- lv_theme.h:66
--        img : aliased lv_theme_t_img_struct;  -- lv_theme.h:73
--        line : aliased lv_theme_t_line_struct;  -- lv_theme.h:79
--        led : access lv_style_h.lv_style_t;  -- lv_theme.h:83
--        bar : aliased lv_theme_t_bar_struct;  -- lv_theme.h:90
--        slider : aliased lv_theme_t_slider_struct;  -- lv_theme.h:98
--        lmeter : access lv_style_h.lv_style_t;  -- lv_theme.h:102
--        gauge : access lv_style_h.lv_style_t;  -- lv_theme.h:106
--        arc : access lv_style_h.lv_style_t;  -- lv_theme.h:110
--        preload : access lv_style_h.lv_style_t;  -- lv_theme.h:114
--        sw : aliased lv_theme_t_sw_struct;  -- lv_theme.h:123
--        chart : access lv_style_h.lv_style_t;  -- lv_theme.h:127
--        calendar : aliased lv_theme_t_calendar_struct;  -- lv_theme.h:140
--        cb : aliased lv_theme_t_cb_struct;  -- lv_theme.h:153
--        btnm : aliased lv_theme_t_btnm_struct;  -- lv_theme.h:166
--        kb : aliased lv_theme_t_kb_struct;  -- lv_theme.h:179
--        mbox : aliased lv_theme_t_mbox_struct;  -- lv_theme.h:190
--        page : aliased lv_theme_t_page_struct;  -- lv_theme.h:198
--        ta : aliased lv_theme_t_ta_struct;  -- lv_theme.h:207
--        list : aliased lv_theme_t_list_struct;  -- lv_theme.h:222
--        ddlist : aliased lv_theme_t_ddlist_struct;  -- lv_theme.h:230
--        roller : aliased lv_theme_t_roller_struct;  -- lv_theme.h:237
--        tabview : aliased lv_theme_t_tabview_struct;  -- lv_theme.h:251
--        win : aliased lv_theme_t_win_struct;  -- lv_theme.h:267
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_theme_t);  -- lv_theme.h:269

end Lv.Theme;
