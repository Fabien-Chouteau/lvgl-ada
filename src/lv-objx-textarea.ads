pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with LV.Style;
with Interfaces.C.Extensions;

with Interfaces.C.Strings;

with LV.Objx.Page;
with LV.Objx.Label;
with LV.Area;

package LV.Objx.Textarea is

   subtype Textarea is Obj_T;

   LV_TA_CURSOR_LAST : constant := (16#7FFF#);  --  lv_ta.h:40

   type lv_cursor_type_t is
     (CURSOR_NONE,
      CURSOR_LINE,
      CURSOR_BLOCK,
      CURSOR_OUTLINE,
      CURSOR_UNDERLINE,
      CURSOR_HIDDEN)
     with Size => 8;

   for lv_cursor_type_t use
     (CURSOR_NONE      => 0,
      CURSOR_LINE      => 1,
      CURSOR_BLOCK     => 2,
      CURSOR_OUTLINE   => 3,
      CURSOR_UNDERLINE => 4,
      CURSOR_HIDDEN    => 8);

   type lv_ta_style_t is
     (STYLE_BG,
      STYLE_SB,
      STYLE_CURSOR)
   with Size => 8;
   --  subtype lv_ta_style_t is uint8_t;  -- lv_ta.h:81

   function create (Par : Obj_T; Copy : Obj_T) return Textarea;  -- lv_ta.h:94
   pragma Import (C, create, "lv_ta_create");

   procedure add_char (Self : Textarea; arg2 : uint32_t);  -- lv_ta.h:107
   pragma Import (C, add_char, "lv_ta_add_char");

   procedure add_text (Self : Textarea; arg2 : Interfaces.C.Strings.chars_ptr);  -- lv_ta.h:114
   pragma Import (C, add_text, "lv_ta_add_text");

   procedure del_char (Self : Textarea);  -- lv_ta.h:120
   pragma Import (C, del_char, "lv_ta_del_char");

   procedure set_text (Self : Textarea; arg2 : Interfaces.C.Strings.chars_ptr);  -- lv_ta.h:131
   pragma Import (C, set_text, "lv_ta_set_text");

   procedure set_cursor_pos (Self : Textarea; arg2 : int16_t);  -- lv_ta.h:140
   pragma Import (C, set_cursor_pos, "lv_ta_set_cursor_pos");

   procedure set_cursor_type (Self : Textarea; arg2 : lv_cursor_type_t);  -- lv_ta.h:147
   pragma Import (C, set_cursor_type, "lv_ta_set_cursor_type");

   procedure set_pwd_mode (Self : Textarea; arg2 : u_Bool);  -- lv_ta.h:154
   pragma Import (C, set_pwd_mode, "lv_ta_set_pwd_mode");

   procedure set_one_line (Self : Textarea; arg2 : u_Bool);  -- lv_ta.h:161
   pragma Import (C, set_one_line, "lv_ta_set_one_line");

   procedure set_text_align (Self : Textarea; arg2 : Label.lv_label_align_t);  -- lv_ta.h:170
   pragma Import (C, set_text_align, "lv_ta_set_text_align");

   procedure set_accepted_chars (Self : Textarea; arg2 : Interfaces.C.Strings.chars_ptr);  -- lv_ta.h:177
   pragma Import (C, set_accepted_chars, "lv_ta_set_accepted_chars");

   procedure set_max_length (Self : Textarea; arg2 : uint16_t);  -- lv_ta.h:184
   pragma Import (C, set_max_length, "lv_ta_set_max_length");

   procedure set_action (Self : Textarea; action : lv_action_t);  -- lv_ta.h:191
   pragma Import (C, set_action, "lv_ta_set_action_inline");

   procedure set_sb_mode (Self : Textarea; mode : Page.lv_sb_mode_t);  -- lv_ta.h:201
   pragma Import (C, set_sb_mode, "lv_ta_set_sb_mode_inline");

   procedure set_style
     (Self : Textarea;
      arg2 : lv_ta_style_t;
      arg3 : LV.Style.Style);  -- lv_ta.h:212
   pragma Import (C, set_style, "lv_ta_set_style");

   function get_text (Self : Textarea) return Interfaces.C.Strings.chars_ptr;  -- lv_ta.h:223
   pragma Import (C, get_text, "lv_ta_get_text");

   function get_label (Self : Textarea) return Label.Instance;  -- lv_ta.h:230
   pragma Import (C, get_label, "lv_ta_get_label");

   function get_cursor_pos (Self : Textarea) return uint16_t;  -- lv_ta.h:237
   pragma Import (C, get_cursor_pos, "lv_ta_get_cursor_pos");

   function get_cursor_show (Self : Textarea) return u_Bool;  -- lv_ta.h:244
   pragma Import (C, get_cursor_show, "lv_ta_get_cursor_show");

   function get_cursor_type (Self : Textarea) return lv_cursor_type_t;  -- lv_ta.h:251
   pragma Import (C, get_cursor_type, "lv_ta_get_cursor_type");

   function get_pwd_mode (Self : Textarea) return u_Bool;  -- lv_ta.h:258
   pragma Import (C, get_pwd_mode, "lv_ta_get_pwd_mode");

   function get_one_line (Self : Textarea) return u_Bool;  -- lv_ta.h:265
   pragma Import (C, get_one_line, "lv_ta_get_one_line");

   function get_accepted_chars (Self : Textarea) return Interfaces.C.Strings.chars_ptr;  -- lv_ta.h:272
   pragma Import (C, get_accepted_chars, "lv_ta_get_accepted_chars");

   function get_max_length (Self : Textarea) return uint16_t;  -- lv_ta.h:279
   pragma Import (C, get_max_length, "lv_ta_get_max_length");

   function get_action (Self : Textarea) return lv_action_t;  -- lv_ta.h:286
   pragma Import (C, get_action, "lv_ta_get_action_inline");

   function get_sb_mode (Self : Textarea) return Page.lv_sb_mode_t;  -- lv_ta.h:296
   pragma Import (C, get_sb_mode, "lv_ta_get_sb_mode_inline");

   function get_style (Self : Textarea; arg2 : lv_ta_style_t) return LV.Style.Style;  -- lv_ta.h:307
   pragma Import (C, get_style, "lv_ta_get_style");

   procedure cursor_right (Self : Textarea);  -- lv_ta.h:317
   pragma Import (C, cursor_right, "lv_ta_cursor_right");

   procedure cursor_left (Self : Textarea);  -- lv_ta.h:323
   pragma Import (C, cursor_left, "lv_ta_cursor_left");

   procedure cursor_down (Self : Textarea);  -- lv_ta.h:329
   pragma Import (C, cursor_down, "lv_ta_cursor_down");

   procedure cursor_up (Self : Textarea);  -- lv_ta.h:335
   pragma Import (C, cursor_up, "lv_ta_cursor_up");

--  private
--
--     type lv_ta_ext_t;
--     type lv_ta_ext_t_cursor_struct is record
--        style : access LV.Style.lv_style_t;  -- lv_ta.h:68
--        valid_x : aliased lv_area_h.Coord_T;  -- lv_ta.h:69
--        pos : aliased uint16_t;  -- lv_ta.h:70
--        c_type : Extensions.Unsigned_4;  -- lv_ta.h:71
--        state : Extensions.Unsigned_1;  -- lv_ta.h:72
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_ta_ext_t_cursor_struct);
--     type lv_ta_ext_t is record
--        page : aliased lv_page_h.lv_page_ext_t;  -- lv_ta.h:59
--        label : access lv_obj_h.lv_obj_t;  -- lv_ta.h:61
--        pwd_tmp : Interfaces.C.Strings.chars_ptr;  -- lv_ta.h:62
--        accapted_chars : Interfaces.C.Strings.chars_ptr;  -- lv_ta.h:63
--        max_length : aliased sys_ustdint_h.uint16_t;  -- lv_ta.h:64
--        pwd_mode : Extensions.Unsigned_1;  -- lv_ta.h:65
--        one_line : Extensions.Unsigned_1;  -- lv_ta.h:66
--        cursor : aliased lv_ta_ext_t_cursor_struct;  -- lv_ta.h:73
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_ta_ext_t);
--     pragma Pack (lv_ta_ext_t);  -- lv_ta.h:74

end LV.Objx.Textarea;
