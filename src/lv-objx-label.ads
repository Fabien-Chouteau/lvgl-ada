pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with LV.Area;
with Interfaces.C.Extensions;
with LV.Style;

package LV.Objx.Label is

   subtype Instance is Obj_T;

   LV_LABEL_DOT_NUM : constant := 3;  --  lv_label.h:32
   LV_LABEL_POS_LAST : constant := 16#FFFF#;  --  lv_label.h:33

   subtype lv_label_long_mode_t is uint8_t;  -- lv_label.h:49

   subtype lv_label_align_t is uint8_t;  -- lv_label.h:57

   function create (Par : Obj_T; Copy : Obj_T) return Instance;  -- lv_label.h:92
   pragma Import (C, create, "lv_label_create");

   procedure set_text (Self : Instance; arg2 : Interfaces.C.Strings.chars_ptr);  -- lv_label.h:103
   pragma Import (C, set_text, "lv_label_set_text");

   procedure set_array_text
     (Self : Instance;
      arg2 : Interfaces.C.Strings.chars_ptr;
      arg3 : uint16_t);  -- lv_label.h:112
   pragma Import (C, set_array_text, "lv_label_set_array_text");

   procedure set_static_text (Self : Instance; arg2 : Interfaces.C.Strings.chars_ptr);  -- lv_label.h:120
   pragma Import (C, set_static_text, "lv_label_set_static_text");

   procedure set_long_mode (Self : Instance; arg2 : lv_label_long_mode_t);  -- lv_label.h:128
   pragma Import (C, set_long_mode, "lv_label_set_long_mode");

   procedure set_align (Self : Instance; arg2 : lv_label_align_t);  -- lv_label.h:135
   pragma Import (C, set_align, "lv_label_set_align");

   procedure set_recolor (Self : Instance; arg2 : u_Bool);  -- lv_label.h:142
   pragma Import (C, set_recolor, "lv_label_set_recolor");

   procedure set_body_draw (Self : Instance; arg2 : u_Bool);  -- lv_label.h:149
   pragma Import (C, set_body_draw, "lv_label_set_body_draw");

   procedure set_anim_speed (Self : Instance; arg2 : uint16_t);  -- lv_label.h:156
   pragma Import (C, set_anim_speed, "lv_label_set_anim_speed");

   procedure set_style (Self : Instance; style : LV.Style.Style);  -- lv_label.h:163
   pragma Import (C, set_style, "lv_label_set_style_inline");

   function get_text (Self : Instance) return Interfaces.C.Strings.chars_ptr;  -- lv_label.h:176
   pragma Import (C, get_text, "lv_label_get_text");

   function get_long_mode (Self : Instance) return lv_label_long_mode_t;  -- lv_label.h:183
   pragma Import (C, get_long_mode, "lv_label_get_long_mode");

   function get_align (Self : Instance) return lv_label_align_t;  -- lv_label.h:190
   pragma Import (C, get_align, "lv_label_get_align");

   function get_recolor (Self : Instance) return u_Bool;  -- lv_label.h:197
   pragma Import (C, get_recolor, "lv_label_get_recolor");

   function get_body_draw (Self : Instance) return u_Bool;  -- lv_label.h:204
   pragma Import (C, get_body_draw, "lv_label_get_body_draw");

   function get_anim_speed (Self : Instance) return uint16_t;  -- lv_label.h:211
   pragma Import (C, get_anim_speed, "lv_label_get_anim_speed");

   procedure get_letter_pos
     (Self : Instance;
      arg2 : uint16_t;
      arg3 : access LV.Area.Point_T);  -- lv_label.h:219
   pragma Import (C, get_letter_pos, "lv_label_get_letter_pos");

   function get_letter_on (Self : Instance; arg2 : access LV.Area.Point_T) return uint16_t;  -- lv_label.h:228
   pragma Import (C, get_letter_on, "lv_label_get_letter_on");

   function get_style (Self : Instance) return LV.Style.Style;  -- lv_label.h:235
   pragma Import (C, get_style, "lv_label_get_style_inline");

   procedure ins_text
     (Self : Instance;
      arg2 : uint32_t;
      arg3 : Interfaces.C.Strings.chars_ptr);  -- lv_label.h:252
   pragma Import (C, ins_text, "lv_label_ins_text");

   procedure cut_text
     (Self : Instance;
      arg2 : uint32_t;
      arg3 : uint32_t);  -- lv_label.h:261
   pragma Import (C, cut_text, "lv_label_cut_text");

--  private
--
--     subtype lv_label_ext_t_dot_tmp_array is Interfaces.C.char_array (0 .. 12);
--     type lv_label_ext_t is record
--        text : Interfaces.C.Strings.chars_ptr;  -- lv_label.h:64
--        long_mode : aliased long_mode_t;  -- lv_label.h:65
--        dot_tmp : aliased lv_label_ext_t_dot_tmp_array;  -- lv_label.h:69
--        dot_end : aliased uint16_t;  -- lv_label.h:71
--        anim_speed : aliased sys_ustdint_h.uint16_t;  -- lv_label.h:72
--        offset : aliased LV.Area.Point_T;  -- lv_label.h:73
--        static_txt : Extensions.Unsigned_1;  -- lv_label.h:74
--        align : Extensions.Unsigned_2;  -- lv_label.h:75
--        recolor : Extensions.Unsigned_1;  -- lv_label.h:76
--        expand : Extensions.Unsigned_1;  -- lv_label.h:77
--        body_draw : Extensions.Unsigned_1;  -- lv_label.h:78
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_label_ext_t);
--     pragma Pack (lv_label_ext_t);  -- lv_label.h:79

end LV.Objx.Label;
