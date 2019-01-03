with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with LV.Style;

package LV.Objx.Checkbox is

   subtype Instance is Obj_T;

   subtype style_t is uint8_t;  -- lv_cb.h:62

   function create (Parent : Obj_T; Copy : Instance) return Instance;  -- lv_cb.h:74
   pragma Import (C, create, "lv_cb_create");

   procedure set_text (Self : Instance; arg2 : Interfaces.C.Strings.chars_ptr);  -- lv_cb.h:85
   pragma Import (C, set_text, "lv_cb_set_text");

   procedure set_checked (Self : Instance; checked : u_Bool);  -- lv_cb.h:92
   pragma Import (C, set_checked, "lv_cb_set_checked_inline");

   procedure set_inactive (Self : Instance);  -- lv_cb.h:101
   pragma Import (C, set_inactive, "lv_cb_set_inactive_inline");

   procedure set_action (Self : Instance; action : lv_action_t);  -- lv_cb.h:110
   pragma Import (C, set_action, "lv_cb_set_action_inline");

   procedure set_style
     (Self : Instance;
      arg2 : style_t;
      arg3 : access LV.Style.Style);  -- lv_cb.h:122
   pragma Import (C, set_style, "lv_cb_set_style");

   function get_text (Self : Instance) return Interfaces.C.Strings.chars_ptr;  -- lv_cb.h:133
   pragma Import (C, get_text, "lv_cb_get_text");

   function is_checked (Self : Instance) return u_Bool;  -- lv_cb.h:140
   pragma Import (C, is_checked, "lv_cb_is_checked_inline");

   function get_action (Self : Instance) return lv_action_t;  -- lv_cb.h:150
   pragma Import (C, get_action, "lv_cb_get_action_inline");

   function get_style (Self : Instance; arg2 : style_t) return access LV.Style.Style;  -- lv_cb.h:162
   pragma Import (C, get_style, "lv_cb_get_style");

--  private
--     type lv_cb_ext_t is record
--        bg_btn : aliased lv_btn_h.lv_btn_ext_t;  -- lv_cb.h:48
--        bullet : access lv_obj_h.lv_obj_t;  -- lv_cb.h:50
--        label : access lv_obj_h.lv_obj_t;  -- lv_cb.h:51
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_cb_ext_t);  -- lv_cb.h:52

end LV.Objx.Checkbox;
