pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with LV.Style;
with Interfaces.C.Extensions;

with LV.Objx.Cont;

package LV.Objx.Btn is

   subtype Instance is Obj_T;

   type state_t is
     (STATE_REL,
      STATE_PR,
      STATE_TGL_REL,
      STATE_TGL_PR,
      STATE_INA,
      STATE_NUM);

   for state_t use
     (STATE_REL     => 0,
      STATE_PR      => 1,
      STATE_TGL_REL => 2,
      STATE_TGL_PR  => 3,
      STATE_INA     => 4,
      STATE_NUM     => 5);

   type action_t is
     (ACTION_CLICK,
      ACTION_PR,
      ACTION_LONG_PR,
      ACTION_LONG_PR_REPEAT,
      ACTION_NUM);

   for action_t use
     (ACTION_CLICK          => 0,
      ACTION_PR             => 1,
      ACTION_LONG_PR        => 2,
      ACTION_LONG_PR_REPEAT => 3,
      ACTION_NUM            => 4);

   subtype style_t is uint8_t;  -- lv_btn.h:89

   function create (Parent : Obj_T; Copy : Obj_T) return Instance;  -- lv_btn.h:101
   pragma Import (C, create, "lv_btn_create");

   procedure set_toggle (Self : Instance; arg2 : u_Bool);  -- lv_btn.h:112
   pragma Import (C, set_toggle, "lv_btn_set_toggle");

   procedure set_state (Self : Instance; arg2 : state_t);  -- lv_btn.h:119
   pragma Import (C, set_state, "lv_btn_set_state");

   procedure toggle (Self : Instance);  -- lv_btn.h:125
   pragma Import (C, toggle, "lv_btn_toggle");

   procedure set_action
     (Self : Instance;
      arg2 : action_t;
      arg3 : lv_action_t);  -- lv_btn.h:132
   pragma Import (C, set_action, "lv_btn_set_action");

   procedure set_layout (Self : Instance; layout : LV.Objx.Cont.lv_layout_t);  -- lv_btn.h:139
   pragma Import (C, set_layout, "lv_btn_set_layout_inline");

   procedure set_fit
     (Self : Instance;
      hor_en : u_Bool;
      ver_en : u_Bool);  -- lv_btn.h:151
   pragma Import (C, set_fit, "lv_btn_set_fit_inline");

   procedure set_ink_in_time (Self : Instance; arg2 : uint16_t);  -- lv_btn.h:161
   pragma Import (C, set_ink_in_time, "lv_btn_set_ink_in_time");

   procedure set_ink_wait_time (Self : Instance; arg2 : uint16_t);  -- lv_btn.h:168
   pragma Import (C, set_ink_wait_time, "lv_btn_set_ink_wait_time");

   procedure set_ink_out_time (Self : Instance; arg2 : uint16_t);  -- lv_btn.h:175
   pragma Import (C, set_ink_out_time, "lv_btn_set_ink_out_time");

   procedure set_style
     (Self : Instance;
      arg2 : style_t;
      arg3 : access LV.Style.Style);  -- lv_btn.h:183
   pragma Import (C, set_style, "lv_btn_set_style");

   function get_state (Self : Instance) return state_t;  -- lv_btn.h:194
   pragma Import (C, get_state, "lv_btn_get_state");

   function get_toggle (Self : Instance) return u_Bool;  -- lv_btn.h:201
   pragma Import (C, get_toggle, "lv_btn_get_toggle");

   function get_action (Self : Instance; arg2 : action_t) return lv_action_t;  -- lv_btn.h:208
   pragma Import (C, get_action, "lv_btn_get_action");

   function get_layout (Self : Instance) return LV.Objx.Cont.lv_layout_t;  -- lv_btn.h:215
   pragma Import (C, get_layout, "lv_btn_get_layout_inline");

   function get_hor_fit (Self : Instance) return u_Bool;  -- lv_btn.h:225
   pragma Import (C, get_hor_fit, "lv_btn_get_hor_fit_inline");

   function get_ver_fit (Self : Instance) return u_Bool;  -- lv_btn.h:235
   pragma Import (C, get_ver_fit, "lv_btn_get_ver_fit_inline");

   function get_ink_in_time (Self : Instance) return uint16_t;  -- lv_btn.h:245
   pragma Import (C, get_ink_in_time, "lv_btn_get_ink_in_time");

   function get_ink_wait_time (Self : Instance) return uint16_t;  -- lv_btn.h:252
   pragma Import (C, get_ink_wait_time, "lv_btn_get_ink_wait_time");

   function get_ink_out_time (Self : Instance) return uint16_t;  -- lv_btn.h:259
   pragma Import (C, get_ink_out_time, "lv_btn_get_ink_out_time");

   function get_style (Self : Instance; arg2 : style_t) return access LV.Style.Style;  -- lv_btn.h:267
   pragma Import (C, get_style, "lv_btn_get_style");

--  private
--     type lv_btn_ext_t_actions_array is array (0 .. 3) of lv_obj_h.lv_action_t;
--     type lv_btn_ext_t_styles_array is array (0 .. 4) of access lv_style_h.lv_style_t;
--     type lv_btn_ext_t is record
--        cont : aliased lv_cont_h.lv_cont_ext_t;  -- lv_btn.h:67
--        actions : lv_btn_ext_t_actions_array;  -- lv_btn.h:69
--        styles : lv_btn_ext_t_styles_array;  -- lv_btn.h:70
--        state : aliased lv_btn_state_t;  -- lv_btn.h:71
--        ink_in_time : aliased uint16_t;  -- lv_btn.h:73
--        ink_wait_time : aliased sys_ustdint_h.uint16_t;  -- lv_btn.h:74
--        ink_out_time : aliased sys_ustdint_h.uint16_t;  -- lv_btn.h:75
--        toggle : Extensions.Unsigned_1;  -- lv_btn.h:77
--        long_pr_action_executed : Extensions.Unsigned_1;  -- lv_btn.h:78
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_btn_ext_t);
--     pragma Pack (lv_btn_ext_t);  -- lv_btn.h:79

end LV.Objx.Btn;
