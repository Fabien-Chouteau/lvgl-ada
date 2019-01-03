with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions;
with Interfaces.C.Strings;
with LV.Style;
with LV.Area;
with LV.Objx.Page;

package LV.Objx.Ddlist is

   subtype Instance is Obj_T;

   subtype style_t is uint8_t;

   function create (Parent : Obj_T; Copy : Instance) return Instance;
   pragma Import (C, create, "lv_ddlist_create");

   procedure set_options (Self : Instance; Options : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, set_options, "lv_ddlist_set_options");

   procedure set_selected (Self : Instance; Sel_Opt : uint16_t);  -- lv_ddlist.h:94
   pragma Import (C, set_selected, "lv_ddlist_set_selected");

   procedure set_action (Self : Instance; Action : lv_action_t);  -- lv_ddlist.h:101
   pragma Import (C, set_action, "lv_ddlist_set_action");

   procedure set_fix_height (Self : Instance; H : LV.Area.Coord_T);  -- lv_ddlist.h:109
   pragma Import (C, set_fix_height, "lv_ddlist_set_fix_height");

   procedure set_hor_fit (Self : Instance; Fit_En : u_Bool);  -- lv_ddlist.h:116
   pragma Import (C, set_hor_fit, "lv_ddlist_set_hor_fit");

   procedure set_sb_mode (Self : Instance; mode : LV.Objx.Page.lv_sb_mode_t);  -- lv_ddlist.h:123
   pragma Import (C, set_sb_mode, "lv_ddlist_set_sb_mode_inline");

   procedure set_anim_time (Self : Instance; Anim_Time : uint16_t);  -- lv_ddlist.h:133
   pragma Import (C, set_anim_time, "lv_ddlist_set_anim_time");

   procedure set_style
     (Self : Instance;
      arg2 : style_t;
      arg3 : access LV.Style.Style);  -- lv_ddlist.h:142
   pragma Import (C, set_style, "lv_ddlist_set_style");

   function get_options (Self : Instance) return Interfaces.C.Strings.chars_ptr;  -- lv_ddlist.h:153
   pragma Import (C, get_options, "lv_ddlist_get_options");

   function get_selected (Self : Instance) return uint16_t;  -- lv_ddlist.h:160
   pragma Import (C, get_selected, "lv_ddlist_get_selected");

   procedure get_selected_str (Self : Instance; arg2 : Interfaces.C.Strings.chars_ptr);  -- lv_ddlist.h:167
   pragma Import (C, get_selected_str, "lv_ddlist_get_selected_str");

   function get_action (Self : Instance) return lv_action_t;  -- lv_ddlist.h:174
   pragma Import (C, get_action, "lv_ddlist_get_action");

   function get_fix_height (Self : Instance) return LV.Area.Coord_T;  -- lv_ddlist.h:181
   pragma Import (C, get_fix_height, "lv_ddlist_get_fix_height");

   function get_sb_mode (Self : Instance) return LV.Objx.Page.lv_sb_mode_t;  -- lv_ddlist.h:188
   pragma Import (C, get_sb_mode, "lv_ddlist_get_sb_mode_inline");

   function get_anim_time (Self : Instance) return uint16_t;  -- lv_ddlist.h:198
   pragma Import (C, get_anim_time, "lv_ddlist_get_anim_time");

   function get_style (Self : Instance; arg2 : style_t) return access LV.Style.Style;  -- lv_ddlist.h:206
   pragma Import (C, get_style, "lv_ddlist_get_style");

   procedure open (Self : Instance; arg2 : u_Bool);  -- lv_ddlist.h:217
   pragma Import (C, open, "lv_ddlist_open");

   procedure close (Self : Instance; arg2 : u_Bool);  -- lv_ddlist.h:224
   pragma Import (C, close, "lv_ddlist_close");

--  private
--     type lv_ddlist_ext_t is record
--        page : aliased lv_page_h.lv_page_ext_t;  -- lv_ddlist.h:47
--        label : access lv_obj_h.lv_obj_t;  -- lv_ddlist.h:49
--        sel_style : access lv_style_h.lv_style_t;  -- lv_ddlist.h:50
--        action : lv_obj_h.lv_action_t;  -- lv_ddlist.h:51
--        option_cnt : aliased uint16_t;  -- lv_ddlist.h:52
--        sel_opt_id : aliased sys_ustdint_h.uint16_t;  -- lv_ddlist.h:53
--        sel_opt_id_ori : aliased sys_ustdint_h.uint16_t;  -- lv_ddlist.h:54
--        anim_time : aliased sys_ustdint_h.uint16_t;  -- lv_ddlist.h:55
--        opened : Extensions.Unsigned_1;  -- lv_ddlist.h:56
--        fix_height : aliased lv_area_h.Coord_T;  -- lv_ddlist.h:57
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_ddlist_ext_t);
--     pragma Pack (lv_ddlist_ext_t);  -- lv_ddlist.h:58

end LV.Objx.Ddlist;
