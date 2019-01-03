pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with LV.Style;
with LV.Area;
with Interfaces.C.Extensions;

with LV.Objx.Cont;

package LV.Objx.Page is

   subtype Instance is Obj_T;

   --  Scrollbar modes: shows when should the scrollbars be visible
   type lv_sb_mode_t is
   (SB_MODE_OFF,   --  Never show scrollbars
    SB_MODE_ON,    --  Always show scrollbars
    SB_MODE_DRAG,  --  Show scrollbars when page is being dragged
    SB_MODE_AUTO,  --  Show scrollbars when the scrollable container is large enough to be scrolled
    SB_MODE_HIDE,  --  Hide the scroll bar temporally
    SB_MODE_UNHIDE --  Unhide the previously hidden scrollbar. Recover it's type too
   );
   --  subtype lv_sb_mode_t is sys_ustdint_h.uint8_t;  -- lv_page.h:50

   for lv_sb_mode_t use
   (SB_MODE_OFF    => 0,
    SB_MODE_ON     => 1,
    SB_MODE_DRAG   => 2,
    SB_MODE_AUTO   => 3,
    SB_MODE_HIDE   => 4,
    SB_MODE_UNHIDE => 5);

   type lv_page_style_t is
   (STYLE_BG,
    STYLE_SCRL,
    STYLE_SB)
   with Size => 8;
   --  subtype lv_page_style_t is sys_ustdint_h.uint8_t;  -- lv_page.h:76

   for lv_page_style_t use
   (STYLE_BG => 0,
    STYLE_SCRL => 1,
    STYLE_SB => 2);

   function Create (Par : Obj_T; Copy : Obj_T) return Instance;  -- lv_page.h:88
   pragma Import (C, Create, "lv_page_create");

   procedure Clean (Self : Instance);  -- lv_page.h:94
   pragma Import (C, Clean, "lv_page_clean");

   function Get_Pr_Action (Self : Instance) return lv_action_t;  -- lv_page.h:101
   pragma Import (C, Get_Pr_Action, "lv_page_get_pr_action");

   function Get_Rel_Action (Self : Instance) return lv_action_t;  -- lv_page.h:108
   pragma Import (C, Get_Rel_Action, "lv_page_get_rel_action");

   function Get_Scrl (arg1 : Instance) return Obj_T;  -- lv_page.h:115
   pragma Import (C, Get_Scrl, "lv_page_get_scrl");

   procedure Set_Rel_Action (Self : Instance; Rel_Action : lv_action_t);  -- lv_page.h:126
   pragma Import (C, Set_Rel_Action, "lv_page_set_rel_action");

   procedure set_pr_action (Self : Instance; PR_Action : lv_action_t);  -- lv_page.h:133
   pragma Import (C, set_pr_action, "lv_page_set_pr_action");

   procedure set_sb_mode (Self : Instance; Sb_Mode : lv_sb_mode_t);  -- lv_page.h:140
   pragma Import (C, set_sb_mode, "lv_page_set_sb_mode");

   procedure set_arrow_scroll (Self : Instance; En : u_Bool);  -- lv_page.h:147
   pragma Import (C, set_arrow_scroll, "lv_page_set_arrow_scroll");

   procedure set_scrl_fit
     (Self   : Instance;
      hor_en : u_Bool;
      ver_en : u_Bool);  -- lv_page.h:157
   pragma Import (C, set_scrl_fit, "lv_page_set_scrl_fit_inline");

   procedure set_scrl_width (Self : Instance; w : LV.Area.Coord_T);  -- lv_page.h:167
   pragma Import (C, set_scrl_width, "lv_page_set_scrl_width_inline");

   procedure set_scrl_height (Self : Instance; h : LV.Area.Coord_T);  -- lv_page.h:177
   pragma Import (C, set_scrl_height, "lv_page_set_scrl_height_inline");

   procedure set_scrl_layout (Self : Instance; layout : LV.Objx.Cont.lv_layout_t);  -- lv_page.h:188
   pragma Import (C, set_scrl_layout, "lv_page_set_scrl_layout_inline");

   procedure set_style
     (Self  : Instance;
      Typ   : lv_page_style_t;
      Style : LV.Style.Style);  -- lv_page.h:199
   pragma Import (C, set_style, "lv_page_set_style");

   function get_sb_mode (Self : Instance) return lv_sb_mode_t;  -- lv_page.h:210
   pragma Import (C, get_sb_mode, "lv_page_get_sb_mode");

   function get_arrow_scroll (Self : Instance) return u_Bool;  -- lv_page.h:218
   pragma Import (C, get_arrow_scroll, "lv_page_get_arrow_scroll");

   function get_fit_width (Self : Instance) return LV.Area.Coord_T;  -- lv_page.h:226
   pragma Import (C, get_fit_width, "lv_page_get_fit_width");

   function get_fit_height (Self : Instance) return LV.Area.Coord_T;  -- lv_page.h:233
   pragma Import (C, get_fit_height, "lv_page_get_fit_height");

   function get_scrl_width (Self : Instance) return LV.Area.Coord_T;  -- lv_page.h:240
   pragma Import (C, get_scrl_width, "lv_page_get_scrl_width_inline");

   function get_scrl_height (Self : Instance) return LV.Area.Coord_T;  -- lv_page.h:250
   pragma Import (C, get_scrl_height, "lv_page_get_scrl_height_inline");

   function get_scrl_layout (Self : Instance) return LV.Objx.Cont.lv_layout_t;  -- lv_page.h:260
   pragma Import (C, get_scrl_layout, "lv_page_get_scrl_layout_inline");

   function get_scrl_hor_fit (Self : Instance) return u_Bool;  -- lv_page.h:270
   pragma Import (C, get_scrl_hor_fit, "lv_page_get_scrl_hor_fit_inline");

   function get_scrl_fit_ver (Self : Instance) return u_Bool;  -- lv_page.h:280
   pragma Import (C, get_scrl_fit_ver, "lv_page_get_scrl_fit_ver_inline");

   function get_style (Self : Instance; arg2 : lv_page_style_t) return LV.Style.Style;  -- lv_page.h:291
   pragma Import (C, get_style, "lv_page_get_style");

   procedure glue_obj (Self : Obj_T; Glue : u_Bool);  -- lv_page.h:302
   pragma Import (C, glue_obj, "lv_page_glue_obj");

   procedure focus
     (Self      : Instance;
      Ob        : Obj_T;
      Anim_Time : uint16_t);  -- lv_page.h:310
   pragma Import (C, focus, "lv_page_focus");

   procedure scroll_hor (Self : Instance; Dist : LV.Area.Coord_T);  -- lv_page.h:317
   pragma Import (C, scroll_hor, "lv_page_scroll_hor");

   procedure scroll_ver (Self : Instance; Dist : LV.Area.Coord_T);  -- lv_page.h:324
   pragma Import (C, scroll_ver, "lv_page_scroll_ver");

--  private
--
--     type lv_page_ext_t;
--     type lv_page_ext_t_sb_struct is record
--        style : access LV.Style.lv_style_t;  -- lv_page.h:61
--        hor_area : aliased LV.Area.lv_area_t;  -- lv_page.h:62
--        ver_area : aliased LV.Area.lv_area_t;  -- lv_page.h:63
--        hor_draw : Extensions.Unsigned_1;  -- lv_page.h:64
--        ver_draw : Extensions.Unsigned_1;  -- lv_page.h:65
--        mode : Extensions.Unsigned_3;  -- lv_page.h:66
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_page_ext_t_sb_struct);
--     type lv_page_ext_t is record
--        bg : aliased lv_cont_h.lv_cont_ext_t;  -- lv_page.h:55
--        scrl : Obj_T;  -- lv_page.h:57
--        rel_action : lv_action_t;  -- lv_page.h:58
--        pr_action : lv_action_t;  -- lv_page.h:59
--        sb : aliased lv_page_ext_t_sb_struct;  -- lv_page.h:67
--        arrow_scroll : Extensions.Unsigned_1;  -- lv_page.h:68
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_page_ext_t);
--     pragma Pack (lv_page_ext_t);  -- lv_page.h:69

end LV.Objx.Page;
