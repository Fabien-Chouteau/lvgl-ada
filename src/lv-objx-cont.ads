pragma Style_Checks (Off);

with LV; use LV;

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions;
with LV.Style;
with LV.Area;

package LV.Objx.Cont is

   subtype Instance is Obj_T;

    type lv_layout_t is
     (LAYOUT_OFF,
      LAYOUT_CENTER,
      LAYOUT_COL_L,    --  Column left align
      LAYOUT_COL_M,    --  Column middle align
      LAYOUT_COL_R,    --  Column right align
      LAYOUT_ROW_T,    --  Row top align
      LAYOUT_ROW_M,    --  Row middle align
      LAYOUT_ROW_B,    --  Row bottom align
      LAYOUT_PRETTY,   --  Put as many object as possible in row and begin a new row
      LAYOUT_GRID);    --  Align same-sized object into a grid

   for lv_layout_t use
     (LAYOUT_OFF    => 0,
      LAYOUT_CENTER => 1,
      LAYOUT_COL_L  => 2,
      LAYOUT_COL_M  => 3,
      LAYOUT_COL_R  => 4,
      LAYOUT_ROW_T  => 5,
      LAYOUT_ROW_M  => 6,
      LAYOUT_ROW_B  => 7,
      LAYOUT_PRETTY => 8,
      LAYOUT_GRID   => 9);

   function create (Parent : Obj_T; Copy : Obj_T) return Instance;  -- lv_cont.h:70
   pragma Import (C, create, "lv_cont_create");

   procedure set_layout (Self : Instance; arg2 : lv_layout_t);  -- lv_cont.h:81
   pragma Import (C, set_layout, "lv_cont_set_layout");

   procedure set_fit
     (Self : Instance;
      arg2 : u_Bool;
      arg3 : u_Bool);  -- lv_cont.h:91
   pragma Import (C, set_fit, "lv_cont_set_fit");

   procedure set_style (Self : Instance; style : LV.Style.Style);  -- lv_cont.h:98
   pragma Import (C, set_style, "lv_cont_set_style_inline");

   function get_layout (Self : Instance) return lv_layout_t;  -- lv_cont.h:112
   pragma Import (C, get_layout, "lv_cont_get_layout");

   function get_hor_fit (Self : Instance) return u_Bool;  -- lv_cont.h:119
   pragma Import (C, get_hor_fit, "lv_cont_get_hor_fit");

   function get_ver_fit (Self : Instance) return u_Bool;  -- lv_cont.h:126
   pragma Import (C, get_ver_fit, "lv_cont_get_ver_fit");

   function get_fit_width (Self : Instance) return LV.Area.Coord_T;  -- lv_cont.h:134
   pragma Import (C, get_fit_width, "lv_cont_get_fit_width");

   function get_fit_height (Self : Instance) return LV.Area.Coord_T;  -- lv_cont.h:141
   pragma Import (C, get_fit_height, "lv_cont_get_fit_height");

   function get_style (Self : Instance) return LV.Style.Style;  -- lv_cont.h:148
   pragma Import (C, get_style, "lv_cont_get_style_inline");

--  private
--     type lv_cont_ext_t is record
--        layout : Extensions.Unsigned_4;  -- lv_cont.h:54
--        hor_fit : Extensions.Unsigned_1;  -- lv_cont.h:55
--        ver_fit : Extensions.Unsigned_1;  -- lv_cont.h:56
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_cont_ext_t);
--     pragma Pack (lv_cont_ext_t);  -- lv_cont.h:57

end LV.Objx.Cont;
