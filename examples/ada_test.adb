with System;

with LV.Style; use LV.Style;
with LV.Color; use LV.Color;
with LV.Area;

with LV;              use LV;
with LV.Objx;         use LV.Objx;
with LV.Objx.Tabview;
with LV.Objx.Page;
with LV.Objx.Textarea;
with LV.Objx.Keyboard;
with LV.Objx.Chart;
with LV.Objx.Slider;
with LV.Objx.List;
with LV.Objx.Mbox;

with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Ada_Test is

   LV_DPI     : constant := 100; --  FIXME: This should be in the conf
   LV_HOR_RES : constant := 800; --  FIXME: This should be in the conf
   LV_VER_RES : constant := 480; --  FIXME: This should be in the conf

   use type LV.Area.Coord_T;

   TA : Textarea.Textarea;
   KB : Keyboard.Instance := No_Obj;
   C  : Chart.Instance := No_Obj;

   function Keyboard_Hide_Action (KeyB : Keyboard.Instance) return lv_res_t
     with Convention => C;
   function Keyboard_Open_Close (Text_Area : Textarea.Textarea) return lv_res_t
     with Convention => C;

   function Keyboard_Hide_Action (KeyB : Keyboard.Instance) return lv_res_t
   is
      use type uint8_t;
   begin
      animate (KB, ANIM_FLOAT_BOTTOM or ANIM_OUT, 300, 0,
               Del'Access);
      KB := No_Obj;

      return RES_OK;
   end Keyboard_Hide_Action;

   function Keyboard_Open_Close (Text_Area : Textarea.Textarea) return lv_res_t
   is
      use type uint8_t;

      Parent : constant Obj_T := get_parent (get_parent (Text_Area));
   begin
      if KB /= No_Obj then
         return Keyboard_Hide_Action (KB);
      else
         KB := Keyboard.Create(Parent, No_Obj);
         Set_Size (KB,
                       Page.get_scrl_width (Parent),
                       get_height (Parent) / 2);

         align (KB, Text_Area, ALIGN_OUT_BOTTOM_MID, 0, 0);
         Keyboard.Set_Textarea (KB, Text_Area);

         --  FIXME: Styles

         Keyboard.set_hide_action (KB, Keyboard_Hide_Action'Access);
         Keyboard.set_ok_action (KB, Keyboard_Hide_Action'Access);

         animate (KB, ANIM_FLOAT_BOTTOM or ANIM_IN, 300, 0, null);

         return RES_OK;
      end if;
   end Keyboard_Open_Close;

   procedure Write_Create (Parent : Page.Instance)  is
--        style_ta  : aliased lv_style_t;
      Unused    : lv_res_t;
   begin
--        Page.set_style (Parent, Page.STYLE_BG, lv_style_transp_fit'Access);
--        Page.set_style (Parent, Page.STYLE_SCRL, lv_style_transp_fit'Access);
      Page.set_sb_mode (Parent, Page.SB_MODE_OFF);

--        lv_style_copy (style_ta'Access, lv_style_pretty'Access);
--        style_ta.c_body.opa := LV_OPA_30;
--        style_ta.c_body.radius := 0;
--        style_ta.text.color := LV_COLOR_GREEN;

      TA := Textarea.Create (Parent, No_Obj);
      Set_Size (TA, Page.get_scrl_width (Parent), get_height (Parent) / 2);
--        Textarea.set_style (TA, Textarea.STYLE_BG, style_ta'Access);
      Textarea.set_text (TA, Interfaces.C.Strings.New_String (""));
      Page.Set_Rel_Action (TA, Keyboard_Open_Close'Access);

      Unused := Keyboard_Open_Close (TA);
   end Write_Create;

   function Slider_Action (Slide : Obj_T) return lv_res_t
     with Convention => C;

   function Slider_Action (Slide : Obj_T) return lv_res_t is
      V : int16_t := Slider.get_value (Slide);
   begin
      V := int16_t (1000 * 100 / Natural (V)); --  Convert to range modify values linearly

      Chart.set_range (C, 0, V);

      return RES_OK;
   end Slider_Action;

   procedure Chart_Create (Parent : Page.Instance) is
      Ser : Chart.Series;
      Slide : Slider.Instance;
      Coord : aliased LV.Area.Area_T;

      Unused : lv_res_t;
   begin
      Page.set_scrl_fit (Parent, 0, 0);
      Page.set_scrl_height (Parent, get_height (Parent));
      Page.set_sb_mode (Parent, Page.SB_MODE_OFF);

      C := Chart.create (Parent, No_Obj);
      Set_Size (C, 2 * get_width (Parent) / 3, get_height (Parent) / 2);
      align (C, No_Obj, ALIGN_IN_TOP_MID, 0, LV_DPI / 4);
      Chart.set_type (C, Chart.TYPE_COLUMN);

      Chart.set_series_opa (C, OPA_70);
      Ser := Chart.add_series (C, COLOR_RED);
      Chart.set_next (C, Ser, 40);
      Chart.set_next (C, Ser, 30);
      Chart.set_next (C, Ser, 47);
      Chart.set_next (C, Ser, 59);
      Chart.set_next (C, Ser, 59);
      Chart.set_next (C, Ser, 31);
      Chart.set_next (C, Ser, 55);
      Chart.set_next (C, Ser, 70);
      Chart.set_next (C, Ser, 82);

      Slide := Slider.create (Parent, No_Obj);
      Set_Size (Slide, get_width (C), LV_DPI / 3);

      get_coords (C, Coord'Access);
      align (Slide, C, ALIGN_OUT_BOTTOM_MID, 0,
             (LV_VER_RES - Coord.y2 - get_height (Slide)) / 2);
      Slider.set_action (Slide, Slider_Action'Access);
      Slider.set_range (Slide, 10, 1000);
      Slider.set_value (Slide, 700);

      Unused := Slider_Action (Slide);
   end Chart_Create;

   function List_Btn_Action (Btn : Obj_T) return lv_res_t
     with Convention => C;

   function List_Btn_Action (Btn : Obj_T) return lv_res_t is
   begin
      Textarea.add_char (TA, Character'Pos (ASCII.CR));
      Textarea.add_text (TA, List.get_btn_text (Btn));
      return RES_OK;
   end List_Btn_Action;

   Btns : constant array (Natural range <>) of chars_ptr
     := (New_String ("Got it"),
         New_String (""))
       with Convention => C;

   procedure List_Create (Parent : Page.Instance)  is
      L : List.Instance;
      Unused : Obj_T;

      Box : Mbox.Instance;
   begin
      Page.set_scrl_fit (Parent, 0, 0);
      Page.set_scrl_height (Parent, get_height (Parent));
      Page.set_sb_mode (Parent, Page.SB_MODE_OFF);

      L := List.create (Parent, No_Obj);
      set_height (L, 2 * get_height (Parent) / 3);
      align (L, No_Obj, ALIGN_IN_TOP_MID, 0, LV_DPI / 4);

      Unused := List.add (L, System.Null_Address, New_String ("New"), List_Btn_Action'Access);
      Unused := List.add (L, System.Null_Address, New_String ("Open"), List_Btn_Action'Access);
      Unused := List.add (L, System.Null_Address, New_String ("Delete"), List_Btn_Action'Access);
      Unused := List.add (L, System.Null_Address, New_String ("Edit"), List_Btn_Action'Access);
      Unused := List.add (L, System.Null_Address, New_String ("Save"), List_Btn_Action'Access);
      Unused := List.add (L, System.Null_Address, New_String ("WiFi"), List_Btn_Action'Access);
      Unused := List.add (L, System.Null_Address, New_String ("GPS"), List_Btn_Action'Access);

      Box := Mbox.create (Parent, No_Obj);
      Mbox.set_text (Box, New_String ("Click a button to copy its text to the Text area "));
      set_width (Box, LV_HOR_RES - LV_DPI);
      Mbox.add_btns (Box, Btns'Address, null);

      align (Box, Parent, ALIGN_IN_TOP_MID, 0, LV_DPI / 2);
   end List_Create;

--     style_tv_btn_bg  : aliased lv_style_t;
--     style_tv_btn_rel : aliased lv_style_t;
--     style_tv_btn_pr  : aliased lv_style_t;

   TV : Tabview.Instance;

   procedure Ada_Demo_Create is
      Tab1      : Page.Instance;
      Tab2      : Page.Instance;
      Tab3      : Page.Instance;
      Tab4      : Page.Instance;
      A : aliased Area.Area_T;
      W : Area.Coord_T;
   begin

      Area.Set (A'Access, 0, 0, 0, 0);

      Area.Copy (A'Access, A'Access);
      W := Area.Get_Width (A'Access);

--        lv_style_copy (style_tv_btn_bg'Access, lv_style_plain'Access);
--        style_tv_btn_bg.c_body.main_color.full := 16#FF0000#;
--        style_tv_btn_bg.c_body.grad_color.full := 16#FF0000#;
--        style_tv_btn_bg.c_body.padding.ver := 0;
--
--        lv_style_copy (style_tv_btn_rel'Access, lv_style_btn_rel'Access);
--        style_tv_btn_bg.c_body.empty := 1;
--        style_tv_btn_bg.c_body.border.width := 0;
--
--        lv_style_copy(style_tv_btn_pr'Access, lv_style_btn_pr'Access);
--        style_tv_btn_pr.c_body.radius := 0;
--        style_tv_btn_pr.c_body.opa := LV_OPA_50;
--        style_tv_btn_pr.c_body.main_color := LV_COLOR_WHITE;
--        style_tv_btn_pr.c_body.grad_color := LV_COLOR_WHITE;
--        style_tv_btn_pr.c_body.border.width := 0;
--        style_tv_btn_pr.text.color := LV_COLOR_GRAY;

      TV := Tabview.Create (scr_act, No_Obj);

      Tab1 := Tabview.Add_Tab (TV, New_String ("Write"));
      Tab2 := Tabview.Add_Tab (TV, New_String ("List"));
      Tab3 := Tabview.Add_Tab (TV, New_String ("Chart"));
      Tab4 := Tabview.Add_Tab (TV, New_String ("Test"));

      --     lv_tabview_h.Set_Style (TV, lv_tabview_h.LV_TABVIEW_STYLE_BG, style_tv_btn_bg'Access);

--        Tabview.Set_Style (TV, Tabview.STYLE_BG, style_tv_btn_bg'Access);
--        Tabview.Set_Style (TV, Tabview.STYLE_BTN_BG, style_tv_btn_bg'Access);
--        Tabview.Set_Style (TV, Tabview.STYLE_INDIC, lv_style_plain'Access);
--        Tabview.Set_Style (TV, Tabview.STYLE_BTN_REL, style_tv_btn_rel'Access);
--        Tabview.Set_Style (TV, Tabview.STYLE_BTN_PR, style_tv_btn_pr'Access);
--        Tabview.Set_Style (TV, Tabview.STYLE_BTN_TGL_REL, style_tv_btn_rel'Access);
--        Tabview.Set_Style (TV, Tabview.STYLE_BTN_TGL_PR, style_tv_btn_pr'Access);
--
      Write_Create (Tab1);
      List_Create (Tab2);
      Chart_Create (Tab3);
   end Ada_Demo_Create;

end Ada_Test;
