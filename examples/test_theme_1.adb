with Ada.Text_IO;

with LV;      use LV;
with LV.Area;
with LV.Objx; use LV.Objx;
with LV.Objx.Cont;
with LV.Objx.Tabview;
with LV.Objx.Page;
with LV.Objx.Btn;
with LV.Objx.Btnm;
with LV.Objx.Label;
with LV.Objx.Switch;
with LV.Objx.Bar;
with LV.Objx.Slider;
with LV.Objx.Line;
with LV.Objx.Textarea;
with LV.Objx.Checkbox;
with LV.Objx.Ddlist;
with LV.Objx.List;
with LV.Objx.Roller;
with LV.Objx.Chart;
with LV.Objx.Gauge;
with LV.Objx.Arc;
with LV.Objx.Keyboard;
with LV.Objx.Preload;
with LV.Objx.Win;
with LV.Objx.Lmeter;
with LV.Objx.LED;
with LV.Objx.Calendar;
with LV.Objx.Mbox;

with Interfaces.C.Strings; use Interfaces.C.Strings;
with System;
with LV.Color;

with LV.HAL.Disp;
with Lv.Theme;
with Lv.Font;

package body Test_Theme_1 is

   use type int16_t;

   LV_DPI     : constant := 100; --  FIXME: This should be in the conf
   LV_HOR_RES : constant := 800; --  FIXME: This should be in the conf
   LV_VER_RES : constant := 480; --  FIXME: This should be in the conf

   subtype C is Character;
   SYMBOL_AUDIO      : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#80#);
   SYMBOL_VIDEO      : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#81#);
   SYMBOL_LIST       : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#82#);
   SYMBOL_OK         : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#83#);
   SYMBOL_CLOSE      : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#84#);
   SYMBOL_POWER      : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#85#);
   SYMBOL_SETTINGS   : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#86#);
   SYMBOL_TRASH      : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#87#);
   SYMBOL_HOME       : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#88#);
   SYMBOL_DOWNLOAD   : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#89#);
   SYMBOL_DRIVE      : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#8A#);
   SYMBOL_REFRESH    : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#8B#);
   SYMBOL_MUTE       : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#8C#);
   SYMBOL_VOLUME_MID : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#8D#);
   SYMBOL_VOLUME_MAX : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#8E#);
   SYMBOL_IMAGE      : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#8F#);
   SYMBOL_EDIT       : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#90#);
   SYMBOL_PREV       : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#91#);
   SYMBOL_PLAY       : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#92#);
   SYMBOL_PAUSE      : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#93#);
   SYMBOL_STOP       : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#94#);
   SYMBOL_NEXT       : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#95#);
   SYMBOL_EJECT      : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#96#);
   SYMBOL_LEFT       : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#97#);
   SYMBOL_RIGHT      : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#98#);
   SYMBOL_PLUS       : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#99#);
   SYMBOL_MINUS      : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#9A#);
   SYMBOL_WARNING    : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#9B#);
   SYMBOL_SHUFFLE    : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#9C#);
   SYMBOL_UP         : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#9D#);
   SYMBOL_DOWN       : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#9E#);
   SYMBOL_LOOP       : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#9F#);
   SYMBOL_DIRECTORY  : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#A0#);
   SYMBOL_UPLOAD     : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#A1#);
   SYMBOL_CALL       : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#A2#);
   SYMBOL_CUT        : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#A3#);
   SYMBOL_COPY       : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#A4#);
   SYMBOL_SAVE       : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#A5#);
   SYMBOL_CHARGE     : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#A6#);
   SYMBOL_BELL       : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#A7#);
   SYMBOL_KEYBOARD   : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#A8#);
   SYMBOL_GPS        : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#A9#);
   SYMBOL_FILE       : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#AA#);
   SYMBOL_WIFI       : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#AB#);
   SYMBOL_BATTERY_FUL: constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#AC#);
   SYMBOL_BATTERY_3  : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#AD#);
   SYMBOL_BATTERY_2  : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#AE#);
   SYMBOL_BATTERY_1  : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#AF#);
   SYMBOL_BATTERY_EMP: constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#B0#);
   SYMBOL_BLUETOOTH  : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#B1#);

   Btnm_Str : constant array (Natural range <>) of chars_ptr
     := (New_String ("1"),
         New_String ("2"),
         New_String ("3"),
         New_String (SYMBOL_OK),
         New_String (SYMBOL_CLOSE),
         New_String (""))
       with Convention => C;

   Points : constant array (Natural range <>) of LV.Area.Point_T
     := ((0, 0), (LV_HOR_RES / 5, 0))
       with Convention => C;

   Theme_Roller : Roller.Instance;

   procedure Create_Theme_Tab (Parent : Page.Instance);
   procedure Create_Tab1 (Parent : Page.Instance);
   procedure Create_Tab2 (Parent : Page.Instance);
   procedure Create_Tab3 (Parent : Page.Instance);

   procedure Init_Themes (Hue : Lv.Theme.Hue_T);

   function Roller_Action (Arg1 : Obj_T) return Lv_Res_T
     with Convention => C;

   function Slider_Action (Arg1 : Obj_T) return Lv_Res_T
     with Convention => C;

   procedure Init_Themes (Hue : Lv.Theme.Hue_T) is
      Unused : Lv.Theme.Theme;
   begin
      Unused := Lv.Theme.Default_Init (Hue, Lv.Font.No_Font);
      Unused := Lv.Theme.Material_Init (Hue, Lv.Font.No_Font);
      Unused := Lv.Theme.Mono_Init (Hue, Lv.Font.No_Font);
      Unused := Lv.Theme.Alien_Init (Hue, Lv.Font.No_Font);
      Unused := Lv.Theme.Nemo_Init (Hue, Lv.Font.No_Font);
      Unused := Lv.Theme.Night_Init (Hue, Lv.Font.No_Font);
      Unused := Lv.Theme.Zen_Init (Hue, Lv.Font.No_Font);
   end Init_Themes;

   function Roller_Action (Arg1 : Obj_T) return Lv_Res_T is
   begin
      case Roller.Get_Selected (Arg1) is
         when 0       => Lv.Theme.Set_Current (Lv.Theme.Get_Default);
         when 1       => Lv.Theme.Set_Current (Lv.Theme.Get_Material);
         when 2       => Lv.Theme.Set_Current (Lv.Theme.Get_Mono);
         when 3       => Lv.Theme.Set_Current (Lv.Theme.Get_Alien);
         when 4       => Lv.Theme.Set_Current (Lv.Theme.Get_Night);
         when 5       => Lv.Theme.Set_Current (Lv.Theme.Get_Zen);
         when others  => Lv.Theme.Set_Current (Lv.Theme.Get_Nemo);
      end case;

      return Res_Ok;
   end Roller_Action;

   function Slider_Action (Arg1 : Obj_T) return Lv_Res_T is
   begin
      Init_Themes (Uint16_T (Slider.Get_Value (Arg1)));
      return Roller_Action (Theme_Roller);
   end Slider_Action;

   procedure Create_Theme_Tab (Parent : Page.Instance) is
      TH  : constant Theme.Theme := Theme.Get_Current;
      pragma Unreferenced (TH);
      Slide : Slider.Instance;
   begin
      Page.Set_Scrl_Layout (Parent, Cont.Layout_Pretty);

      Theme_Roller := Roller.Create (Parent, No_Obj);
      Roller.Set_Options (Theme_Roller, New_String ("Default" & ASCII.LF &
                                           "Material" & ASCII.LF &
                                           "Mono" & ASCII.LF &
                                           "Alien" & ASCII.LF &
                                           "Night" & ASCII.LF &
                                           "Zen" & ASCII.LF &
                                           "Nemo"));
      Roller.Set_Selected (Theme_Roller, 4, 0);
      Roller.Set_Visible_Row_Count (Theme_Roller, 3);
      Roller.Set_Action (Theme_Roller, Roller_Action'Access);

      Slide := Slider.Create (Parent, No_Obj);
      Slider.Set_Action (Slide, Slider_Action'Access);
      Slider.Set_Range (Slide, 0, 360);
      Slider.Set_Value (Slide, 70);

   end Create_Theme_Tab;

   procedure Create_Tab1 (Parent : Page.Instance) is
      TH  : constant Theme.Theme :=  Theme.Get_current;
      H   : Cont.Instance;
      B   : Btn.Instance;
      Lab : Label.Instance;

      BM    : Btnm.Instance;
      Sw_H  : Cont.Instance;
      S     : Switch.Instance;
      Ba    : Bar.Instance;
      Slide : Slider.Instance;
      L     : Line.Instance;
      TA    : Textarea.Textarea;
      Check : Checkbox.Instance;
      Drop  : Ddlist.Instance;
      Lst   : List.Instance;
      L_Btn : Btn.Instance;
      Rol   : Roller.Instance;
   begin
      Page.set_scrl_layout (Parent, Cont.LAYOUT_PRETTY);

      H := Cont.create (Parent, No_Obj);
      set_click (H, 0);
      Cont.set_fit (H, 1, 1);
      Cont.set_layout (H, Cont.LAYOUT_COL_M);

      B := Btn.create (H, No_Obj);
      Btn.set_fit (B, 1, 1);
      set_click (B, 0);
      Label.set_text (Label.create (B, No_Obj), New_String ("Button"));

      B := Btn.create (H, No_Obj);
      Btn.set_fit (B, 1, 1);
      Btn.set_toggle (B, 1);
      Label.set_text (Label.create (B, No_Obj), New_String ("Toggle"));

      B := Btn.create (H, No_Obj);
      Btn.toggle (B);
      Label.set_text (Label.create (B, No_Obj), New_String ("Toggled"));

      B := Btn.create (H, No_Obj);
      Btn.set_state (B, Btn.STATE_INA);
      Label.set_text (Label.create (B, No_Obj), New_String ("Inactive"));

      Lab := Label.create (H, No_Obj);
      Label.set_text (Lab, New_String ("Primary"));
      --  FIXME: Style
      Lab := Label.create (H, No_Obj);
      Label.set_text (Lab, New_String ("Secondary"));
      --  FIXME: Style
      Lab := Label.create (H, No_Obj);
      Label.set_text (Lab, New_String ("Hint"));
      --  FIXME: Style

      BM := Btnm.Create (H, No_Obj);
      Set_Size (BM, LV_HOR_RES / 4, 2 * LV_DPI / 3);
      Btnm.set_map (BM, Btnm_Str'Address);
      Btnm.set_toggle (BM, 1, 3);

      H := Cont.create (Parent, H);

      Sw_H := Cont.create (H, No_Obj);
      Cont.set_fit (Sw_H, 0, 1);
      set_width (Sw_H, LV_HOR_RES / 4);
      Cont.set_layout (Sw_H, Cont.LAYOUT_PRETTY);

      S := Switch.create (Sw_H, No_Obj);
      Switch.off (S);
      S := Switch.create (Sw_H, No_Obj);
      Switch.on (S);

      --  FIXME: anim

      Ba := Bar.Create (H, No_Obj);
      Bar.set_Value (Ba, 70);

      Slide := Slider.create (H, No_Obj);
      Slider.set_value (Slide, 70);

      L := Line.create (H, No_Obj);
      Line.set_points (L, Points'Address, 2);
      --  FIXME: style

      Ta := Textarea.create (H, No_Obj);
      --  FIXME: style
      Textarea.set_text (TA, New_String ("Some text"));
      Textarea.set_one_line (TA, 1);

      Check := Checkbox.create (H, No_Obj);

      Check := Checkbox.create (H, Check);
      Btn.set_state (Check, Btn.STATE_TGL_REL);

      Drop := Ddlist.create (H, No_Obj);
      Ddlist.open (Drop, 0);
      Ddlist.set_selected (Drop, 1);

      H := Cont.create (Parent, H);

      Lst := List.create (H, No_Obj);

      L_Btn := List.add (Lst, SYMBOL_GPS'Address, New_String ("GPS"), null);
      Set_Size (L_Btn, LV_HOR_RES / 4, LV_VER_RES / 2);
      Btn.set_toggle (L_Btn, 1);

      L_Btn := List.add (Lst, SYMBOL_WIFI'Address, New_String ("WiFi"), null);
      L_Btn := List.add (Lst, SYMBOL_GPS'Address, New_String ("GPS"), null);
      L_Btn := List.add (Lst, SYMBOL_AUDIO'Address, New_String ("Audio"), null);
      L_Btn := List.add (Lst, SYMBOL_VIDEO'Address, New_String ("Video"), null);
      L_Btn := List.add (Lst, SYMBOL_CALL'Address, New_String ("Call"), null);
      L_Btn := List.add (Lst, SYMBOL_BELL'Address, New_String ("Bell"), null);
      L_Btn := List.add (Lst, SYMBOL_FILE'Address, New_String ("File"), null);
      L_Btn := List.add (Lst, SYMBOL_EDIT'Address, New_String ("Edit"), null);
      L_Btn := List.add (Lst, SYMBOL_CUT'Address, New_String ("Cut"), null);
      L_Btn := List.add (Lst, SYMBOL_COPY'Address, New_String ("Copy"), null);

      Rol := Roller.create (H, No_Obj);
      Roller.set_options (Rol, New_String ("Monday" & ASCII.LF &
                                           "Tuesday" & ASCII.LF &
                                           "Wednesday" & ASCII.LF &
                                           "Thursday" & ASCII.LF &
                                           "Friday" & ASCII.LF &
                                           "Saturday" & ASCII.LF &
                            "Sunday"));
      Roller.set_selected (Rol, 1, 0);
      Roller.set_visible_row_count (Rol, 3);
   end Create_Tab1;

   procedure Create_Tab2 (Parent : Page.Instance)is
      W  : constant LV.Area.Coord_T := page.get_scrl_width (Parent);
      Ch : Chart.Instance;
      S1 : Chart.Series;
      G  : Gauge.Instance;
      A  : Arc.Instance;
      TA : Textarea.Textarea;
      KB : Keyboard.Instance;
      LD : Preload.Instance;
   begin
      Ch := Chart.create (Parent, No_Obj);
      Set_Size (Ch, W / 3, LV_VER_RES / 3);
      Set_Pos (Ch, LV_DPI / 10, LV_DPI / 10);

      S1 := Chart.add_series (Ch, LV.Color.COLOR_RED);
      Chart.set_next (Ch, S1, 30);
      Chart.set_next (Ch, S1, 20);
      Chart.set_next (Ch, S1, 10);
      Chart.set_next (Ch, S1, 12);
      Chart.set_next (Ch, S1, 20);
      Chart.set_next (Ch, S1, 27);
      Chart.set_next (Ch, S1, 35);
      Chart.set_next (Ch, S1, 55);
      Chart.set_next (Ch, S1, 70);
      Chart.set_next (Ch, S1, 75);

      G := Gauge.create (Parent, No_Obj);
      Gauge.set_value (G, 0, 40);
      Set_Size (G, W / 4, W / 4);
      Align (G, Ch, ALIGN_OUT_BOTTOM_LEFT, 0, LV_DPI / 4);

      A := Arc.create (Parent, No_Obj);
      align (A, G, ALIGN_OUT_BOTTOM_MID, 0, LV_DPI / 8);

      TA := Textarea.create (Parent, No_Obj);
      Set_Size (TA, W / 3, LV_VER_RES / 4);
      align (TA, No_Obj, ALIGN_IN_TOP_RIGHT, -LV_DPI / 10, LV_DPI / 10);
      Textarea.set_cursor_type (TA, Textarea.CURSOR_BLOCK);

      KB := Keyboard.create (Parent, No_Obj);
      Set_Size (KB, 2 * W / 3, LV_VER_RES / 3);
      align (KB, TA, ALIGN_OUT_BOTTOM_RIGHT, 0, LV_DPI);
      Keyboard.Set_Textarea (KB, TA);

      LD := Preload.create (Parent, No_Obj);
      align (LD, No_Obj, ALIGN_CENTER, 0, -LV_DPI);
   end Create_Tab2;

   function Win_Close_Action (B : Btn.Instance) return lv_res_t
     with Convention => C;

   function Win_Close_Action (B : Btn.Instance) return lv_res_t is
      W : Win.Instance := Win.get_from_btn (B);
   begin
      Del (W);
      return RES_INV;
   end Win_Close_Action;

   Box_Btn_Map : constant array (Natural range <>) of Interfaces.C.Strings.chars_ptr
     := (New_String (Character'Val (16#89#) & ""),
         New_String (Character'Val (16#89#) & "Got it!"),
         New_String (Character'Val (16#89#) & ""),
         New_String (""))
       with Convention => C;

   procedure Create_Tab3 (Parent : Page.Instance) is
      W      : Win.Instance;
      Unused : Btn.Instance;
      L      : Label.Instance;
      Meter  : Lmeter.Instance;

      LED1, LED2 : LED.Instance;
      P      : Page.Instance;

      Cal    : Calendar.Instance;
      Highlighted_days : aliased array (0 .. 1) of aliased Calendar.date_t
        with Convention => C;

      Box : Mbox.Instance;
   begin
      --  Create a Window
      W := Win.create (Parent, No_Obj);
      Unused := Win.add_btn (W, SYMBOL_CLOSE'Address, Win_Close_Action'Access);
      Unused := Win.add_btn (W, SYMBOL_DOWN'Address, null);
      Set_Size (W, LV_HOR_RES / 2, LV_VER_RES / 2);
      Set_Pos (W, LV_DPI / 20, LV_DPI / 20);
      set_top (W, 1);

      --  Create a Label in the Window
      L := Label.create (W, No_Obj);
      Label.set_text (L, New_String ("Label in the Window"));

      --  Create a Line meter in the Window
      Meter := Lmeter.create (W, No_Obj);
      align (Meter, L, ALIGN_OUT_BOTTOM_LEFT, 0, LV_DPI / 2);
      Lmeter.set_value (Meter, 70);

      --  Create a 2 LEDs in the Window
      LED1 := LED.create (W, No_Obj);
      align (LED1, Meter, ALIGN_OUT_RIGHT_MID, LV_DPI / 2, 0);
      LED.on (LED1);

      LED2 := LED.create (W, No_Obj);
      align (LED2, LED1, ALIGN_OUT_RIGHT_MID, LV_DPI / 2, 0);
      LED.off (LED2);

      --  Create a Page
      P := Page.Create (Parent, No_Obj);
      Set_Size (P, LV_HOR_RES / 3, LV_VER_RES / 2);
      set_top (P, 1);
      align (P, W, ALIGN_IN_TOP_RIGHT, LV_DPI, LV_DPI);

      L := Label.create (P, No_Obj);
      Label.set_text (L, New_String ("Lorem ipsum dolor sit amet, repudiare voluptatibus pri cu." & ASCII.CR &
                        "Ei mundi pertinax posidonium eum, cum tempor maiorum at," & ASCII.CR &
                        "mea fuisset assentior ad. Usu cu suas civibus iudicabit." & ASCII.CR &
                        "Eum eu congue tempor facilisi. Tale hinc unum te vim." & ASCII.CR &
                        "Te cum populo animal eruditi, labitur inciderint at nec.\n" & ASCII.CR &
                        "Eius corpora et quo. Everti voluptaria instructior est id," & ASCII.CR &
                        "vel in falli primis. Mea ei porro essent admodum," & ASCII.CR &
                        "his ei malis quodsi, te quis aeterno his." & ASCII.CR &
                        "Qui tritani recusabo reprehendunt ne," & ASCII.CR &
                        "per duis explicari at. Simul mediocritatem mei et."));
      Page.set_scrl_fit (P, 1, 1);

      --  Create a Calendar
      Cal := Calendar.create (Parent, No_Obj);
      Set_Size (Cal, 5 * LV_DPI / 2, 5 * LV_DPI / 2);
      align (Cal, P, ALIGN_OUT_RIGHT_TOP, -LV_DPI / 2, LV_DPI / 3);
      set_top (Cal, 1);

      Highlighted_days (0).day := 5;
      Highlighted_days (0).month := 5;
      Highlighted_days (0).year := 2018;

      Highlighted_days (1).day := 8;
      Highlighted_days (1).month := 5;
      Highlighted_days (1).year := 2018;

      Calendar.set_highlighted_dates (Cal, Highlighted_days'Address, 2);
      Calendar.set_today_date (Cal, Highlighted_days (0)'access);
      Calendar.set_showed_date (Cal, Highlighted_days (0)'access);

      Box := Mbox.create (Parent, No_Obj);
      Mbox.set_text (Box, New_String ("Click on the window or the page to bring it to the foreground"));
      Mbox.add_btns (Box, Box_Btn_Map'Address, null);
      set_Top (Box, 1);
   end Create_Tab3;

   procedure Init is
      Scr  : Cont.Instance;
      TV   : Tabview.Instance;
      Tab1 : Page.Instance;
      Tab2 : Page.Instance;
      Tab3 : Page.Instance;
      Theme_Tab : Page.Instance;
   begin
      Init_Themes (220);
      Lv.Theme.Set_Current (Lv.Theme.Get_Night);

      Scr := Cont.Create (No_Obj, No_Obj);
      Scr_Load (Scr);

      --        Cont.set_style (Scr, T.bg);

      TV := Tabview.Create (Scr, No_Obj);
      Set_Size (TV, LV_HOR_RES, LV_VER_RES);

      Theme_Tab := Tabview.Add_Tab (TV, New_String ("Theme"));
      Tab1 := Tabview.Add_Tab (TV, New_String ("Tab1"));
      Tab2 := Tabview.Add_Tab (TV, New_String ("Tab2"));
      Tab3 := Tabview.Add_Tab (TV, New_String ("Tab3"));

      Create_Theme_Tab (Theme_Tab);
      Create_Tab1 (Tab1);
      Create_Tab2 (Tab2);
      Create_Tab3 (Tab3);
   end Init;

end Test_Theme_1;
