with Interfaces.C; use Interfaces.C;
with System;
with Lv.Style;

package Lv.Objx.Calendar is

   subtype Instance is Obj_T;

   type Date_T is record
      Year  : aliased Uint16_T;
      Month : aliased Int8_T;
      Day   : aliased Int8_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Date_T);

   type String_Array is
     array (Natural range <>) of Interfaces.C.Strings.chars_ptr with
        Convention => C;

   subtype Style_T is Uint8_T;

   function Create (Parent : Obj_T; Copy : Instance) return Instance;
   pragma Import (C, Create, "lv_calendar_create");

   procedure Set_Today_Date (Self : Instance; Arg2 : access Date_T);
   pragma Import (C, Set_Today_Date, "lv_calendar_set_today_date");

   procedure Set_Showed_Date (Self : Instance; Arg2 : access Date_T);
   pragma Import (C, Set_Showed_Date, "lv_calendar_set_showed_date");

   procedure Set_Highlighted_Dates
     (Self : Instance;
      Arg2 : System.Address; --  Array of date_t
      Arg3 : Uint16_T);
   pragma Import
     (C,
      Set_Highlighted_Dates,
      "lv_calendar_set_highlighted_dates");

   procedure Set_Day_Names (Self : Instance; Arg2 : System.Address);
   pragma Import (C, Set_Day_Names, "lv_calendar_set_day_names");

   procedure Set_Month_Names (Self : Instance; Arg2 : System.Address);
   pragma Import (C, Set_Month_Names, "lv_calendar_set_month_names");

   procedure Set_Style
     (Self : Instance;
      Arg2 : Style_T;
      Arg3 : access Lv.Style.Style);
   pragma Import (C, Set_Style, "lv_calendar_set_style");

   function Get_Today_Date (Self : Instance) return access Date_T;
   pragma Import (C, Get_Today_Date, "lv_calendar_get_today_date");

   function Get_Showed_Date (Self : Instance) return access Date_T;
   pragma Import (C, Get_Showed_Date, "lv_calendar_get_showed_date");

   function Get_Highlighted_Dates (Self : Instance) return access Date_T;
   pragma Import
     (C,
      Get_Highlighted_Dates,
      "lv_calendar_get_highlighted_dates");

   function Get_Highlighted_Dates_Num (Self : Instance) return Uint16_T;
   pragma Import
     (C,
      Get_Highlighted_Dates_Num,
      "lv_calendar_get_highlighted_dates_num");

   function Get_Day_Names (Self : Instance) return System.Address;
   pragma Import (C, Get_Day_Names, "lv_calendar_get_day_names");

   function Get_Month_Names (Self : Instance) return System.Address;
   pragma Import (C, Get_Month_Names, "lv_calendar_get_month_names");

   function Get_Style
     (Self : Instance;
      Arg2 : Style_T) return access Lv.Style.Style;
   pragma Import (C, Get_Style, "lv_calendar_get_style");

end Lv.Objx.Calendar;
