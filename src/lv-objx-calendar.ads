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

   type Style_T is
    (Style_BG,       --  Also the style of the "normal" date numbers
     Style_HEADER,
     Style_HEADER_PR,
     Style_DAY_NAMES,
     Style_HIGHLIGHTED_DAYS,
     Style_INACTIVE_DAYS,
     Style_WEEK_BOX,
     Style_TODAY_BOX);

   type Date_Array is array (Natural range <>) of aliased Date_T
     with Convention => C;
   pragma Suppress (All_Checks, Date_Array);

   --  Create a calendar objects
   --  @param par pointer to an object, it will be the parent of the new calendar
   --  @param copy pointer to a calendar object, if not NULL then the new object will be copied from it
   --  @return pointer to the created calendar
   function Create (Parent : Obj_T; Copy : Instance) return Instance;

   ----------------------
   -- Setter functions --
   ----------------------

   --  Set the today's date
   --  @param self pointer to a calendar object
   --  @param today pointer to an `lv_calendar_date_t` variable containing the date of today. The value will be saved it can be local variable too.
   procedure Set_Today_Date (Self : Instance; Today : access Date_T);

   --  Set the currently showed
   --  @param self pointer to a calendar object
   --  @param showed pointer to an `lv_calendar_date_t` variable containing the date to show. The value will be saved it can be local variable too.
   procedure Set_Showed_Date (Self : Instance; Showed : access Date_T);

   --  Set the the highlighted dates
   --  @param self pointer to a calendar object
   --  @param highlighted pointer to an `lv_calendar_date_t` array containing the dates. ONLY A POINTER WILL BE SAVED! CAN'T BE LOCAL ARRAY.
   --  @param date_num number of dates in the array
   procedure Set_Highlighted_Dates
     (Self        : Instance;
      Highlighted : access constant Date_Array;
      Date_Num    : Uint16_T);

   --  Set the name of the days
   --  @param self pointer to a calendar object
   --  @param day_names pointer to an array with the names. E.g. `const char   --  days[7] = {"Sun", "Mon", ...}`
   --                   Only the pointer will be saved so this variable can't be local which will be destroyed later.
   procedure Set_Day_Names (Self : Instance; Day_Names : access constant String_Array);

   --  Set the name of the month
   --  @param self pointer to a calendar object
   --  @param month_names pointer to an array with the names. E.g. `const char   --  days[12] = {"Jan", "Feb", ...}`
   --                   Only the pointer will be saved so this variable can't be local which will be destroyed later.
   procedure Set_Month_Names (Self : Instance; Month_Names : access constant String_Array);

   --  Set a style of a calendar.
   --  @param self pointer to calendar object
   --  @param type which style should be set
   --  @param style pointer to a style
   procedure Set_Style
     (Self   : Instance;
      Type_P : Style_T;
      Style  : Lv.Style.Style);

   ----------------------
   -- Getter functions --
   ----------------------

   --  Get the today's date
   --  @param self pointer to a calendar object
   --  @return return pointer to an `lv_calendar_date_t` variable containing the date of today.
   function Today_Date (Self : Instance) return access Date_T;

   --  Get the currently showed
   --  @param self pointer to a calendar object
   --  @return pointer to an `lv_calendar_date_t` variable containing the date is being shown.
   function Showed_Date (Self : Instance) return access Date_T;

   --  Get the the highlighted dates
   --  @param self pointer to a calendar object
   --  @return pointer to an `lv_calendar_date_t` array containing the dates.
   function Highlighted_Dates (Self : Instance) return access Date_T;

   --  Get the number of the highlighted dates
   --  @param self pointer to a calendar object
   --  @return number of highlighted days
   function Highlighted_Dates_Num (Self : Instance) return Uint16_T;

   --  Get the name of the days
   --  @param self pointer to a calendar object
   --  @return pointer to the array of day names
   function Day_Names (Self : Instance) return System.Address;

   --  Get the name of the month
   --  @param self pointer to a calendar object
   --  @return pointer to the array of month names
   function Month_Names (Self : Instance) return System.Address;

   --  Get style of a calendar.
   --  @param self pointer to calendar object
   --  @param type which style should be get
   --  @return style pointer to the style
   function Style
     (Self   : Instance;
      Type_P : Style_T) return Lv.Style.Style;

   -------------
   -- Imports --
   -------------

   pragma Import (C, Create, "lv_calendar_create");
   pragma Import (C, Set_Today_Date, "lv_calendar_set_today_date");
   pragma Import (C, Set_Showed_Date, "lv_calendar_set_showed_date");
   pragma Import (C, Set_Highlighted_Dates, "lv_calendar_set_highlighted_dates");
   pragma Import (C, Set_Day_Names, "lv_calendar_set_day_names");
   pragma Import (C, Set_Month_Names, "lv_calendar_set_month_names");
   pragma Import (C, Set_Style, "lv_calendar_set_style");
   pragma Import (C, Today_Date, "lv_calendar_get_today_date");
   pragma Import (C, Showed_Date, "lv_calendar_get_showed_date");
   pragma Import (C, Highlighted_Dates, "lv_calendar_get_highlighted_dates");
   pragma Import (C, Highlighted_Dates_Num, "lv_calendar_get_highlighted_dates_num");
   pragma Import (C, Day_Names, "lv_calendar_get_day_names");
   pragma Import (C, Month_Names, "lv_calendar_get_month_names");
   pragma Import (C, Style, "lv_calendar_get_style");

   for Style_T'Size use 8;
   for Style_T use (Style_BG               => 0,
                    Style_HEADER           => 1,
                    Style_HEADER_PR        => 2,
                    Style_DAY_NAMES        => 3,
                    Style_HIGHLIGHTED_DAYS => 4,
                    Style_INACTIVE_DAYS    => 5,
                    Style_WEEK_BOX         => 6,
                    Style_TODAY_BOX        => 7);

end Lv.Objx.Calendar;
