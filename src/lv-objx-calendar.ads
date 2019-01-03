with Interfaces.C; use Interfaces.C;
with System;
with LV.Style;

package LV.Objx.Calendar is

   subtype Instance is Obj_T;

   type date_t is record
      year  : aliased uint16_t;  -- lv_calendar.h:35
      month : aliased int8_t;  -- lv_calendar.h:36
      day  : aliased int8_t;  -- lv_calendar.h:37
   end record;
   pragma Convention (C_Pass_By_Copy, date_t);  -- lv_calendar.h:38

   type String_Array is array (Natural range <>) of Interfaces.C.Strings.chars_ptr
     with Convention => C;

   subtype style_t is uint8_t;  -- lv_calendar.h:73

   function create (Parent : Obj_T; Copy : Instance) return Instance;  -- lv_calendar.h:87
   pragma Import (C, create, "lv_calendar_create");

   procedure set_today_date (Self : Instance; arg2 : access date_t);  -- lv_calendar.h:103
   pragma Import (C, set_today_date, "lv_calendar_set_today_date");

   procedure set_showed_date (Self : Instance; arg2 : access date_t);  -- lv_calendar.h:110
   pragma Import (C, set_showed_date, "lv_calendar_set_showed_date");

   procedure set_highlighted_dates
     (Self : Instance;
      arg2 : System.Address; --  Array of date_t
      arg3 : uint16_t);  -- lv_calendar.h:118
   pragma Import (C, set_highlighted_dates, "lv_calendar_set_highlighted_dates");

   procedure set_day_names (Self : Instance; arg2 : System.Address);  -- lv_calendar.h:127
   pragma Import (C, set_day_names, "lv_calendar_set_day_names");

   procedure set_month_names (Self : Instance; arg2 : System.Address);  -- lv_calendar.h:135
   pragma Import (C, set_month_names, "lv_calendar_set_month_names");

   procedure set_style
     (Self : Instance;
      arg2 : style_t;
      arg3 : access LV.Style.Style);  -- lv_calendar.h:143
   pragma Import (C, set_style, "lv_calendar_set_style");

   function get_today_date (Self : Instance) return access date_t;  -- lv_calendar.h:154
   pragma Import (C, get_today_date, "lv_calendar_get_today_date");

   function get_showed_date (Self : Instance) return access date_t;  -- lv_calendar.h:161
   pragma Import (C, get_showed_date, "lv_calendar_get_showed_date");

   function get_highlighted_dates (Self : Instance) return access date_t;  -- lv_calendar.h:168
   pragma Import (C, get_highlighted_dates, "lv_calendar_get_highlighted_dates");

   function get_highlighted_dates_num (Self : Instance) return uint16_t;  -- lv_calendar.h:175
   pragma Import (C, get_highlighted_dates_num, "lv_calendar_get_highlighted_dates_num");

   function get_day_names (Self : Instance) return System.Address;  -- lv_calendar.h:183
   pragma Import (C, get_day_names, "lv_calendar_get_day_names");

   function get_month_names (Self : Instance) return System.Address;  -- lv_calendar.h:190
   pragma Import (C, get_month_names, "lv_calendar_get_month_names");

   function get_style (Self : Instance; arg2 : style_t) return access LV.Style.Style;  -- lv_calendar.h:198
   pragma Import (C, get_style, "lv_calendar_get_style");

--  private
--     type lv_calendar_ext_t is record
--        today : aliased lv_calendar_date_t;  -- lv_calendar.h:44
--        showed_date : aliased lv_calendar_date_t;  -- lv_calendar.h:45
--        highlighted_dates : access lv_calendar_date_t;  -- lv_calendar.h:46
--        highlighted_dates_num : aliased uint8_t;  -- lv_calendar.h:47
--        btn_pressing : aliased sys_ustdint_h.int8_t;  -- lv_calendar.h:48
--        day_names : System.Address;  -- lv_calendar.h:49
--        month_names : System.Address;  -- lv_calendar.h:50
--        style_header : access lv_style_h.lv_style_t;  -- lv_calendar.h:53
--        style_header_pr : access lv_style_h.lv_style_t;  -- lv_calendar.h:54
--        style_day_names : access lv_style_h.lv_style_t;  -- lv_calendar.h:55
--        style_highlighted_days : access lv_style_h.lv_style_t;  -- lv_calendar.h:56
--        style_inactive_days : access lv_style_h.lv_style_t;  -- lv_calendar.h:57
--        style_week_box : access lv_style_h.lv_style_t;  -- lv_calendar.h:58
--        style_today_box : access lv_style_h.lv_style_t;  -- lv_calendar.h:59
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_calendar_ext_t);  -- lv_calendar.h:60

end LV.Objx.Calendar;
