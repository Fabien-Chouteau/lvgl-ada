with Interfaces.C; use Interfaces.C;
with Lv.Area;
with System;
with Interfaces.C.Extensions;

package Lv.Hal.Indev is

   type Indev_Type_T is
     (Type_None, Type_Pointer, Type_Keypad, Type_Button, Type_Encoder) with
        Size => 8;

   for Indev_Type_T use
     (Type_None    => 0,
      Type_Pointer => 1,
      Type_Keypad  => 2,
      Type_Button  => 3,
      Type_Encoder => 4);

   type Indev_State_T is (State_Rel, State_Pr) with
        Size => 8;

   for Indev_State_T use (State_Rel => 0, State_Pr => 1);

   type Indev_Data_T_Union (Discr : unsigned := 0) is record
      case Discr is
         when 0 =>
            Point : aliased Lv.Area.Point_T;
         when 1 =>
            Key : aliased Uint32_T;
         when 2 =>
            Btn : aliased Uint32_T;
         when others =>
            Enc_Diff : aliased Int16_T;
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, Indev_Data_T_Union);
   pragma Unchecked_Union (Indev_Data_T_Union);

   type Indev_Data_T is record
      Union     : aliased Indev_Data_T_Union;
      User_Data : System.Address;
      State     : aliased Indev_State_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Indev_Data_T);

   type Indev_Drv_T is record
      C_Type    : aliased Indev_Type_T;
      Read      : access function (Data : access Indev_Data_T) return U_Bool;
      User_Data : System.Address;
   end record;
   pragma Convention (C_Pass_By_Copy, Indev_Drv_T);

   type Indev_T is private;

   procedure Init (Arg1 : access Indev_Drv_T);
   pragma Import (C, Init, "lv_indev_drv_init");

   function Register
     (Arg1 : access Indev_Drv_T) return Indev_T;  -- lv_hal_indev.h:136
   pragma Import (C, Register, "lv_indev_drv_register");

   function Next
     (Arg1 : access Indev_T) return access Indev_T;  -- lv_hal_indev.h:143
   pragma Import (C, Next, "lv_indev_next");

   function Read
     (Arg1 : access Indev_T;
      Arg2 : access Indev_Data_T) return U_Bool;  -- lv_hal_indev.h:151
   pragma Import (C, Read, "lv_indev_read");

private

   type Indev_T is new System.Address;

--     subtype lv_hal_indev_type_t is sys_ustdint_h.uint8_t;  -- lv_hal_indev.h:39
--
--     subtype lv_indev_state_t is sys_ustdint_h.uint8_t;  -- lv_hal_indev.h:46
--
--     type lv_indev_data_t;
--     type lv_indev_data_t_anon3901_union (discr : unsigned := 0) is record
--        case discr is
--           when 0 =>
--              point : aliased lv_area_h.lv_point_t;  -- lv_hal_indev.h:51
--           when 1 =>
--              key : aliased sys_ustdint_h.uint32_t;  -- lv_hal_indev.h:52
--           when 2 =>
--              btn : aliased sys_ustdint_h.uint32_t;  -- lv_hal_indev.h:53
--           when others =>
--              enc_diff : aliased sys_ustdint_h.int16_t;  -- lv_hal_indev.h:54
--        end case;
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_indev_data_t_anon3901_union);
--     pragma Unchecked_Union (lv_indev_data_t_anon3901_union);type lv_indev_data_t is record
--        anon4686 : aliased lv_indev_data_t_anon3901_union;  -- lv_hal_indev.h:55
--        user_data : System.Address;  -- lv_hal_indev.h:56
--        state : aliased lv_indev_state_t;  -- lv_hal_indev.h:57
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_indev_data_t);  -- lv_hal_indev.h:58
--
--
--     --  skipped empty struct u_lv_obj_t
--
--     type u_lv_indev_proc_t;
--     type anon3912_anon3914_struct is record
--        act_point : aliased lv_area_h.lv_point_t;  -- lv_hal_indev.h:74
--        last_point : aliased lv_area_h.lv_point_t;  -- lv_hal_indev.h:75
--        vect : aliased lv_area_h.lv_point_t;  -- lv_hal_indev.h:76
--        drag_sum : aliased lv_area_h.lv_point_t;  -- lv_hal_indev.h:77
--        act_obj : System.Address;  -- lv_hal_indev.h:78
--        last_obj : System.Address;  -- lv_hal_indev.h:79
--        drag_range_out : Extensions.Unsigned_1;  -- lv_hal_indev.h:82
--        drag_in_prog : Extensions.Unsigned_1;  -- lv_hal_indev.h:83
--        wait_unil_release : Extensions.Unsigned_1;  -- lv_hal_indev.h:84
--     end record;
--     pragma Convention (C_Pass_By_Copy, anon3912_anon3914_struct);
--     type anon3912_anon3916_struct is record
--        last_state : aliased lv_indev_state_t;  -- lv_hal_indev.h:87
--        last_key : aliased sys_ustdint_h.uint32_t;  -- lv_hal_indev.h:88
--     end record;
--     pragma Convention (C_Pass_By_Copy, anon3912_anon3916_struct);
--     type anon3912_anon3913_union (discr : unsigned := 0) is record
--        case discr is
--              anon4711 : aliased anon3912_anon3914_struct;  -- lv_hal_indev.h:85
--              anon4715 : aliased anon3912_anon3916_struct;  -- lv_hal_indev.h:89
--        end case;
--     end record;
--     pragma Convention (C_Pass_By_Copy, anon3912_anon3913_union);
--     pragma Unchecked_Union (anon3912_anon3913_union);type u_lv_indev_proc_t is record
--        state : aliased lv_indev_state_t;  -- lv_hal_indev.h:71
--        anon4716 : aliased anon3912_anon3913_union;  -- lv_hal_indev.h:90
--        pr_timestamp : aliased sys_ustdint_h.uint32_t;  -- lv_hal_indev.h:92
--        longpr_rep_timestamp : aliased sys_ustdint_h.uint32_t;  -- lv_hal_indev.h:93
--        long_pr_sent : Extensions.Unsigned_1;  -- lv_hal_indev.h:96
--        reset_query : Extensions.Unsigned_1;  -- lv_hal_indev.h:97
--        disabled : Extensions.Unsigned_1;  -- lv_hal_indev.h:98
--     end record;
--     pragma Convention (C_Pass_By_Copy, u_lv_indev_proc_t);
--     pragma Pack (u_lv_indev_proc_t);  -- lv_hal_indev.h:70
--
--     subtype lv_indev_proc_t is u_lv_indev_proc_t;  -- lv_hal_indev.h:99
--
--     --  skipped empty struct u_lv_group_t
--
--     type u_lv_indev_t;
--     type anon3919_anon3920_union (discr : unsigned := 0) is record
--        case discr is
--           when 0 =>
--              cursor : System.Address;  -- lv_hal_indev.h:111
--           when 1 =>
--              group : System.Address;  -- lv_hal_indev.h:112
--           when others =>
--              btn_points : access lv_area_h.lv_point_t;  -- lv_hal_indev.h:113
--        end case;
--     end record;
--     pragma Convention (C_Pass_By_Copy, anon3919_anon3920_union);
--     pragma Unchecked_Union (anon3919_anon3920_union);type u_lv_indev_t is record
--        driver : aliased lv_indev_drv_t;  -- lv_hal_indev.h:107
--        proc : aliased lv_indev_proc_t;  -- lv_hal_indev.h:108
--        last_activity_time : aliased sys_ustdint_h.uint32_t;  -- lv_hal_indev.h:109
--        anon4733 : aliased anon3919_anon3920_union;  -- lv_hal_indev.h:115
--        next : access u_lv_indev_t;  -- lv_hal_indev.h:116
--     end record;
--     pragma Convention (C_Pass_By_Copy, u_lv_indev_t);  -- lv_hal_indev.h:106
--
--     subtype lv_indev_t is u_lv_indev_t;  -- lv_hal_indev.h:117

end Lv.Hal.Indev;
