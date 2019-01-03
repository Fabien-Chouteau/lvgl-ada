pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with System;
with Interfaces.C.Extensions;

package LV.Anim is

   type lv_anim_path_t is access function (arg1 : System.Address) return int32_t;
   pragma Convention (C, lv_anim_path_t);  -- ../lv_misc/lv_anim.h:38

   type lv_anim_fp_t is access procedure (arg1 : System.Address; arg2 : int32_t);
   pragma Convention (C, lv_anim_fp_t);  -- ../lv_misc/lv_anim.h:40

   type lv_anim_cb_t is access procedure (arg1 : System.Address);
   pragma Convention (C, lv_anim_cb_t);  -- ../lv_misc/lv_anim.h:41

   type u_lv_anim_t is record
      var : System.Address;  -- ../lv_misc/lv_anim.h:45
      fp : lv_anim_fp_t;  -- ../lv_misc/lv_anim.h:46
      end_cb : lv_anim_cb_t;  -- ../lv_misc/lv_anim.h:47
      path : lv_anim_path_t;  -- ../lv_misc/lv_anim.h:48
      start : aliased int32_t;  -- ../lv_misc/lv_anim.h:49
      c_end : aliased int32_t;  -- ../lv_misc/lv_anim.h:50
      time : aliased uint16_t;  -- ../lv_misc/lv_anim.h:51
      act_time : aliased int16_t;  -- ../lv_misc/lv_anim.h:52
      playback_pause : aliased uint16_t;  -- ../lv_misc/lv_anim.h:53
      repeat_pause : aliased uint16_t;  -- ../lv_misc/lv_anim.h:54
      playback : Extensions.Unsigned_1;  -- ../lv_misc/lv_anim.h:55
      repeat : Extensions.Unsigned_1;  -- ../lv_misc/lv_anim.h:56
      playback_now : Extensions.Unsigned_1;  -- ../lv_misc/lv_anim.h:58
      has_run : Extensions.Unsigned_1;  -- ../lv_misc/lv_anim.h:59
   end record;
   pragma Convention (C_Pass_By_Copy, u_lv_anim_t);
   pragma Pack (u_lv_anim_t);  -- ../lv_misc/lv_anim.h:43

   subtype lv_anim_t is u_lv_anim_t;  -- ../lv_misc/lv_anim.h:60

   procedure lv_anim_init;  -- ../lv_misc/lv_anim.h:85
   pragma Import (C, lv_anim_init, "lv_anim_init");

   procedure lv_anim_create (arg1 : access lv_anim_t);  -- ../lv_misc/lv_anim.h:91
   pragma Import (C, lv_anim_create, "lv_anim_create");

   function lv_anim_del (arg1 : System.Address; arg2 : lv_anim_fp_t) return u_Bool;  -- ../lv_misc/lv_anim.h:100
   pragma Import (C, lv_anim_del, "lv_anim_del");

   function lv_anim_speed_to_time
     (arg1 : uint16_t;
      arg2 : int32_t;
      arg3 : int32_t) return uint16_t;  -- ../lv_misc/lv_anim.h:109
   pragma Import (C, lv_anim_speed_to_time, "lv_anim_speed_to_time");

   function lv_anim_path_linear (arg1 : access constant lv_anim_t) return int32_t;  -- ../lv_misc/lv_anim.h:116
   pragma Import (C, lv_anim_path_linear, "lv_anim_path_linear");

   function lv_anim_path_ease_in_out (arg1 : access constant lv_anim_t) return int32_t;  -- ../lv_misc/lv_anim.h:124
   pragma Import (C, lv_anim_path_ease_in_out, "lv_anim_path_ease_in_out");

   function lv_anim_path_step (arg1 : access constant lv_anim_t) return int32_t;  -- ../lv_misc/lv_anim.h:132
   pragma Import (C, lv_anim_path_step, "lv_anim_path_step");

end LV.Anim;
