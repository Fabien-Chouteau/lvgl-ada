pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with System;
with Interfaces.C.Extensions;

package Lv.Anim is

   type Anim_Path_T is access function (Arg1 : System.Address) return Int32_T;
   pragma Convention (C, Anim_Path_T);

   type Anim_Fp_T is access procedure (Arg1 : System.Address; Arg2 : Int32_T);
   pragma Convention (C, Anim_Fp_T);

   type Anim_Cb_T is access procedure (Arg1 : System.Address);
   pragma Convention (C, Anim_Cb_T);

   type U_Lv_Anim_T is record
      Var            : System.Address;
      Fp             : Anim_Fp_T;
      End_Cb         : Anim_Cb_T;
      Path           : Anim_Path_T;
      Start          : aliased Int32_T;
      C_End          : aliased Int32_T;
      Time           : aliased Uint16_T;
      Act_Time       : aliased Int16_T;
      Playback_Pause : aliased Uint16_T;
      Repeat_Pause   : aliased Uint16_T;
      Playback       : Extensions.Unsigned_1;
      Repeat         : Extensions.Unsigned_1;
      Playback_Now   : Extensions.Unsigned_1;
      Has_Run        : Extensions.Unsigned_1;
   end record;
   pragma Convention (C_Pass_By_Copy, U_Lv_Anim_T);
   pragma Pack (U_Lv_Anim_T);

   subtype Anim_T is U_Lv_Anim_T;

   procedure Anim_Init;
   pragma Import (C, Anim_Init, "lv_anim_init");

   procedure Anim_Create (Arg1 : access Anim_T);
   pragma Import (C, Anim_Create, "lv_anim_create");

   function Anim_Del (Arg1 : System.Address; Arg2 : Anim_Fp_T) return U_Bool;
   pragma Import (C, Anim_Del, "lv_anim_del");

   function Anim_Speed_To_Time
     (Arg1 : Uint16_T;
      Arg2 : Int32_T;
      Arg3 : Int32_T) return Uint16_T;
   pragma Import (C, Anim_Speed_To_Time, "lv_anim_speed_to_time");

   function Anim_Path_Linear (Arg1 : access constant Anim_T) return Int32_T;
   pragma Import (C, Anim_Path_Linear, "lv_anim_path_linear");

   function Anim_Path_Ease_In_Out
     (Arg1 : access constant Anim_T) return Int32_T;
   pragma Import (C, Anim_Path_Ease_In_Out, "lv_anim_path_ease_in_out");

   function Anim_Path_Step (Arg1 : access constant Anim_T) return Int32_T;
   pragma Import (C, Anim_Path_Step, "lv_anim_path_step");

end Lv.Anim;
