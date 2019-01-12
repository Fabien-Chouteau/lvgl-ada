with System;
with Interfaces.C.Extensions;

package Lv.Anim is

   type Path_T is access function (Arg1 : System.Address) return Int32_T;
   pragma Convention (C, Path_T);

   type Fp_T is access procedure (Arg1 : System.Address; Arg2 : Int32_T);
   pragma Convention (C, Fp_T);

   type Cb_T is access procedure (Arg1 : System.Address);
   pragma Convention (C, Cb_T);

   type U_Lv_Anim_T is record
      Var            : System.Address;
      Fp             : Fp_T;
      End_Cb         : Cb_T;
      Path           : Path_T;
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

   --  Init. the animation module
   procedure Init;

   --  Create an animation
   --  @param p an initialized 'anim_t' variable. Not required after call.
   procedure Create (A : access Anim_T);

   --  Delete an animation for a variable with a given animatior function
   --  @param var pointer to variable
   --  @param fp a function pointer which is animating 'var',
   --            or NULL to ignore it and delete all animation with 'var
   --  @return true: at least 1 animation is deleted, false: no animation is deleted
   function Del (Var : System.Address; Fp : Fp_T) return U_Bool;

   --  Calculate the time of an animation with a given speed and the start and end values
   --  @param speed speed of animation in unit/sec
   --  @param start start value of the animation
   --  @param end end value of the animation
   --  @return the required time [ms] for the animation with the given parameters
   function Speed_To_Time
     (Speed : Uint16_T;
      Start : Int32_T;
      End_P : Int32_T) return Uint16_T;

   --  Calculate the current value of an animation applying linear characteristic
   --  @param a pointer to an animation
   --  @return the current value to set
   function Path_Linear (A : access constant Anim_T) return Int32_T;

   --  Calculate the current value of an animation applying an "S" characteristic (cosine)
   --  @param a pointer to an animation
   --  @return the current value to set
   function Path_Ease_In_Out
     (A : access constant Anim_T) return Int32_T;

   --  Calculate the current value of an animation applying step characteristic.
   --  (Set end value on the end of the animation)
   --  @param a pointer to an animation
   --  @return the current value to set
   function Path_Step (A : access constant Anim_T) return Int32_T;

   -------------
   -- Imports --
   -------------

   pragma Import (C, Init, "lv_anim_init");
   pragma Import (C, Create, "lv_anim_create");
   pragma Import (C, Del, "lv_anim_del");
   pragma Import (C, Speed_To_Time, "lv_anim_speed_to_time");
   pragma Import (C, Path_Linear, "lv_anim_path_linear");
   pragma Import (C, Path_Ease_In_Out, "lv_anim_path_ease_in_out");
   pragma Import (C, Path_Step, "lv_anim_path_step");

end Lv.Anim;
