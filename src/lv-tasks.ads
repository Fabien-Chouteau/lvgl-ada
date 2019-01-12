with System;

package Lv.Tasks is

   type Instance is private;

   type Prio_T is (Prio_Off,
                   Prio_Lowest,
                   Prio_Low,
                   Prio_Mid,
                   Prio_High,
                   Prio_Highest,
                   Prio_Num);

   --  Call it  periodically to handle lv_tasks.
   procedure Handler;

   --  Create a new lv_task
   --  @param task a function which is the task itself
   --  @param period call period in ms unit
   --  @param prio priority of the task (LV_TASK_PRIO_OFF means the task is stopped)
   --  @param param free parameter
   --  @return pointer to the new task
   function Create
     (Proc   : access procedure (Param : System.Address);
      Period : Uint32_T;
      Prio   : Prio_T;
      Param  : System.Address) return Instance;

   --  Delete a lv_task
   --  @param lv_task_p pointer to task created by lv_task_p
   procedure Del (Self : Instance);

   --  Set new priority for a lv_task
   --  @param lv_task_p pointer to a lv_task
   --  @param prio the new priority
   procedure Set_Prio (Self : Instance; Prio : Prio_T);

   --  Set new period for a lv_task
   --  @param lv_task_p pointer to a lv_task
   --  @param period the new period
   procedure Set_Period (Self : Instance; Period : Uint32_T);

   --  Make a lv_task ready. It will not wait its period.
   --  @param lv_task_p pointer to a lv_task.
   procedure Ready (Self : Instance);

   --  Delete the lv_task after one call
   --  @param lv_task_p pointer to a lv_task.
   procedure Once (Self : Instance);

   --  Reset a lv_task.
   --  It will be called the previously set period milliseconds later.
   --  @param lv_task_p pointer to a lv_task.
   procedure Reset (Self : Instance);

   --  Enable or disable the whole  lv_task handling
   --  @param en: true: lv_task handling is running, false: lv_task handling is suspended
   procedure Enable (En : U_Bool);

   --  Get idle percentage
   --  @return the lv_task idle in percentage
   function Get_Idle return Uint8_T;

private
   type Instance is new System.Address;

   --  This Init is already called in lv_init()
   procedure Init;

   -------------
   -- Imports --
   -------------

   pragma Import (C, Handler, "lv_task_handler");
   pragma Import (C, Create, "lv_task_create");
   pragma Import (C, Del, "lv_task_del");
   pragma Import (C, Set_Prio, "lv_task_set_prio");
   pragma Import (C, Set_Period, "lv_task_set_period");
   pragma Import (C, Ready, "lv_task_ready");
   pragma Import (C, Once, "lv_task_once");
   pragma Import (C, Reset, "lv_task_reset");
   pragma Import (C, Enable, "lv_task_enable");
   pragma Import (C, Get_Idle, "lv_task_get_idle");
   pragma Import (C, Init, "lv_task_init");

   for Prio_T'Size use 8;
   for Prio_T use
     (Prio_Off     => 0,
      Prio_Lowest  => 1,
      Prio_Low     => 2,
      Prio_Mid     => 3,
      Prio_High    => 4,
      Prio_Highest => 5,
      Prio_Num     => 6);

end Lv.Tasks;
