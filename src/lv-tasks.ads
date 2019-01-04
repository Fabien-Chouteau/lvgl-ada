with Interfaces.C; use Interfaces.C;
with System;
with Interfaces.C.Extensions;

package Lv.Tasks is

   type Instance is private;

   type Prio_T is
     (Prio_Off,
      Prio_Lowest,
      Prio_Low,
      Prio_Mid,
      Prio_High,
      Prio_Highest,
      Prio_Num) with
        Size => 8;

   for Prio_T use
     (Prio_Off     => 0,
      Prio_Lowest  => 1,
      Prio_Low     => 2,
      Prio_Mid     => 3,
      Prio_High    => 4,
      Prio_Highest => 5,
      Prio_Num     => 6);

   procedure Handler;
   pragma Import (C, Handler, "lv_task_handler");

   function Create
     (Proc   : access procedure (Param : System.Address);
      Period : Uint32_T;
      Prio   : Prio_T;
      Param  : System.Address) return Instance;
   pragma Import (C, Create, "lv_task_create");

   procedure Del (Self : Instance);
   pragma Import (C, Del, "lv_task_del");

   procedure Set_Prio (Self : Instance; Prio : Prio_T);
   pragma Import (C, Set_Prio, "lv_task_set_prio");

   procedure Set_Period (Self : Instance; Period : Uint32_T);
   pragma Import (C, Set_Period, "lv_task_set_period");

   procedure Ready (Self : Instance);
   pragma Import (C, Ready, "lv_task_ready");

   procedure Once (Self : Instance);
   pragma Import (C, Once, "lv_task_once");

   procedure Reset (Self : Instance);
   pragma Import (C, Reset, "lv_task_reset");

   procedure Enable (En : U_Bool);
   pragma Import (C, Enable, "lv_task_enable");

   function Get_Idle return Uint8_T;
   pragma Import (C, Get_Idle, "lv_task_get_idle");

private
   type Instance is new System.Address;

   --  This Init is already called in lv_init()
   procedure Init;
   pragma Import (C, Init, "lv_task_init");

end Lv.Tasks;
