with Lv.Objx;

private with System;

package Lv.Group is

   type Instance is private;

   function Create return Instance;

   procedure Del (Group : Instance);

   procedure Add_Obj (Group : Instance; Obj : Lv.Objx.Obj_T);

   procedure Remove_Obj (Obj : Lv.Objx.Obj_T);

   procedure Focus_Obj (Obj : Lv.Objx.Obj_T);

   procedure Focus_Next (Group : Instance);

   procedure Focus_Prev (Group : Instance);

   procedure Focus_Freeze (Group : Instance; En : U_Bool);

   function Send_Data (Group : Instance; C : Uint32_T) return Lv.Objx.Res_T;

private

   type Instance is new System.Address;

   -------------
   -- Imports --
   -------------

   pragma Import (C, Create, "lv_group_create");
   pragma Import (C, Del, "lv_group_del");
   pragma Import (C, Add_Obj, "lv_group_add_obj");
   pragma Import (C, Remove_Obj, "lv_group_remove_obj");
   pragma Import (C, Focus_Obj, "lv_group_focus_obj");
   pragma Import (C, Focus_Next, "lv_group_focus_next");
   pragma Import (C, Focus_Prev, "lv_group_focus_prev");
   pragma Import (C, Focus_Freeze, "lv_group_focus_freeze");
   pragma Import (C, Send_Data, "lv_group_send_data");

end Lv.Group;
