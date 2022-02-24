with Lv.Hal.Indev;
with Lv.Objx;
with Lv.Area;
with Lv.Group;

package Lv.Indev is

   --  Initialize the display input device subsystem
   procedure Init;

   --  Get the currently processed input device. Can be used in action functions too.
   --  @return pointer to the currently processed input device or NULL if no input device processing right now
   function Get_Act return Lv.Hal.Indev.Indev_T;

   --  Get the type of an input device
   --  @param indev pointer to an input device
   --  @return the type of the input device from `lv_hal_indev_type_t` (`LV_INDEV_TYPE_...`)
   function Get_Type
     (Indev : Lv.Hal.Indev.Indev_T)
     return Lv.Hal.Indev.Indev_Type_T;

   --  Reset one or all input devices
   --  @param indev pointer to an input device to reset or NULL to reset all of them
   procedure Reset (Indev : Lv.Hal.Indev.Indev_T);

   --  Reset the long press state of an input device
   --  @param indev_proc pointer to an input device
   procedure Reset_Lpr (Indev_Proc : Lv.Hal.Indev.Indev_T);

   --  Enable input devices device by type
   --  @param type Input device type
   --  @param enable true: enable this type; false: disable this type
   procedure Enable (Type_P : Lv.Hal.Indev.Indev_Type_T;
                     Enable : U_Bool);  -- lv_indev.h:68

   --  Set a cursor for a pointer input device (for LV_INPUT_TYPE_POINTER and LV_INPUT_TYPE_BUTTON)
   --  @param indev pointer to an input device
   --  @param cur_obj pointer to an object to be used as cursor
   procedure Set_Cursor (Indev   : Lv.Hal.Indev.Indev_T;
                         Cur_Obj : Lv.Objx.Obj_T);

   --  Set a destination group for a keypad input device (for LV_INDEV_TYPE_KEYPAD)
   --  @param indev pointer to an input device
   --  @param group point to a group
   procedure Set_Group (Indev : LV.HAL.Indev.Indev_T;
                        Group : LV.Group.Instance);

   --  Set the an array of points for LV_INDEV_TYPE_BUTTON.
   --  These points will be assigned to the buttons to press a specific point on the screen
   --  @param indev pointer to an input device
   --  @param group point to a group
   procedure Set_Button_Points
     (Indev : Lv.Hal.Indev.Indev_T;
      Group : access Lv.Area.Point_T);

   --  Get the last point of an input device (for LV_INDEV_TYPE_POINTER and LV_INDEV_TYPE_BUTTON)
   --  @param indev pointer to an input device
   --  @param point pointer to a point to store the result
   procedure Get_Point
     (Indev : Lv.Hal.Indev.Indev_T;
      Point : access Lv.Area.Point_T);

   --  Get the last key of an input device (for LV_INDEV_TYPE_KEYPAD)
   --  @param indev pointer to an input device
   --  @return the last pressed key (0 on error)
   function Get_Key
     (Indev : Lv.Hal.Indev.Indev_T) return Uint32_T;

   --  Check if there is dragging with an input device or not (for LV_INDEV_TYPE_POINTER and LV_INDEV_TYPE_BUTTON)
   --  @param indev pointer to an input device
   --  @return true: drag is in progress
   function Is_Dragging
     (Indev: Lv.Hal.Indev.Indev_T) return U_Bool;

   --  Get the vector of dragging of an input device (for LV_INDEV_TYPE_POINTER and LV_INDEV_TYPE_BUTTON)
   --  @param indev pointer to an input device
   --  @param point pointer to a point to store the vector
   procedure Get_Vect
     (Indev : Lv.Hal.Indev.Indev_T;
      Point : access Lv.Area.Point_T);

   --  Get elapsed time since last press
   --  @param indev pointer to an input device (NULL to get the overall smallest inactivity)
   --  @return Elapsed ticks (milliseconds) since last press
   function Get_Inactive_Time
     (Indev : Lv.Hal.Indev.Indev_T) return Uint32_T;

   --  Do nothing until the next release
   --  @param indev pointer to an input device
   procedure Wait_Release (Indev : Lv.Hal.Indev.Indev_T);

   -------------
   -- Imports --
   -------------

   pragma Import (C, Init, "lv_indev_init");
   pragma Import (C, Get_Act, "lv_indev_get_act");
   pragma Import (C, Get_Type, "lv_indev_get_type");
   pragma Import (C, Reset, "lv_indev_reset");
   pragma Import (C, Reset_Lpr, "lv_indev_reset_lpr");
   pragma Import (C, Enable, "lv_indev_enable");
   pragma Import (C, Set_Cursor, "lv_indev_set_cursor");
   pragma Import (C, Set_Group, "lv_indev_set_group");
   pragma Import (C, Set_Button_Points, "lv_indev_set_button_points");
   pragma Import (C, Get_Point, "lv_indev_get_point");
   pragma Import (C, Get_Key, "lv_indev_get_key");
   pragma Import (C, Is_Dragging, "lv_indev_is_dragging");
   pragma Import (C, Get_Vect, "lv_indev_get_vect");
   pragma Import (C, Get_Inactive_Time, "lv_indev_get_inactive_time");
   pragma Import (C, Wait_Release, "lv_indev_wait_release");

end Lv.Indev;
