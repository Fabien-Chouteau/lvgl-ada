with Lv.Area;
with System;

package Lv.Hal.Indev is

   type Indev_Type_T is (Type_None,
                         Type_Pointer,
                         Type_Keypad,
                         Type_Button,
                         Type_Encoder);

   type Indev_State_T is (State_Rel, State_Pr);


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

   --  Initialize an input device driver with default values.
   --  It is used to surly have known values in the fields ant not memory junk.
   --  After it you can set the fields.
   --  @param driver pointer to driver variable to initialize
   procedure Init_Drv (Driver : access Indev_Drv_T);

   --  Register an initialized input device driver.
   --  @param driver pointer to an initialized 'lv_indev_drv_t' variable (can be local variable)
   --  @return pointer to the new input device or NULL on error
   function Register
     (Driver : access Indev_Drv_T) return Indev_T;

   --  Get the next input device.
   --  @param indev pointer to the current input device. NULL to initialize.
   --  @return the next input devise or NULL if no more. Gives the first input device when the parameter is NULL
   function Next
     (Indev : access Indev_T) return access Indev_T;

   --  Read data from an input device.
   --  @param indev pointer to an input device
   --  @param data input device will write its data here
   --  @return false: no more data; true: there more data to read (buffered)
   function Read
     (Indev : access Indev_T;
      Data  : access Indev_Data_T) return U_Bool;

private

   type Indev_T is new System.Address;

   -------------
   -- Imports --
   -------------

   pragma Import (C, Init_Drv, "lv_indev_drv_init");
   pragma Import (C, Register, "lv_indev_drv_register");
   pragma Import (C, Next, "lv_indev_next");
   pragma Import (C, Read, "lv_indev_read");

   for Indev_Type_T'Size use 8;
   for Indev_Type_T use
     (Type_None    => 0,
      Type_Pointer => 1,
      Type_Keypad  => 2,
      Type_Button  => 3,
      Type_Encoder => 4);

   for Indev_State_T'Size use 8;
   for Indev_State_T use (State_Rel => 0, State_Pr => 1);

end Lv.Hal.Indev;
