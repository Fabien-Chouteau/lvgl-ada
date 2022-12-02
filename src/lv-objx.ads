with Lv.Area;
with Lv.Style;
with Lv.Color; use Lv.Color;

package Lv.Objx is

   type Obj_T is new System.Address;

   No_Obj : constant Obj_T := Obj_T (System.Null_Address);

   Anim_In       : constant := 16#00#;
   Anim_Out      : constant := 16#80#;
   Anim_Dir_Mask : constant := 16#80#;

   Lv_Max_Ancestor_Num : constant := 8;

   Res_Inv : constant := 0;
   Res_Ok  : constant := 1;

   type Align_T is
     (Align_Center,
      Align_In_Top_Left,
      Align_In_Top_Mid,
      Align_In_Top_Right,
      Align_In_Bottom_Left,
      Align_In_Bottom_Mid,
      Align_In_Bottom_Right,
      Align_In_Left_Mid,
      Align_In_Right_Mid,
      Align_Out_Top_Left,
      Align_Out_Top_Mid,
      Align_Out_Top_Right,
      Align_Out_Bottom_Left,
      Align_Out_Bottom_Mid,
      Align_Out_Bottom_Right,
      Align_Out_Left_Top,
      Align_Out_Left_Mid,
      Align_Out_Left_Bottom,
      Align_Out_Right_Top,
      Align_Out_Right_Mid,
      Align_Out_Right_Bottom) with
        Size => 8;

   for Align_T use
     (Align_Center           => 0,
      Align_In_Top_Left      => 1,
      Align_In_Top_Mid       => 2,
      Align_In_Top_Right     => 3,
      Align_In_Bottom_Left   => 4,
      Align_In_Bottom_Mid    => 5,
      Align_In_Bottom_Right  => 6,
      Align_In_Left_Mid      => 7,
      Align_In_Right_Mid     => 8,
      Align_Out_Top_Left     => 9,
      Align_Out_Top_Mid      => 10,
      Align_Out_Top_Right    => 11,
      Align_Out_Bottom_Left  => 12,
      Align_Out_Bottom_Mid   => 13,
      Align_Out_Bottom_Right => 14,
      Align_Out_Left_Top     => 15,
      Align_Out_Left_Mid     => 16,
      Align_Out_Left_Bottom  => 17,
      Align_Out_Right_Top    => 18,
      Align_Out_Right_Mid    => 19,
      Align_Out_Right_Bottom => 20);

   subtype Design_Mode_T is Uint8_T;

   type Design_Func_T is access function
     (Arg1 : System.Address;
      Arg2 : access constant Lv.Area.Area_T;
      Arg3 : Design_Mode_T) return U_Bool;
   pragma Convention (C, Design_Func_T);

   subtype Res_T is Uint8_T;

   subtype Signal_T is Uint8_T;

   type Signal_Func_T is access function
     (Arg1 : System.Address;
      Arg2 : Signal_T;
      Arg3 : System.Address) return Res_T;
   pragma Convention (C, Signal_Func_T);

   type Action_Func_T is access function (Arg1 : Obj_T) return Res_T;
   pragma Convention (C, Action_Func_T);

   subtype Protect_T is Uint8_T;

   type Obj_Type_T_C_Type_Array is
     array (0 .. 7) of C_String_Ptr;
   type Obj_Type_T is record
      C_Type : Obj_Type_T_C_Type_Array;
   end record;
   pragma Convention (C_Pass_By_Copy, Obj_Type_T);

   subtype Anim_Builtin_T is Uint8_T;
   Anim_None         : constant Anim_Builtin_T := 0;
   Anim_Float_Top    : constant Anim_Builtin_T := 1;
   Anim_Float_Left   : constant Anim_Builtin_T := 2;
   Anim_Float_Bottom : constant Anim_Builtin_T := 3;
   Anim_Float_Right  : constant Anim_Builtin_T := 4;
   Anim_Grow_H       : constant Anim_Builtin_T := 5;
   Anim_Grow_V       : constant Anim_Builtin_T := 6;

   -----------------------
   -- Create and delete --
   -----------------------

   --  Create a basic object
   --  @param parent pointer to a parent object.
   --                   If NULL then a screen will be created
   --  @param copy pointer to a base object, if not NULL then the new object will be copied from it
   --  @return pointer to the new object
   function Create (Parent : Obj_T; Copy : Obj_T) return Obj_T;

   --  Delete 'obj' and all of its children
   --  @param self pointer to an object to delete
   --  @return LV_RES_INV because the object is deleted
   function Del (Self : Obj_T) return Res_T;

   --  Delete all children of an object
   --  @param self pointer to an object
   procedure Clean (Self : Obj_T);

   --  Mark the object as invalid therefore its current position will be redrawn by 'lv_refr_task'
   --  @param self pointer to an object
   procedure Invalidate (Self : Obj_T);

   --  Load a new screen
   --  @param scr pointer to a screen
   procedure Scr_Load (Scr : Obj_T);

   ----------------------
   -- Setter functions --
   ----------------------

   -------------------------
   -- Parent/children set --
   -------------------------

   --  Set a new parent for an object. Its relative position will be the same.
   --  @param self pointer to an object. Can't be a screen.
   --  @param parent pointer to the new parent object. (Can't be NULL)
   procedure Set_Parent (Self : Obj_T; Parent : Obj_T);

   --------------------
   -- Coordinate set --
   --------------------

   --  Set relative the position of an object (relative to the parent)
   --  @param self pointer to an object
   --  @param x new distance from the left side of the parent
   --  @param y new distance from the top of the parent
   procedure Set_Pos
     (Self : Obj_T;
      X    : Lv.Area.Coord_T;
      Y    : Lv.Area.Coord_T);

   --  Set the x coordinate of a object
   --  @param self pointer to an object
   --  @param x new distance from the left side from the parent
   procedure Set_X (Self : Obj_T; X : Lv.Area.Coord_T);

   --  Set the y coordinate of a object
   --  @param self pointer to an object
   --  @param y new distance from the top of the parent
   procedure Set_Y (Self : Obj_T; Y : Lv.Area.Coord_T);

   --  Set the size of an object
   --  @param self pointer to an object
   --  @param w new width
   --  @param h new height
   procedure Set_Size (Self : Obj_T; X : Lv.Area.Coord_T; Y : Lv.Area.Coord_T);

   --  Set the width of an object
   --  @param self pointer to an object
   --  @param w new width
   procedure Set_Width (Self : Obj_T; W : Lv.Area.Coord_T);

   --  Set the height of an object
   --  @param self pointer to an object
   --  @param h new height
   procedure Set_Height (Self : Obj_T; H : Lv.Area.Coord_T);

   --  Align an object to an other object.
   --  @param self pointer to an object to align
   --  @param base pointer to an object (if NULL the parent is used). 'obj' will be aligned to it.
   --  @param align type of alignment (see 'lv_align_t' enum)
   --  @param x_mod x coordinate shift after alignment
   --  @param y_mod y coordinate shift after alignment
   procedure Align
     (Self  : Obj_T;
      Base  : Obj_T;
      Align : Align_T;
      X_Mod : Lv.Area.Coord_T;
      Y_Mod : Lv.Area.Coord_T);

   --------------------
   -- Appearance set --
   --------------------

   --  Set a new style for an object
   --  @param self pointer to an object
   --  @param style_p pointer to the new style
   procedure Set_Style (Self : Obj_T; Style_P : Lv.Style.Style);

   --  Notify an object about its style is modified
   --  @param obj pointer to an object
   procedure Refresh_Style (Self : Obj_T);

   --  Notify all object if a style is modified
   --  @param style pointer to a style. Only the objects with this style will be notified
   --                (NULL to notify all objects)
   procedure Report_Style_Mod (Style_P : Lv.Style.Style);

   -------------------
   -- Attribute set --
   -------------------

   --  Hide an object. It won't be visible and clickable.
   --  @param self pointer to an object
   --  @param en true: hide the object
   procedure Set_Hidden (Self : Obj_T; En: U_Bool);

   --  Enable or disable the clicking of an object
   --  @param self pointer to an object
   --  @param en true: make the object clickable
   procedure Set_Click (Self : Obj_T; En : U_Bool);

   --  Enable to bring this object to the foreground if it
   --  or any of its children is clicked
   --  @param self pointer to an object
   --  @param en true: enable the auto top feature
   procedure Set_Top (Self : Obj_T; En : U_Bool);

   --  Enable the dragging of an object
   --  @param self pointer to an object
   --  @param en true: make the object dragable
   procedure Set_Drag (Self : Obj_T; En : U_Bool);

   --  Enable the throwing of an object after is is dragged
   --  @param self pointer to an object
   --  @param en true: enable the drag throw
   procedure Set_Drag_Throw (Self : Obj_T; En : U_Bool);

   --  Enable to use parent for drag related operations.
   --  If trying to drag the object the parent will be moved instead
   --  @param self pointer to an object
   --  @param en true: enable the 'drag parent' for the object
   procedure Set_Drag_Parent (Self : Obj_T; En : U_Bool);

   --  Set editable parameter Used by groups and keyboard/encoder control.
   --  Editable object has something inside to choose (the elements of a list)
   --  @param self pointer to an object
   --  @param en true: enable editing
   procedure Set_Editable (Self : Obj_T; En : U_Bool);

   --  Set the opa scale enable parameter (required to set opa_scale with `lv_obj_set_opa_scale()`)
   --  @param self pointer to an object
   --  @param en true: opa scaling is enabled for this object and all children; false: no opa scaling
   procedure Set_Opa_Scale_Enable (Self : Obj_T; En : U_Bool);

   --  Set the opa scale of an object
   --  @param self pointer to an object
   --  @param opa_scale a factor to scale down opacity [0..255]
   procedure Set_Opa_Scale (Self : Obj_T; Opa_Scale : Lv.Color.Opa_T);

   --  Set a bit or bits in the protect filed
   --  @param self pointer to an object
   --  @param prot 'OR'-ed values from `lv_protect_t`
   procedure Set_Protect (Self : Obj_T; Prot : Protect_T);

   --  Clear a bit or bits in the protect filed
   --  @param self pointer to an object
   --  @param prot 'OR'-ed values from `lv_protect_t`
   procedure Clear_Protect (Self : Obj_T; Prot : Protect_T);

   --  Set the signal function of an object.
   --  Always call the previous signal function in the new.
   --  @param self pointer to an object
   --  @param fp the new signal function
   procedure Set_Signal_Func (Self : Obj_T; Fp : Signal_Func_T);

   --  Set a new design function for an object
   --  @param self pointer to an object
   --  @param fp the new design function
   procedure Set_Design_Func (Self : Obj_T; Fp : Design_Func_T);

   ---------------
   -- Other set --
   ---------------

   --  Allocate a new ext. data for an object
   --  @param self pointer to an object
   --  @param ext_size the size of the new ext. data
   --  @return pointer to the allocated ext
   function Allocate_Ext_Attr
     (Self     : Obj_T;
      Ext_Size : Uint16_T) return System.Address;

   --  Send a 'LV_SIGNAL_REFR_EXT_SIZE' signal to the object
   --  @param self pointer to an object
   procedure Refresh_Ext_Size (Self : Obj_T);

   --  Set an application specific number for an object.
   --  It can help to identify objects in the application.
   --  @param self pointer to an object
   --  @param free_num the new free number
   procedure Set_Free_Num (Self : Obj_T; Free_Num : Uint32_T);

   --  Set an application specific  pointer for an object.
   --  It can help to identify objects in the application.
   --  @param self pointer to an object
   --  @param free_p the new free pinter
   procedure Set_Free_Ptr (Self : Obj_T; Free_P : System.Address);

   --  Animate an object
   --  @param self pointer to an object to animate
   --  @param type_p type of animation from 'lv_anim_builtin_t'. 'OR' it with ANIM_IN or ANIM_OUT
   --  @param time time of animation in milliseconds
   --  @param delay_p delay before the animation in milliseconds
   --  @param cb a function to call when the animation is ready
   procedure Animate
     (Self    : Obj_T;
      Type_P  : Anim_Builtin_T;
      Time    : Uint16_T;
      Delay_P : Uint16_T;
      Cb      : access procedure (Arg1 : Obj_T));

   ----------------------
   -- Getter functions --
   ----------------------

   ----------------
   -- Screen get --
   ----------------

   --  Return with a pointer to the active screen
   --  @return pointer to the active screen object (loaded by 'lv_scr_load()')
   function Scr_Act return Obj_T;

   --  Return with the top layer. (Same on every screen and it is above the normal screen layer)
   --  @return pointer to the top layer object  (transparent screen sized lv_obj)
   function Layer_Top return Obj_T;

   --  Return with the system layer. (Same on every screen and it is above the all other layers)
   --  It is used for example by the cursor
   --  @return pointer to the system layer object (transparent screen sized lv_obj)
   function Layer_Sys return Obj_T;

   --  Return with the screen of an object
   --  @param obj pointer to an object
   --  @return pointer to a screen
   function Screen (Arg1 : Obj_T) return Obj_T;

   -------------------------
   -- Parent/children get --
   -------------------------

   --  Returns with the parent of an object
   --  @param self pointer to an object
   --  @return pointer to the parent of  'obj'
   function Parent (Self : Obj_T) return Obj_T;

   --  Iterate through the children of an object (start from the "youngest, lastly created")
   --  @param self pointer to an object
   --  @param child NULL at first call to get the next children
   --                   and the previous return value later
   --  @return the child after 'act_child' or NULL if no more child
   function Child (Self : Obj_T; Child : Obj_T) return Obj_T;

   --  Iterate through the children of an object (start from the "oldest", firstly created)
   --  @param self pointer to an object
   --  @param child NULL at first call to get the next children
   --                   and the previous return value later
   --  @return the child after 'act_child' or NULL if no more child
   function Child_Back (Self : Obj_T; Child : Obj_T) return Obj_T;

   --  Count the children of an object (only children directly on 'obj')
   --  @param self pointer to an object
   --  @return children number of 'obj'
   function Count_Children (Self : Obj_T) return Uint16_T;

   --------------------
   -- Coordinate get --
   --------------------

   --  Copy the coordinates of an object to an area
   --  @param self pointer to an object
   --  @param cords_p pointer to an area to store the coordinates
   procedure Coords (Self : Obj_T; Cords_P : access Lv.Area.Area_T);

   --  Get the x coordinate of object
   --  @param self pointer to an object
   --  @return distance of 'obj' from the left side of its parent
   function X (Self : Obj_T) return Lv.Area.Coord_T;

   --  Get the y coordinate of object
   --  @param self pointer to an object
   --  @return distance of 'obj' from the top of its parent
   function Y (Self : Obj_T) return Lv.Area.Coord_T;

   --  Get the width of an object
   --  @param self pointer to an object
   --  @return the width
   function Width (Self : Obj_T) return Lv.Area.Coord_T;

   --  Get the height of an object
   --  @param self pointer to an object
   --  @return the height
   function Height (Self : Obj_T) return Lv.Area.Coord_T;

   --  Get the extended size attribute of an object
   --  @param self pointer to an object
   --  @return the extended size attribute
   function Ext_Size (Self : Obj_T) return Lv.Area.Coord_T;

   --------------------
   -- Appearance get --
   --------------------

   --  Get the style pointer of an object (if NULL get style of the parent)
   --  @param self pointer to an object
   --  @return pointer to a style
   function Style (Self : Obj_T) return Lv.Style.Style;

   -------------------
   -- Attribute get --
   -------------------

   --  Get the hidden attribute of an object
   --  @param self pointer to an object
   --  @return true: the object is hidden
   function Hidden (Self : Obj_T) return U_Bool;

   --  Get the click enable attribute of an object
   --  @param self pointer to an object
   --  @return true: the object is clickable
   function Click (Self : Obj_T) return U_Bool;

   --  Get the top enable attribute of an object
   --  @param self pointer to an object
   --  @return true: the auto top feture is enabled
   function Top (Self : Obj_T) return U_Bool;

   --  Get the drag enable attribute of an object
   --  @param self pointer to an object
   --  @return true: the object is dragable
   function Drag (Self : Obj_T) return U_Bool;

   --  Get the drag thow enable attribute of an object
   --  @param self pointer to an object
   --  @return true: drag throw is enabled
   function Drag_Throw (Self : Obj_T) return U_Bool;

   --  Get the drag parent attribute of an object
   --  @param self pointer to an object
   --  @return true: drag parent is enabled
   function Drag_Parent (Self : Obj_T) return U_Bool;

   --  Get the opa scale parameter of an object
   --  @param self pointer to an object
   --  @return opa scale [0..255]
   function Opa_Scale (Self : Obj_T) return Lv.Color.Opa_T;

   --  Get the protect field of an object
   --  @param self pointer to an object
   --  @return protect field ('OR'ed values of `lv_protect_t`)
   function Protect (Self : Obj_T) return Uint8_T;

   --  Check at least one bit of a given protect bitfield is set
   --  @param self pointer to an object
   --  @param prot protect bits to test ('OR'ed values of `lv_protect_t`)
   --  @return false: none of the given bits are set, true: at least one bit is set
   function Is_Protected (Self : Obj_T; Prot : Protect_T) return U_Bool;

   --  Get the signal function of an object
   --  @param self pointer to an object
   --  @return the signal function
   function Signal_Func (Self : Obj_T) return Signal_Func_T;

   --  Get the design function of an object
   --  @param self pointer to an object
   --  @return the design function
   function Design_Func (Self : Obj_T) return Design_Func_T;

   ---------------
   -- Other get --
   ---------------

   --  Get the ext pointer
   --  @param self pointer to an object
   --  @return the ext pointer but not the dynamic version
   --          Use it as ext->data1, and NOT da(ext)->data1
   function Ext_Attr (Self : Obj_T) return System.Address;

   --  Get object's and its ancestors type. Put their name in `type_buf` starting with the current type.
   --  E.g. buf.type[0]="lv_btn", buf.type[1]="lv_cont", buf.type[2]="lv_obj"
   --  @param self pointer to an object which type should be get
   --  @param buf pointer to an `lv_obj_type_t` buffer to store the types
   procedure Obj_Type (Self : Obj_T; Buf : access Obj_Type_T);

   --  Get the free number
   --  @param self pointer to an object
   --  @return the free number
   function Free_Num (Self : Obj_T) return Uint32_T;

   --  Get the free pointer
   --  @param self pointer to an object
   --  @return the free pointer
   function Free_Ptr (Self : Obj_T) return System.Address;

   --  Get the group of the object
   --  @param self pointer to an object
   --  @return the pointer to group of the object
   function Group (Self : Obj_T) return System.Address;

   --  Tell whether the ohe object is the focused object of a group or not.
   --  @param self pointer to an object
   --  @return true: the object is focused, false: the object is not focused or not in a group
   function Is_Focused (Self : Obj_T) return U_Bool;

   -------------
   -- Imports --
   -------------

   pragma Import (C, Create, "lv_obj_create");
   pragma Import (C, Del, "lv_obj_del");
   pragma Import (C, Clean, "lv_obj_clean");
   pragma Import (C, Invalidate, "lv_obj_invalidate");
   pragma Import (C, Scr_Load, "lv_scr_load");
   pragma Import (C, Set_Parent, "lv_obj_set_parent");
   pragma Import (C, Set_Pos, "lv_obj_set_pos");
   pragma Import (C, Set_X, "lv_obj_set_x");
   pragma Import (C, Set_Y, "lv_obj_set_y");
   pragma Import (C, Set_Size, "lv_obj_set_size");
   pragma Import (C, Set_Width, "lv_obj_set_width");
   pragma Import (C, Set_Height, "lv_obj_set_height");
   pragma Import (C, Align, "lv_obj_align");
   pragma Import (C, Set_Style, "lv_obj_set_style");
   pragma Import (C, Refresh_Style, "lv_obj_refresh_style");
   pragma Import (C, Report_Style_Mod, "lv_obj_report_style_mod");
   pragma Import (C, Set_Hidden, "lv_obj_set_hidden");
   pragma Import (C, Set_Click, "lv_obj_set_click");
   pragma Import (C, Set_Top, "lv_obj_set_top");
   pragma Import (C, Set_Drag, "lv_obj_set_drag");
   pragma Import (C, Set_Drag_Throw, "lv_obj_set_drag_throw");
   pragma Import (C, Set_Drag_Parent, "lv_obj_set_drag_parent");
   pragma Import (C, Set_Editable, "lv_obj_set_editable");
   pragma Import (C, Set_Opa_Scale_Enable, "lv_obj_set_opa_scale_enable");
   pragma Import (C, Set_Opa_Scale, "lv_obj_set_opa_scale");
   pragma Import (C, Set_Protect, "lv_obj_set_protect");
   pragma Import (C, Clear_Protect, "lv_obj_clear_protect");
   pragma Import (C, Set_Signal_Func, "lv_obj_set_signal_func");
   pragma Import (C, Set_Design_Func, "lv_obj_set_design_func");
   pragma Import (C, Allocate_Ext_Attr, "lv_obj_allocate_ext_attr");
   pragma Import (C, Refresh_Ext_Size, "lv_obj_refresh_ext_size");
   pragma Import (C, Set_Free_Num, "lv_obj_set_free_num");
   pragma Import (C, Set_Free_Ptr, "lv_obj_set_free_ptr");
   pragma Import (C, Animate, "lv_obj_animate");
   pragma Import (C, Scr_Act, "lv_scr_act");
   pragma Import (C, Layer_Top, "lv_layer_top");
   pragma Import (C, Layer_Sys, "lv_layer_sys");
   pragma Import (C, Screen, "lv_obj_get_screen");
   pragma Import (C, Parent, "lv_obj_get_parent");
   pragma Import (C, Child, "lv_obj_get_child");
   pragma Import (C, Child_Back, "lv_obj_get_child_back");
   pragma Import (C, Count_Children, "lv_obj_count_children");
   pragma Import (C, Coords, "lv_obj_get_coords");
   pragma Import (C, X, "lv_obj_get_x");
   pragma Import (C, Y, "lv_obj_get_y");
   pragma Import (C, Width, "lv_obj_get_width");
   pragma Import (C, Height, "lv_obj_get_height");
   pragma Import (C, Ext_Size, "lv_obj_get_ext_size");
   pragma Import (C, Style, "lv_obj_get_style");
   pragma Import (C, Hidden, "lv_obj_get_hidden");
   pragma Import (C, Click, "lv_obj_get_click");
   pragma Import (C, Top, "lv_obj_get_top");
   pragma Import (C, Drag, "lv_obj_get_drag");
   pragma Import (C, Drag_Throw, "lv_obj_get_drag_throw");
   pragma Import (C, Drag_Parent, "lv_obj_get_drag_parent");
   pragma Import (C, Opa_Scale, "lv_obj_get_opa_scale");
   pragma Import (C, Protect, "lv_obj_get_protect");
   pragma Import (C, Is_Protected, "lv_obj_is_protected");
   pragma Import (C, Signal_Func, "lv_obj_get_signal_func");
   pragma Import (C, Design_Func, "lv_obj_get_design_func");
   pragma Import (C, Ext_Attr, "lv_obj_get_ext_attr");
   pragma Import (C, Obj_Type, "lv_obj_get_type");
   pragma Import (C, Free_Num, "lv_obj_get_free_num");
   pragma Import (C, Free_Ptr, "lv_obj_get_free_ptr");
   pragma Import (C, Group, "lv_obj_get_group");
   pragma Import (C, Is_Focused, "lv_obj_is_focused");

end Lv.Objx;
