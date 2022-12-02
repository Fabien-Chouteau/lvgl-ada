with Lv.Style;
with Lv.Objx.Page;

package Lv.Objx.Tabview is

   subtype Instance is Obj_T;

   type Action_T is access function
     (Self   : access Instance;
      Tab_Id : Uint16_T) return Res_T;
   pragma Convention (C, Action_T);

   type Btns_Pos_T is (Pos_Top, Pos_Bottom);

   type Style_T is
     (Style_Bg,
      Style_Indic,
      Style_Btn_Bg,
      Style_Btn_Rel,
      Style_Btn_Pr,
      Style_Btn_Tgl_Rel,
      Style_Btn_Tgl_Pr);

   --  Create a Tab view object
   --  @param par pointer to an object, it will be the parent of the new tab
   --  @param copy pointer to a tab object, if not NULL then the new object will be copied from it
   --  @return pointer to the created tab
   function Create (Par : Obj_T; Copy : Instance) return Instance;

   --  Delete all children of the scrl object, without deleting scrl child.
   --  @param self pointer to an object
   procedure Clean (Self : Instance);

   --  Add a new tab with the given name
   --  @param self pointer to Tab view object where to ass the new tab
   --  @param name the text on the tab button
   --  @return pointer to the created page object (lv_page). You can create your content here
   function Add_Tab
     (Self : Instance;
      Name : C_String_Ptr)
      return Lv.Objx.Page.Instance;

   ----------------------
   -- Setter functions --
   ----------------------

   --  Set a new tab
   --  @param self pointer to Tab view object
   --  @param id index of a tab to load
   --  @param anim_en true: set with sliding animation; false: set immediately
   procedure Set_Tab_Act (Self : Instance; Id : Uint16_T; Anim_En : U_Bool);

   --  Set an action to call when a tab is loaded (Good to create content only if required)
   --  lv_tabview_get_act() still gives the current (old) tab (to remove content from here)
   --  @param self pointer to a tabview object
   --  @param action pointer to a function to call when a tab is loaded
   procedure Set_Tab_Load_Action
     (Self   : Instance;
      Action : Action_T);

   --  Enable horizontal sliding with touch pad
   --  @param self pointer to Tab view object
   --  @param en true: enable sliding; false: disable sliding
   procedure Set_Sliding (Self : Instance; En : U_Bool);

   --  Set the animation time of tab view when a new tab is loaded
   --  @param self pointer to Tab view object
   --  @param anim_time time of animation in milliseconds
   procedure Set_Anim_Time (Self : Instance; Anim_Time : Uint16_T);

   --  Set the style of a tab view
   --  @param self pointer to a tan view object
   --  @param type which style should be set
   --  @param style pointer to the new style
   procedure Set_Style
     (Self  : Instance;
      Typ   : Style_T;
      Style : Lv.Style.Style);

   --  Set the position of tab select buttons
   --  @param self pointer to a tan view object
   --  @param btns_pos which button position
   procedure Set_Btns_Pos (Self : Instance; Btns_Pos : Btns_Pos_T);

   ----------------------
   -- Getter functions --
   ----------------------

   --  Get the index of the currently active tab
   --  @param self pointer to Tab view object
   --  @return the active tab index
   function Tab_Act (Self : Instance) return Uint16_T;

   --  Get the number of tabs
   --  @param self pointer to Tab view object
   --  @return tab count
   function Tab_Count (Self : Instance) return Uint16_T;

   --  Get the page (content area) of a tab
   --  @param self pointer to Tab view object
   --  @param id index of the tab (>= 0)
   --  @return pointer to page (lv_page) object
   function Tab (Self : Instance; Id : Uint16_T)
                 return Lv.Objx.Page.Instance;

   --  Get the tab load action
   --  @param self pointer to a tabview object
   --  @param return the current tab load action
   function Tab_Load_Action (Self : Instance) return Action_T;

   --  Get horizontal sliding is enabled or not
   --  @param self pointer to Tab view object
   --  @return true: enable sliding; false: disable sliding
   function Sliding (Self : Instance) return U_Bool;

   --  Get the animation time of tab view when a new tab is loaded
   --  @param self pointer to Tab view object
   --  @return time of animation in milliseconds
   function Anim_Time (Self : Instance) return Uint16_T;

   --  Get a style of a tab view
   --  @param self pointer to a ab view object
   --  @param type which style should be get
   --  @return style pointer to a style
   function Style
     (Self       : Instance;
      Style_Type : Style_T) return Lv.Style.Style;

   --  Get position of tab select buttons
   --  @param self pointer to a ab view object
   function Btns_Pos (Self : Instance) return Btns_Pos_T;

   -------------
   -- Imports --
   -------------

   pragma Import (C, Create, "lv_tabview_create");
   pragma Import (C, Clean, "lv_tabview_clean");
   pragma Import (C, Add_Tab, "lv_tabview_add_tab");
   pragma Import (C, Set_Tab_Act, "lv_tabview_set_tab_act");
   pragma Import (C, Set_Tab_Load_Action, "lv_tabview_set_tab_load_action");
   pragma Import (C, Set_Sliding, "lv_tabview_set_sliding");
   pragma Import (C, Set_Anim_Time, "lv_tabview_set_anim_time");
   pragma Import (C, Set_Style, "lv_tabview_set_style");
   pragma Import (C, Set_Btns_Pos, "lv_tabview_set_btns_pos");
   pragma Import (C, Tab_Act, "lv_tabview_get_tab_act");
   pragma Import (C, Tab_Count, "lv_tabview_get_tab_count");
   pragma Import (C, Tab, "lv_tabview_get_tab");
   pragma Import (C, Tab_Load_Action, "lv_tabview_get_tab_load_action");
   pragma Import (C, Sliding, "lv_tabview_get_sliding");
   pragma Import (C, Anim_Time, "lv_tabview_get_anim_time");
   pragma Import (C, Style, "lv_tabview_get_style");
   pragma Import (C, Btns_Pos, "lv_tabview_get_btns_pos");

   for Btns_Pos_T'Size use 8;
   for Btns_Pos_T use (Pos_Top    => 0,
                       Pos_Bottom => 1);

   for Style_T'Size use 8;
   for Style_T use
     (Style_Bg          => 0,
      Style_Indic       => 1,
      Style_Btn_Bg      => 2,
      Style_Btn_Rel     => 3,
      Style_Btn_Pr      => 4,
      Style_Btn_Tgl_Rel => 5,
      Style_Btn_Tgl_Pr  => 6);

end Lv.Objx.Tabview;
