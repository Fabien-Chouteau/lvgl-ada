with Interfaces.C;
with System;

package Lv.Objx.Img is

   subtype Instance is Obj_T;

   --  Create an image objects
   --  @param par pointer to an object, it will be the parent of the new button
   --  @param copy pointer to a image object, if not NULL then the new object will be copied from it
   --  @return pointer to the created image
   function Create (Parent : Obj_T; Copy : Instance) return Instance;

   ----------------------
   -- Setter functions --
   ----------------------

   --  Set the pixel map to display by the image
   --  @param self pointer to an image object
   --  @param data the image data
   procedure Set_Src (Self : Instance; Data : System.Address);

   --  Enable the auto size feature.
   --  If enabled the object size will be same as the picture size.
   --  @param self pointer to an image
   --  @param autosize_en true: auto size enable, false: auto size disable
   procedure Set_Auto_Size (Self : Instance; Autosize : U_Bool);

   --  Set the style of an image
   --  @param self pointer to an image object
   --  @param style pointer to a style
   procedure Set_Style
     (Self  : Instance;
      Style : Lv.Style.Style);

   ----------------------
   -- Getter functions --
   ----------------------

   --  Get the source of the image
   --  @param self pointer to an image object
   --  @return the image source (symbol, file name or C array)
  function Src (Self : Instance) return System.Address;

   --  Get the name of the file set for an image
   --  @param self pointer to an image
   --  @return file name
   function File_Name
     (Self : Instance) return C_String_Ptr;

   --  Get the auto size enable attribute
   --  @param self pointer to an image
   --  @return true: auto size is enabled, false: auto size is disabled
   function Auto_Size (Self : Instance) return U_Bool;

   --  Get the style of an image object
   --  @param self pointer to an image object
   --  @return pointer to the image's style
   function Style
     (Self : Instance) return Lv.Style.Style;

   -------------
   -- Imports --
   -------------

   pragma Import (C, Create, "lv_img_create");
   pragma Import (C, Set_Src, "lv_img_set_src");
   pragma Import (C, Set_Auto_Size, "lv_img_set_auto_size");
   pragma Import (C, Set_Style, "lv_img_set_style");
   pragma Import (C, Src, "lv_img_get_src");
   pragma Import (C, File_Name, "lv_img_get_file_name");
   pragma Import (C, Auto_Size, "lv_img_get_auto_size");
   pragma Import (C, Style, "lv_img_get_style");

end Lv.Objx.Img;
