with Interfaces.C; use Interfaces.C;
with System;
with Interfaces.C.Extensions;
with Interfaces.C.Strings;

package Lv.Objx.Img is

   subtype Instance is Obj_T;

   function Create
     (Parent : Obj_T;
      Copy   : Instance) return Instance;  -- lv_img.h:61
   pragma Import (C, Create, "lv_img_create");

   procedure Set_Src (Self : Instance; Arg2 : System.Address);  -- lv_img.h:72
   pragma Import (C, Set_Src, "lv_img_set_src");

   procedure Set_File
     (Self : Instance;
      Fn   : Interfaces.C.Strings.chars_ptr);  -- lv_img.h:80
   pragma Import (C, Set_File, "lv_img_set_file");

   procedure Set_Auto_Size (Self : Instance; Arg2 : U_Bool);  -- lv_img.h:92
   pragma Import (C, Set_Auto_Size, "lv_img_set_auto_size");

   procedure Set_Style
     (Self  : Instance;
      Style : access Lv.Style.Style);  -- lv_img.h:99
   pragma Import (C, Set_Style, "lv_img_set_style");

   procedure Set_Upscale (Self : Instance; Upcale : U_Bool);  -- lv_img.h:109
   pragma Import (C, Set_Upscale, "lv_img_set_upscale");

   function Get_Src (Self : Instance) return System.Address;  -- lv_img.h:124
   pragma Import (C, Get_Src, "lv_img_get_src");

   function Get_File_Name
     (Self : Instance) return Interfaces.C.Strings.chars_ptr;  -- lv_img.h:131
   pragma Import (C, Get_File_Name, "lv_img_get_file_name");

   function Get_Auto_Size (Self : Instance) return U_Bool;  -- lv_img.h:138
   pragma Import (C, Get_Auto_Size, "lv_img_get_auto_size");

   function Get_Style
     (Self : Instance) return access Lv.Style.Style;  -- lv_img.h:145
   pragma Import (C, Get_Style, "lv_img_get_style");

   function Get_Upscale (Self : Instance) return U_Bool;  -- lv_img.h:155
   pragma Import (C, Get_Upscale, "lv_img_get_upscale");

end Lv.Objx.Img;
