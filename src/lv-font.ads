pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions;
with System;

package Lv.Font is

   type Font is private;

   No_Font : constant Font;

   procedure Init;
   pragma Import (C, Init, "lv_font_init");

   procedure Add (Child : Font; Parent : Font);
   pragma Import (C, Add, "lv_font_add");

   function Is_Monospace (F : Font; Letter : Uint32_T) return U_Bool;
   pragma Import (C, Is_Monospace, "lv_font_is_monospace");

   function Get_Bitmap (F : Font; Letter : Uint32_T) return access Uint8_T;
   pragma Import (C, Get_Bitmap, "lv_font_get_bitmap");

   function Get_Width (F : Font; Letter : Uint32_T) return Uint8_T;
   pragma Import (C, Get_Width, "lv_font_get_width");

   function Get_Real_Width (F : Font; Letter : Uint32_T) return Uint8_T;
   pragma Import (C, Get_Real_Width, "lv_font_get_real_width");

   function Get_Height (F : Font) return Uint8_T;
   pragma Import (C, Get_Height, "lv_font_get_height_inline");

   function Get_Bpp (F : Font; Letter : Uint32_T) return Uint8_T;
   pragma Import (C, Get_Bpp, "lv_font_get_bpp");

   function Get_Bitmap_Continuous
     (F              : Font;
      Unicode_Letter : Uint32_T) return access Uint8_T;
   pragma Import (C, Get_Bitmap_Continuous, "lv_font_get_bitmap_continuous");

   function Get_Bitmap_Sparse
     (F              : Font;
      Unicode_Letter : Uint32_T) return access Uint8_T;
   pragma Import (C, Get_Bitmap_Sparse, "lv_font_get_bitmap_sparse");

   function Get_Width_Continuous
     (F              : Font;
      Unicode_Letter : Uint32_T) return Int16_T;
   pragma Import (C, Get_Width_Continuous, "lv_font_get_width_continuous");

   function Get_Width_Sparse
     (F              : Font;
      Unicode_Letter : Uint32_T) return Int16_T;
   pragma Import (C, Get_Width_Sparse, "lv_font_get_width_sparse");

private

   type Font is new System.Address;
   No_Font : constant Font := Font (System.Null_Address);

end Lv.Font;
