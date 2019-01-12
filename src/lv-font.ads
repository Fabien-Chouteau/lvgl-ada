with System;

package Lv.Font is

   type Font is private;

   No_Font : constant Font;

   --  Initialize the fonts
   procedure Init;

   --  Create a pair from font name and font dsc. get function. After it 'font_get' can be used for this font
   --  @param child pointer to a font to join to the 'parent'
   --  @param parent pointer to a font. 'child' will be joined here
   procedure Add (Child : Font; Parent : Font);

   --  Tells if font which contains `letter` is monospace or not
   --  @param font_p point to font
   --  @param letter an UNICODE character code
   --  @return true: the letter is monospace; false not monospace
   function Is_Monospace (F : Font; Letter : Uint32_T) return U_Bool;

   --  Return with the bitmap of a font.
   --  @param font_p pointer to a font
   --  @param letter an UNICODE character code
   --  @return  pointer to the bitmap of the letter
   function Bitmap (F : Font; Letter : Uint32_T) return access Uint8_T;

   --  Get the width of a letter in a font. If `monospace` is set then return with it.
   --  @param font_p pointer to a font
   --  @param letter an UNICODE character code
   --  @return the width of a letter
   function Width (F : Font; Letter : Uint32_T) return Uint8_T;

   --  Get the width of the letter without overwriting it with the `monospace` attribute
   --  @param font_p pointer to a font
   --  @param letter an UNICODE character code
   --  @return the width of a letter
   function Real_Width (F : Font; Letter : Uint32_T) return Uint8_T;

   --  Get the height of a font
   --  @param font_p pointer to a font
   --  @return the height of a font
   function Height (F : Font) return Uint8_T;

   --  Get the bit-per-pixel of font
   --  @param font pointer to font
   --  @param letter a letter from font (font extensions can have different bpp)
   --  @return bpp of the font (or font extension)
   function Bpp (F : Font; Letter : Uint32_T) return Uint8_T;

   --  Generic bitmap get function used in 'font->get_bitmap' when the font contains all characters in the range
   --  @param font pointer to font
   --  @param unicode_letter an unicode letter which bitmap should be get
   --  @return pointer to the bitmap or NULL if not found
   function Bitmap_Continuous
     (F              : Font;
      Unicode_Letter : Uint32_T) return access Uint8_T;

   --  Generic bitmap get function used in 'font->get_bitmap' when the font NOT contains all characters in the range (sparse)
   --  @param font pointer to font
   --  @param unicode_letter an unicode letter which bitmap should be get
   --  @return pointer to the bitmap or NULL if not found
   function Bitmap_Sparse
     (F              : Font;
      Unicode_Letter : Uint32_T) return access Uint8_T;

   --  Generic glyph width get function used in 'font->get_width' when the font contains all characters in the range
   --  @param font pointer to font
   --  @param unicode_letter an unicode letter which width should be get
   --  @return width of the gylph or -1 if not found
   function Width_Continuous
     (F              : Font;
      Unicode_Letter : Uint32_T) return Int16_T;

   --  Generic glyph width get function used in 'font->get_bitmap' when the font NOT contains all characters in the range (sparse)
   --  @param font pointer to font
   --  @param unicode_letter an unicode letter which width should be get
   --  @return width of the glyph or -1 if not found
   function Width_Sparse
     (F              : Font;
      Unicode_Letter : Uint32_T) return Int16_T;

private

   type Font is new System.Address;
   No_Font : constant Font := Font (System.Null_Address);

   -------------
   -- Imports --
   -------------

   pragma Import (C, Init, "lv_font_init");
   pragma Import (C, Add, "lv_font_add");
   pragma Import (C, Is_Monospace, "lv_font_is_monospace");
   pragma Import (C, Bitmap, "lv_font_get_bitmap");
   pragma Import (C, Width, "lv_font_get_width");
   pragma Import (C, Real_Width, "lv_font_get_real_width");
   pragma Import (C, Height, "lv_font_get_height_inline");
   pragma Import (C, Bpp, "lv_font_get_bpp");
   pragma Import (C, Bitmap_Continuous, "lv_font_get_bitmap_continuous");
   pragma Import (C, Bitmap_Sparse, "lv_font_get_bitmap_sparse");
   pragma Import (C, Width_Continuous, "lv_font_get_width_continuous");
   pragma Import (C, Width_Sparse, "lv_font_get_width_sparse");

end Lv.Font;
