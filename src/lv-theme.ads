private with System;

with Lv.Font;

package Lv.Theme is

   type Theme is private;

   No_Theme : constant Theme;

   subtype Hue_T is Uint16_T range 0 .. 360;

   --  Set a theme for the system.
   --  From now, all the created objects will use styles from this theme by default
   --  @param th pointer to theme (return value of: 'lv_theme_init_xxx()')
   procedure Set_Current (T : Theme);

   --  Get the current system theme.
   --  @return pointer to the current system theme. NULL if not set.
   function Get_Current return Theme;

   --  Initialize the material theme
   --  @param hue [0..360] hue value from HSV color space to define the theme's base color
   --  @param font pointer to a font (NULL to use the default)
   --  @return pointer to the initialized theme
   function Material_Init (Hue : Hue_T; Font : Lv.Font.Font) return Theme;

   --  Get a pointer to the theme
   --  @return pointer to the theme
   function Get_Material return Theme;

   --  Initialize the mono theme
   --  @param hue [0..360] hue value from HSV color space to define the theme's base color
   --  @param font pointer to a font (NULL to use the default)
   --  @return pointer to the initialized theme
   function Mono_Init (Hue : Hue_T; Font : Lv.Font.Font) return Theme;

   --  Get a pointer to the theme
   --  @return pointer to the theme
   function Get_Mono return Theme;

   --  Initialize the alien theme
   --  @param hue [0..360] hue value from HSV color space to define the theme's base color
   --  @param font pointer to a font (NULL to use the default)
   --  @return pointer to the initialized theme
   function Alien_Init (Hue : Hue_T; Font : Lv.Font.Font) return Theme;

   --  Get a pointer to the theme
   --  @return pointer to the theme
   function Get_Alien return Theme;

   --  Initialize the default theme
   --  @param hue [0..360] hue value from HSV color space to define the theme's base color
   --  @param font pointer to a font (NULL to use the default)
   --  @return pointer to the initialized theme
   function Default_Init (Hue : Hue_T; Font : Lv.Font.Font) return Theme;

   --  Get a pointer to the theme
   --  @return pointer to the theme
   function Get_Default return Theme;

   --  Initialize the nemo theme
   --  @param hue [0..360] hue value from HSV color space to define the theme's base color
   --  @param font pointer to a font (NULL to use the default)
   --  @return pointer to the initialized theme
   function Nemo_Init (Hue : Hue_T; Font : Lv.Font.Font) return Theme;

   --  Get a pointer to the theme
   --  @return pointer to the theme
   function Get_Nemo return Theme;

   --  Initialize the night theme
   --  @param hue [0..360] hue value from HSV color space to define the theme's base color
   --  @param font pointer to a font (NULL to use the default)
   --  @return pointer to the initialized theme
   function Night_Init (Hue : Hue_T; Font : Lv.Font.Font) return Theme;

   --  Get a pointer to the theme
   --  @return pointer to the theme
   function Get_Night return Theme;

   --  Initialize the zen theme
   --  @param hue [0..360] hue value from HSV color space to define the theme's base color
   --  @param font pointer to a font (NULL to use the default)
   --  @return pointer to the initialized theme
   function Zen_Init (Hue : Hue_T; Font : Lv.Font.Font) return Theme;

   --  Get a pointer to the theme
   --  @return pointer to the theme
   function Get_Zen return Theme;

private

   type Theme is new System.Address;
   No_Theme : constant Theme := Theme (System.Null_Address);

   pragma Import (C, Set_Current, "lv_theme_set_current");
   pragma Import (C, Get_Current, "lv_theme_get_current");
   pragma Import (C, Material_Init, "lv_theme_material_init");
   pragma Import (C, Get_Material, "lv_theme_get_material");
   pragma Import (C, Mono_Init, "lv_theme_mono_init");
   pragma Import (C, Get_Mono, "lv_theme_get_mono");
   pragma Import (C, Alien_Init, "lv_theme_alien_init");
   pragma Import (C, Get_Alien, "lv_theme_get_alien");
   pragma Import (C, Default_Init, "lv_theme_default_init");
   pragma Import (C, Get_Default, "lv_theme_get_default");
   pragma Import (C, Nemo_Init, "lv_theme_nemo_init");
   pragma Import (C, Get_Nemo, "lv_theme_get_nemo");
   pragma Import (C, Night_Init, "lv_theme_night_init");
   pragma Import (C, Get_Night, "lv_theme_get_night");
   pragma Import (C, Zen_Init, "lv_theme_zen_init");
   pragma Import (C, Get_Zen, "lv_theme_get_zen");

end Lv.Theme;
