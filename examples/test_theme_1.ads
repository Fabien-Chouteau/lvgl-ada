with LV.Theme;

package Test_Theme_1 is
   procedure Init (T : LV.Theme.Theme);
   pragma Export (C, Init, "ada_test_theme_1");
end Test_Theme_1;
