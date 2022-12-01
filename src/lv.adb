package body Lv is

   --  Init. the 'lv' library.
   --
   --  Must be called before any lv_* functions, including the lv_mem_alloc
   --  used in Lv.Mem. So we automaticall call it during elaboration of this
   --  package.
   procedure LV_Init;
   pragma Import (C, LV_Init, "lv_init");

begin
   LV_Init;
end Lv;
