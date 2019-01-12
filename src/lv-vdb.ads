with Lv.Area;
with Lv.Color;
with System;

package Lv.Vdb is

   type Vdb_T is record
      Area : aliased Lv.Area.Area_T;
      Buf  : access Lv.Color.Color_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Vdb_T);

   --  Get the 'vdb' variable or allocate one in LV_VDB_DOUBLE mode
   --  @return pointer to a 'vdb' variable
   function Get return access Vdb_T;

   --  Flush the content of the vdb
   procedure Flush;

   --  Set the address of VDB buffer(s) manually. To use this set `LV_VDB_ADR` (and `LV_VDB2_ADR`) to `LV_VDB_ADR_INV` in `lv_conf.h`.
   --  It should be called before `lv_init()`. The size of the buffer should be: `LV_VDB_SIZE_IN_BYTES`
   --  @param buf1 address of the VDB.
   --  @param buf2 address of the second buffer. `NULL` if `LV_VDB_DOUBLE  0`
   procedure Set_Adr (Arg1 : System.Address; Arg2 : System.Address);

   --  Call in the display driver's  'disp_flush' function when the flushing is finished
   procedure Flush_Ready;

   -------------
   -- Imports --
   -------------

   pragma Import (C, Get, "lv_vdb_get");
   pragma Import (C, Flush, "lv_vdb_flush");
   pragma Import (C, Set_Adr, "lv_vdb_set_adr");
   pragma Import (C, Flush_Ready, "lv_flush_ready");

end Lv.Vdb;
