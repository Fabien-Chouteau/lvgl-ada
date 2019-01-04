with Interfaces.C; use Interfaces.C;
with Lv.Area;
with Lv.Color;
with System;

package Lv.Vdb is

   Lv_Vdb_Adr_Inv : constant := 8;
   --  unsupported macro: LV_VDB_SIZE_IN_BYTES ((LV_VDB_SIZE * LV_VDB_PX_BPP) >> 3) + (((LV_VDB_SIZE * LV_VDB_PX_BPP) & 0x7) ? 1 : 0)

   type Vdb_T is record
      Area : aliased Lv.Area.Area_T;
      Buf  : access Lv.Color.Color_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Vdb_T);

   function Get return access Vdb_T;
   pragma Import (C, Get, "lv_vdb_get");

   procedure Flush;
   pragma Import (C, Flush, "lv_vdb_flush");

   procedure Set_Adr (Arg1 : System.Address; Arg2 : System.Address);
   pragma Import (C, Set_Adr, "lv_vdb_set_adr");

   procedure Flush_Ready;
   pragma Import (C, Flush_Ready, "lv_flush_ready");

end Lv.Vdb;
