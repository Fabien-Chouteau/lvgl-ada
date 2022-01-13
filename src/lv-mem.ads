with System;

package Lv.Mem is

   --  Allocate a memory dynamically
   --  @param size size of the memory to allocate in bytes
   --  @return pointer to the allocated memory
   function Alloc (Size : Uint32_T) return System.Address;

   --  Free an allocated data
   --  @param data pointer to an allocated memory
   procedure Free (Addr : System.Address);

   --  Reallocate a memory with a new size. The old content will be kept.
   --  @param data pointer to an allocated memory.
   --  Its content will be copied to the new memory block and freed
   --  @param new_size the desired new size in byte
   --  @return pointer to the new memory
   function Realloc (Addr : System.Address; New_Size : Uint32_T)
                     return System.Address;

   --  Join the adjacent free memory blocks
   procedure Defrag;

   --  Give the size of an allocated memory
   --  @param data pointer to an allocated memory
   --  @return the size of data memory in bytes
   function Get_Size (Addr : System.Address) return Uint32_T;

   type Monitor_T is record
      Total_Size        : Uint32_T;
      Free_Cnt          : Uint32_T;
      Free_Size         : Uint32_T;
      Free_Biggest_Size : Uint32_T;
      Used_Pct          : Uint8_T;
      Frag_Pct          : Uint8_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Monitor_T);

   --  Give information about the work memory of dynamic allocation
   --  @param mon_p pointer to a dm_mon_p variable,
   --               the result of the analysis will be stored here
   procedure Monitor (Mon : not null access Monitor_T);

   -------------
   -- Imports --
   -------------

   pragma Import (C, Alloc, "lv_mem_alloc");
   pragma Import (C, Free, "lv_mem_free");
   pragma Import (C, Realloc, "lv_mem_realloc");
   pragma Import (C, Defrag, "lv_mem_defrag");
   pragma Import (C, Get_Size, "lv_mem_get_size");
   pragma Import (C, Monitor, "lv_mem_monitor");

end Lv.Mem;
