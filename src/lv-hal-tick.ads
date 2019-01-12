package Lv.Hal.Tick is

   --  You have to call this function periodically
   --  @param tick_period the call period of this function in milliseconds
   procedure Inc (Tick_Period : Uint32_T);

   --  Get the elapsed milliseconds since start up
   --  @return the elapsed milliseconds
   function Get return Uint32_T;

   --  Get the elapsed milliseconds since a previous time stamp
   --  @param prev_tick a previous time stamp (return value of systick_get() )
   --  @return the elapsed milliseconds since 'prev_tick'
   function Elaps (Prev_Tick : Uint32_T) return Uint32_T;

   -------------
   -- Imports --
   -------------

   pragma Import (C, Inc, "lv_tick_inc");
   pragma Import (C, Get, "lv_tick_get");
   pragma Import (C, Elaps, "lv_tick_elaps");
end Lv.Hal.Tick;
