with System;
with Interfaces.C; use Interfaces.C;
with Interfaces;

package Lv
with Elaborate_Body
is

   subtype Int8_T is Interfaces.Integer_8;
   subtype Uint8_T is Interfaces.Unsigned_8;
   subtype Int16_T is Interfaces.Integer_16;
   subtype Uint16_T is Interfaces.Unsigned_16;
   subtype Int32_T is Interfaces.Integer_32;
   subtype Uint32_T is Interfaces.Unsigned_32;
   subtype Int64_T is Interfaces.Integer_64;
   subtype Uint64_T is Interfaces.Unsigned_64;

   subtype U_Bool is int;

   subtype C_String_Ptr is System.Address;

   type String_Array is array (Natural range <>) of C_String_Ptr
     with Convention => C;

   type String_Array_Ptr is private;

   LV_KEY_UP        : constant := 17;
   LV_KEY_DOWN      : constant := 18;
   LV_KEY_RIGHT     : constant := 19;
   LV_KEY_LEFT      : constant := 20;
   LV_KEY_ESC       : constant := 27;
   LV_KEY_DEL       : constant := 127;
   LV_KEY_BACKSPACE : constant := 8;
   LV_KEY_ENTER     : constant := 10;
   LV_KEY_NEXT      : constant := 9;
   LV_KEY_PREV      : constant := 11;
   LV_KEY_HOME      : constant := 2;
   LV_KEY_END       : constant := 3;

   subtype C is Character;

   SYMBOL_AUDIO       : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#80#);
   SYMBOL_VIDEO       : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#81#);
   SYMBOL_LIST        : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#82#);
   SYMBOL_OK          : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#83#);
   SYMBOL_CLOSE       : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#84#);
   SYMBOL_POWER       : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#85#);
   SYMBOL_SETTINGS    : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#86#);
   SYMBOL_TRASH       : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#87#);
   SYMBOL_HOME        : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#88#);
   SYMBOL_DOWNLOAD    : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#89#);
   SYMBOL_DRIVE       : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#8A#);
   SYMBOL_REFRESH     : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#8B#);
   SYMBOL_MUTE        : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#8C#);
   SYMBOL_VOLUME_MID  : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#8D#);
   SYMBOL_VOLUME_MAX  : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#8E#);
   SYMBOL_IMAGE       : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#8F#);
   SYMBOL_EDIT        : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#90#);
   SYMBOL_PREV        : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#91#);
   SYMBOL_PLAY        : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#92#);
   SYMBOL_PAUSE       : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#93#);
   SYMBOL_STOP        : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#94#);
   SYMBOL_NEXT        : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#95#);
   SYMBOL_EJECT       : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#96#);
   SYMBOL_LEFT        : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#97#);
   SYMBOL_RIGHT       : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#98#);
   SYMBOL_PLUS        : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#99#);
   SYMBOL_MINUS       : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#9A#);
   SYMBOL_WARNING     : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#9B#);
   SYMBOL_SHUFFLE     : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#9C#);
   SYMBOL_UP          : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#9D#);
   SYMBOL_DOWN        : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#9E#);
   SYMBOL_LOOP        : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#9F#);
   SYMBOL_DIRECTORY   : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#A0#);
   SYMBOL_UPLOAD      : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#A1#);
   SYMBOL_CALL        : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#A2#);
   SYMBOL_CUT         : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#A3#);
   SYMBOL_COPY        : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#A4#);
   SYMBOL_SAVE        : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#A5#);
   SYMBOL_CHARGE      : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#A6#);
   SYMBOL_BELL        : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#A7#);
   SYMBOL_KEYBOARD    : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#A8#);
   SYMBOL_GPS         : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#A9#);
   SYMBOL_FILE        : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#AA#);
   SYMBOL_WIFI        : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#AB#);
   SYMBOL_BATTERY_FUL : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#AC#);
   SYMBOL_BATTERY_3   : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#AD#);
   SYMBOL_BATTERY_2   : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#AE#);
   SYMBOL_BATTERY_1   : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#AF#);
   SYMBOL_BATTERY_EMP : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#B0#);
   SYMBOL_BLUETOOTH   : constant String := C'Val (16#EF#) & C'Val (16#A0#) & C'Val (16#B1#);

private

   type String_Array_Ptr is new System.Address;

end Lv;
