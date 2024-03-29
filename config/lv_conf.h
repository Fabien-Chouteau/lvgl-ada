/**
 * @file lv_conf.h
 *
 */

#if 1 /*Set it to "1" to enable the content*/

#ifndef LV_CONF_H
#define LV_CONF_H

#include "lvgl_ada_config.h"

/*===================
   Dynamic memory
 *===================*/

/* Memory size which will be used by the library
 * to store the graphical objects and other data */
#if USE_BUILTIN_ALLOCATOR == USE_BUILTIN_ALLOCATOR_TRUE
#define LV_MEM_CUSTOM                                                          \
  0 /*1: use custom malloc/free, 0: use the built-in                           \
       lv_mem_alloc/lv_mem_free*/
#else
#define LV_MEM_CUSTOM                                                          \
  1 /*1: use custom malloc/free, 0: use the built-in                           \
       lv_mem_alloc/lv_mem_free*/
#endif

#if LV_MEM_CUSTOM == 0
#define LV_MEM_SIZE                                                            \
  (BUILTIN_ALLOCATOR_SIZE)   /*Size memory used by `lv_mem_alloc` in bytes (>= \
                                2kB)*/
#define LV_MEM_ATTR          /*Complier prefix for big array declaration*/
#define LV_MEM_AUTO_DEFRAG 1 /*Automatically defrag on free*/
#else                        /*LV_MEM_CUSTOM*/
#define LV_MEM_CUSTOM_INCLUDE                                                  \
  <stdlib.h>                       /*Header for the dynamic memory function*/
#define LV_MEM_CUSTOM_ALLOC malloc /*Wrapper to malloc*/
#define LV_MEM_CUSTOM_FREE free    /*Wrapper to free*/
#endif                             /*LV_MEM_CUSTOM*/

/*===================
   Graphical settings
 *===================*/

/* Horizontal and vertical resolution of the library.*/
#define LV_HOR_RES (HORIZONTAL_RESOLUTION)
#define LV_VER_RES (VERTICAL_RESOLUTION)
#define LV_DPI (DENSITY_PER_INCH)

/* Size of VDB (Virtual Display Buffer: the internal graphics buffer).
 * Required for buffered drawing, opacity and anti-aliasing
 * VDB makes the double buffering, you don't need to deal with it!
 * Typical size: ~1/10 screen */
#define LV_VDB_SIZE                                                            \
  (VIRTUAL_DISPLAY_BUFFER_SIZE) /*Size of VDB in pixel count (1/10 screen size is good for \
                       first)*/
#define LV_VDB_PX_BPP                                                          \
  LV_COLOR_SIZE /*Bit-per-pixel of VDB. Useful for monochrome or non-standard  \
                   color format displays. (Special formats are handled with    \
                   `disp_drv->vdb_wr`)*/
#define LV_VDB_ADR                                                             \
  0 /*Place VDB to a specific address (e.g. in external RAM) (0: allocate      \
       automatically into RAM; LV_VDB_ADR_INV: to replace it later with        \
       `lv_vdb_set_adr()`)*/

/* Use two Virtual Display buffers (VDB) parallelize rendering and flushing
 * (optional) The flushing should use DMA to write the frame buffer in the
 * background*/

#if DOUBLE_BUFFERING == DOUBLE_BUFFERING_TRUE
#define LV_VDB_DOUBLE 1 /*1: Enable the use of 2 VDBs*/
#else
#define LV_VDB_DOUBLE 0 /*1: Enable the use of 2 VDBs*/
#endif
#define LV_VDB2_ADR         0       /*Place VDB2 to a specific address (e.g. in external RAM) (0: allocate automatically into RAM; LV_VDB_ADR_INV: to replace it later with `lv_vdb_set_adr()`)*/

/* Enable anti-aliasing (lines, and radiuses will be smoothed) */
#define LV_ANTIALIAS        1       /*1: Enable anti-aliasing*/

/*Screen refresh settings*/
#define LV_REFR_PERIOD      30    /*Screen refresh period in milliseconds*/
#define LV_INV_FIFO_SIZE    32    /*The average count of objects on a screen */

/*=================
   Misc. setting
 *=================*/

/*Input device settings*/
#define LV_INDEV_READ_PERIOD            50                     /*Input device read period in milliseconds*/
#define LV_INDEV_POINT_MARKER           0                      /*Mark the pressed points  (required: USE_LV_REAL_DRAW = 1)*/
#define LV_INDEV_DRAG_LIMIT             10                     /*Drag threshold in pixels */
#define LV_INDEV_DRAG_THROW             20                     /*Drag throw slow-down in [%]. Greater value means faster slow-down */
#define LV_INDEV_LONG_PRESS_TIME        400                    /*Long press time in milliseconds*/
#define LV_INDEV_LONG_PRESS_REP_TIME    100                    /*Repeated trigger period in long press [ms] */

/*Color settings*/

/*Color depth: 1/8/16/32*/
#if PIXEL_BIT_DEPTH == PIXEL_BIT_DEPTH_PIX_1BIT
#define LV_COLOR_DEPTH     1
#elif PIXEL_BIT_DEPTH == PIXEL_BIT_DEPTH_PIX_8BIT
#define LV_COLOR_DEPTH     8
#elif PIXEL_BIT_DEPTH == PIXEL_BIT_DEPTH_PIX_16BIT
#define LV_COLOR_DEPTH     16
#elif PIXEL_BIT_DEPTH == PIXEL_BIT_DEPTH_PIX_32BIT
#define LV_COLOR_DEPTH     32
#else
#error "Invalid Pixel_Bit_Depth"
#endif

/* Swap the 2 bytes of RGB565 color. Useful if the display has a 8 bit interface
 * (e.g. SPI)
 */
#if COLOR_16_SWAP == COLOR_16_SWAP_TRUE
#define LV_COLOR_16_SWAP   1
#else
#define LV_COLOR_16_SWAP   0
#endif

#define LV_COLOR_SCREEN_TRANSP        0           /*1: Enable screen transparency. Useful for OSD or other overlapping GUIs. Requires ARGB8888 colors*/
#define LV_COLOR_TRANSP    LV_COLOR_LIME          /*Images pixels with this color will not be drawn (with chroma keying)*/

/*Text settings*/
#define LV_TXT_UTF8             1                /*Enable UTF-8 coded Unicode character usage */
#define LV_TXT_BREAK_CHARS     " ,.;:-_"         /*Can break texts on these chars*/

/*Graphics feature usage*/
#define USE_LV_ANIMATION        1               /*1: Enable all animations*/
#define USE_LV_SHADOW           1               /*1: Enable shadows*/
#define USE_LV_GROUP            1               /*1: Enable object groups (for keyboards)*/
#define USE_LV_GPU              1               /*1: Enable GPU interface*/
#define USE_LV_REAL_DRAW        1               /*1: Enable function which draw directly to the frame buffer instead of VDB (required if LV_VDB_SIZE = 0)*/
#define USE_LV_FILESYSTEM       1               /*1: Enable file system (required by images*/

/*Compiler settings*/
#define LV_ATTRIBUTE_TICK_INC                   /* Define a custom attribute to `lv_tick_inc` function */
#define LV_ATTRIBUTE_TASK_HANDLER               /* Define a custom attribute to `lv_task_handler` function */
#define LV_COMPILER_VLA_SUPPORTED            1  /* 1: Variable length array is supported*/
#define LV_COMPILER_NON_CONST_INIT_SUPPORTED 1  /* 1: Initialization with non constant values are supported */

/*HAL settings*/
#define LV_TICK_CUSTOM     0                        /*1: use a custom tick source (removing the need to manually update the tick with `lv_tick_inc`) */
#if LV_TICK_CUSTOM == 1
#define LV_TICK_CUSTOM_INCLUDE  "Arduino.h"         /*Header for the sys time function*/
#define LV_TICK_CUSTOM_SYS_TIME_EXPR (millis())     /*Expression evaluating to current systime in ms*/
#endif     /*LV_TICK_CUSTOM*/


/*Log settings*/
#define USE_LV_LOG      1   /*Enable/disable the log module*/
#if USE_LV_LOG
/* How important log should be added:
 * LV_LOG_LEVEL_TRACE       A lot of logs to give detailed information
 * LV_LOG_LEVEL_INFO        Log important events
 * LV_LOG_LEVEL_WARN        Log if something unwanted happened but didn't caused problem
 * LV_LOG_LEVEL_ERROR       Only critical issue, when the system may fail
 */
#if LOG_LEVEL == LOG_LEVEL_TRACE
#define LV_LOG_LEVEL    LV_LOG_LEVEL_TRACE
#elif LOG_LEVEL == LOG_LEVEL_INFO
#define LV_LOG_LEVEL    LV_LOG_LEVEL_INFO
#elif LOG_LEVEL == LOG_LEVEL_WARN
#define LV_LOG_LEVEL    LV_LOG_LEVEL_WARN
#elif LOG_LEVEL == LOG_LEVEL_ERROR
#define LV_LOG_LEVEL    LV_LOG_LEVEL_ERROR
#else
#error "Invalid Log_Level"
#endif
/* 1: Print the log with 'printf'; 0: user need to register a callback*/

#if LOG_WITH_PRINTF == LOG_WITH_PRINTF_TRUE
#define LV_LOG_PRINTF   1
#endif
#endif  /*USE_LV_LOG*/

/*================
 *  THEME USAGE
 *================*/
#if THEME_LIVE_UPDATE == THEME_LIVE_UPDATE_TRUE
#define LV_THEME_LIVE_UPDATE    1       /*1: Allow theme switching at run time. Uses 8..10 kB of RAM*/
#endif

#define USE_LV_THEME_TEMPL      0       /*Just for test*/
#define USE_LV_THEME_DEFAULT    1       /*Built mainly from the built-in styles. Consumes very few RAM*/
#define USE_LV_THEME_ALIEN      1       /*Dark futuristic theme*/
#define USE_LV_THEME_NIGHT      1       /*Dark elegant theme*/
#define USE_LV_THEME_MONO       1       /*Mono color theme for monochrome displays*/
#define USE_LV_THEME_MATERIAL   1       /*Flat theme with bold colors and light shadows*/
#define USE_LV_THEME_ZEN        1       /*Peaceful, mainly light theme */
#define USE_LV_THEME_NEMO       1       /*Water-like theme based on the movie "Finding Nemo"*/

/*==================
 *    FONT USAGE
 *===================*/

/* More info about fonts: https://littlevgl.com/basics#fonts
 * To enable a built-in font use 1,2,4 or 8 values
 * which will determine the bit-per-pixel */
#define USE_LV_FONT_DEJAVU_10              4
#define USE_LV_FONT_DEJAVU_10_LATIN_SUP    4
#define USE_LV_FONT_DEJAVU_10_CYRILLIC     4
#define USE_LV_FONT_SYMBOL_10              4

#define USE_LV_FONT_DEJAVU_20              4
#define USE_LV_FONT_DEJAVU_20_LATIN_SUP    4
#define USE_LV_FONT_DEJAVU_20_CYRILLIC     4
#define USE_LV_FONT_SYMBOL_20              4

#define USE_LV_FONT_DEJAVU_30              4
#define USE_LV_FONT_DEJAVU_30_LATIN_SUP    4
#define USE_LV_FONT_DEJAVU_30_CYRILLIC     4
#define USE_LV_FONT_SYMBOL_30              4

#define USE_LV_FONT_DEJAVU_40              4
#define USE_LV_FONT_DEJAVU_40_LATIN_SUP    4
#define USE_LV_FONT_DEJAVU_40_CYRILLIC     4
#define USE_LV_FONT_SYMBOL_40              4

#define USE_LV_FONT_MONOSPACE_8            1

/* Optionally declare your custom fonts here.
 * You can use these fonts as default font too
 * and they will be available globally. E.g.
 * #define LV_FONT_CUSTOM_DECLARE LV_FONT_DECLARE(my_font_1) \
 *                                LV_FONT_DECLARE(my_font_2) \
 */
#define LV_FONT_CUSTOM_DECLARE

/*Always set a default font from the built-in fonts*/

#if DEFAULT_FONT == DEFAULT_FONT_DEJAVU_10
# define LV_FONT_DEFAULT        &lv_font_dejavu_10
#elif DEFAULT_FONT == DEFAULT_FONT_DEJAVU_20
# define LV_FONT_DEFAULT        &lv_font_dejavu_20
#elif DEFAULT_FONT == DEFAULT_FONT_DEJAVU_30
# define LV_FONT_DEFAULT        &lv_font_dejavu_30
#elif DEFAULT_FONT == DEFAULT_FONT_DEJAVU_40
# define LV_FONT_DEFAULT        &lv_font_dejavu_40
#elif DEFAULT_FONT == DEFAULT_FONT_MONOSPACE_8
# define LV_FONT_DEFAULT        &lv_font_monospace_8
#else
#error "Invalid Default font"
#endif

/*===================
 *  LV_OBJ SETTINGS
 *==================*/
#define LV_OBJ_FREE_NUM_TYPE    uint32_t    /*Type of free number attribute (comment out disable free number)*/
#define LV_OBJ_FREE_PTR         1           /*Enable the free pointer attribute*/

/*==================
 *  LV OBJ X USAGE
 *================*/
/*
 * Documentation of the object types: https://littlevgl.com/object-types
 */

/*****************
 * Simple object
 *****************/

/*Label (dependencies: -*/
#define USE_LV_LABEL    1
#if USE_LV_LABEL != 0
#define LV_LABEL_SCROLL_SPEED       25     /*Hor, or ver. scroll speed [px/sec] in 'LV_LABEL_LONG_SCROLL/ROLL' mode*/
#endif

/*Image (dependencies: lv_label*/
#define USE_LV_IMG      1
#if USE_LV_IMG != 0
#define LV_IMG_CF_INDEXED   1       /*Enable indexed (palette) images*/
#define LV_IMG_CF_ALPHA     1       /*Enable alpha indexed images*/
#endif

/*Line (dependencies: -*/
#define USE_LV_LINE     1

/*Arc (dependencies: -)*/
#define USE_LV_ARC      1

/*******************
 * Container objects
 *******************/

/*Container (dependencies: -*/
#define USE_LV_CONT     1

/*Page (dependencies: lv_cont)*/
#define USE_LV_PAGE     1

/*Window (dependencies: lv_cont, lv_btn, lv_label, lv_img, lv_page)*/
#define USE_LV_WIN      1

/*Tab (dependencies: lv_page, lv_btnm)*/
#define USE_LV_TABVIEW      1
#if USE_LV_TABVIEW != 0
#define LV_TABVIEW_ANIM_TIME    300     /*Time of slide animation [ms] (0: no animation)*/
#endif

/*************************
 * Data visualizer objects
 *************************/

/*Bar (dependencies: -)*/
#define USE_LV_BAR      1

/*Line meter (dependencies: *;)*/
#define USE_LV_LMETER   1

/*Gauge (dependencies:bar, lmeter)*/
#define USE_LV_GAUGE    1

/*Chart (dependencies: -)*/
#define USE_LV_CHART    1

/*LED (dependencies: -)*/
#define USE_LV_LED      1

/*Message box (dependencies: lv_rect, lv_btnm, lv_label)*/
#define USE_LV_MBOX     1

/*Text area (dependencies: lv_label, lv_page)*/
#define USE_LV_TA       1
#if USE_LV_TA != 0
#define LV_TA_CURSOR_BLINK_TIME 400     /*ms*/
#define LV_TA_PWD_SHOW_TIME     1500    /*ms*/
#endif

/*Calendar (dependencies: -)*/
#define USE_LV_CALENDAR 1

/*Preload (dependencies: arc)*/
#define USE_LV_PRELOAD      1
#if USE_LV_PRELOAD != 0
#define LV_PRELOAD_DEF_ARC_LENGTH   60      /*[deg]*/
#define LV_PRELOAD_DEF_SPIN_TIME    1000    /*[ms]*/
#endif

/*Canvas (dependencies: -)*/
#define USE_LV_CANVAS      1


/*************************
 * User input objects
 *************************/

/*Button (dependencies: lv_cont*/
#define USE_LV_BTN      1
#if USE_LV_BTN != 0
#define LV_BTN_INK_EFFECT   1       /*Enable button-state animations - draw a circle on click (dependencies: USE_LV_ANIMATION)*/
#endif

/*Image Button (dependencies: lv_btn*/
#define USE_LV_IMGBTN   1

/*Button matrix (dependencies: -)*/
#define USE_LV_BTNM     1

/*Keyboard (dependencies: lv_btnm)*/
#define USE_LV_KB       1

/*Check box (dependencies: lv_btn, lv_label)*/
#define USE_LV_CB       1

/*List (dependencies: lv_page, lv_btn, lv_label, (lv_img optionally for icons ))*/
#define USE_LV_LIST     1
#if USE_LV_LIST != 0
#define LV_LIST_FOCUS_TIME  100 /*Default animation time of focusing to a list element [ms] (0: no animation)  */
#endif

/*Drop down list (dependencies: lv_page, lv_label)*/
#define USE_LV_DDLIST    1
#if USE_LV_DDLIST != 0
#define LV_DDLIST_ANIM_TIME     200     /*Open and close default animation time [ms] (0: no animation)*/
#endif

/*Roller (dependencies: lv_ddlist)*/
#define USE_LV_ROLLER    1
#if USE_LV_ROLLER != 0
#define LV_ROLLER_ANIM_TIME     200     /*Focus animation time [ms] (0: no animation)*/
#endif

/*Slider (dependencies: lv_bar)*/
#define USE_LV_SLIDER    1

/*Switch (dependencies: lv_slider)*/
#define USE_LV_SW       1

/*************************
 * Non-user section
 *************************/
#ifdef _MSC_VER                               /* Disable warnings for Visual Studio*/
# define _CRT_SECURE_NO_WARNINGS
#endif

#endif /*LV_CONF_H*/

#endif /*End of "Content enable"*/
