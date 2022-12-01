with Lv.Style;
with Lv.Objx.Page;

package Lv.Objx.Canvas is

   subtype Instance is Obj_T;

   --  Create a canvas object
   --  @param par pointer to an object, it will be the parent of the new canvas
   --  @param copy pointer to a canvas object, if not NULL then the new object will be copied from it
   --  @return pointer to the created canvas
   function Create (Par : Obj_T; Copy : Instance) return Instance;

   ----------------------
   -- Setter functions --
   ----------------------

   type Img_CF_T is
     (LV_IMG_CF_UNKOWN,

      LV_IMG_CF_RAW,                  -- Contains the file as it is. Needs custom decoder function*/
      LV_IMG_CF_RAW_ALPHA,            -- Contains the file as it is. The image has alpha. Needs custom decoder function*/
      LV_IMG_CF_RAW_CHROMA_KEYED,     -- Contains the file as it is. The image is chroma keyed. Needs custom decoder function*/

      LV_IMG_CF_TRUE_COLOR,           -- Color format and depth should match with LV_COLOR settings*/
      LV_IMG_CF_TRUE_COLOR_ALPHA,     -- Same as `LV_IMG_CF_TRUE_COLOR` but every pixel has an alpha byte*/
      LV_IMG_CF_TRUE_COLOR_CHROMA_KEYED,  -- Same as `LV_IMG_CF_TRUE_COLOR` but LV_COLOR_TRANSP pixels will be transparent*/

      LV_IMG_CF_INDEXED_1BIT,         -- Can have 2 different colors in a palette (always chroma keyed)*/
      LV_IMG_CF_INDEXED_2BIT,         -- Can have 4 different colors in a palette (always chroma keyed)*/
      LV_IMG_CF_INDEXED_4BIT,         -- Can have 16 different colors in a palette (always chroma keyed)*/
      LV_IMG_CF_INDEXED_8BIT,         -- Can have 256 different colors in a palette (always chroma keyed)*/

      LV_IMG_CF_ALPHA_1BIT,           -- Can have one color and it can be drawn or not*/
      LV_IMG_CF_ALPHA_2BIT,           -- Can have one color but 4 different alpha value*/
      LV_IMG_CF_ALPHA_4BIT,           -- Can have one color but 16 different alpha value*/
      LV_IMG_CF_ALPHA_8BIT);          -- Can have one color but 256 different alpha value*/
   pragma Convention (C, Img_CF_T);


   --  Set a buffer for the canvas.
   --  @param buf a buffer where the content of the canvas will be.
   --  The required size is (lv_img_color_format_get_px_size(cf) * w * h) / 8)
   --  It can be allocated with `lv_mem_alloc()` or
   --  it can be statically allocated array (e.g. static lv_color_t buf[100*50]) or
   --  it can be an address in RAM or external SRAM
   --  @param canvas pointer to a canvas object
   --  @param w width of the canvas
   --  @param h height of the canvas
   --  @param cf color format. The following formats are supported:
   --       LV_IMG_CF_TRUE_COLOR, LV_IMG_CF_TRUE_COLOR_CHROMA_KEYED, LV_IMG_CF_INDEXES_1/2/4/8BIT
   procedure Set_Buffer (Self : Instance;
                         Buf  : System.Address;
                         W, H : Lv.Area.Coord_T;
                         CF   : Img_CF_T);

   --  Set the color of a pixel on the canvas
   --  @param canvas
   --  @param x x coordinate of the point to set
   --  @param y x coordinate of the point to set
   --  @param c color of the point
   procedure Set_Px(Self : Instance;
                    X, Y : Lv.Area.Coord_T;
                    C    : Lv.Color.Color_T);

   --  /**
   --   * Set a style of a canvas.
   --   * @param canvas pointer to canvas object
   --   * @param type which style should be set
   --   * @param style pointer to a style
   --   */
   --  void lv_canvas_set_style(lv_obj_t * canvas, lv_canvas_style_t type, lv_style_t * style);
   --

   ----------------------
   -- Getter functions --
   ----------------------

   --  Get the color of a pixel on the canvas
   --  @param canvas
   --  @param x x coordinate of the point to set
   --  @param y x coordinate of the point to set
   --  @return color of the point
   function Get_Px(Self : Instance;
                   X, Y : Lv.Area.Coord_T)
                   return Lv.Color.Color_T;

   --  /**
   --   * Get style of a canvas.
   --   * @param canvas pointer to canvas object
   --   * @param type which style should be get
   --   * @return style pointer to the style
   --   */
   --  lv_style_t * lv_canvas_get_style(const lv_obj_t * canvas, lv_canvas_style_t type);

   ---------------------
   -- Other functions --
   ---------------------

   --  Copy a buffer to the canvas
   --  @param canvas pointer to a canvas object
   --  @param to_copy buffer to copy. The color format has to match with the canvas's buffer color format
   --  @param w width of the buffer to copy
   --  @param h height of the buffer to copy
   --  @param x left side of the destination position
   --  @param y top side of the destination position
   procedure Copy_Buf(Self : Instance;
                      To_Copy : System.Address;
                      W, H, X, Y : Lv.Area.Coord_T);

   --  Multiply a buffer with the canvas
   --  @param canvas pointer to a canvas object
   --  @param to_copy buffer to copy (multiply). LV_IMG_CF_TRUE_COLOR_ALPHA is not supported
   --  @param w width of the buffer to copy
   --  @param h height of the buffer to copy
   --  @param x left side of the destination position
   --  @param y top side of the destination position
   procedure Mult_Buf(Self : Instance;
                      To_Copy : System.Address;
                      W, H, X, Y : Lv.Area.Coord_T);

   --  Draw circle function of the canvas
   --  @param canvas pointer to a canvas object
   --  @param x0 x coordinate of the circle
   --  @param y0 y coordinate of the circle
   --  @param radius radius of the circle
   --  @param color border color of the circle
   procedure Draw_Circle(Self           : Instance;
                         X0, Y0, Radius : Lv.Area.Coord_T;
                         Color          : Lv.Color.Color_T);

   --  Draw line function of the canvas
   --  @param canvas pointer to a canvas object
   --  @param point1 start point of the line
   --  @param point2 end point of the line
   --  @param color color of the line
   --
   --  NOTE: The lv_canvas_draw_line function originates from https://github.com/jb55/bresenham-line.c.
   procedure Draw_Line(Self           : Instance;
                       Point1, Point2 : Lv.Area.Point_T;
                       Color          : Lv.Color.Color_T);

   --  Draw triangle function of the canvas
   --  @param canvas pointer to a canvas object
   --  @param points edge points of the triangle
   --  @param color line color of the triangle
   type Triangle_Points is array (1 .. 3) of Lv.Area.Point_T
     with Convention => C;
   procedure Draw_Triangle(Self   : Instance;
                           Points : not null access constant Triangle_Points;
                           Color  : Lv.Color.Color_T);

   --  Draw rectangle function of the canvas
   --  @param canvas pointer to a canvas object
   --  @param points edge points of the rectangle
   --  @param color line color of the rectangle
   type Rectangle_Points is array (1 .. 4) of Lv.Area.Point_T
     with Convention => C;
   procedure Draw_Rect (Self   : Instance;
                        Points : not null access constant Rectangle_Points;
                        Color  : Lv.Color.Color_T);

   --  Draw polygon function of the canvas
   --  @param canvas pointer to a canvas object
   --  @param points edge points of the polygon
   --  @param size edge count of the polygon
   --  @param color line color of the polygon
   type Poly_Points is array (Natural range <>) of Lv.Area.Point_T
     with Convention => C;
   procedure Draw_Polygon(Self : Instance;
                          Points : not null access constant Poly_Points;
                          Size : Natural;
                          Color : Lv.Color.Color_T);

   --  Fill polygon function of the canvas
   --  @param canvas pointer to a canvas object
   --  @param points edge points of the polygon
   --  @param size edge count of the polygon
   --  @param boundary_color line color of the polygon
   --  @param fill_color fill color of the polygon
   procedure Fill_Polygon(Self : Instance;
                          Points : not null access constant Poly_Points;
                          Size : Natural;
                          Boundary_Color, Fill_Color : Lv.Color.Color_T);

   --  Boundary fill function of the canvas
   --  @param canvas pointer to a canvas object
   --  @param x x coordinate of the start position (seed)
   --  @param y y coordinate of the start position (seed)
   --  @param boundary_color edge/boundary color of the area
   --  @param fill_color fill color of the area
   procedure Boundary_Fill4(Self : Instance;
                            X, Y : Lv.Area.Coord_T;
                            Boundary_Color, Fill_Color : Lv.Color.Color_T);

   --  Flood fill function of the canvas
   --  @param canvas pointer to a canvas object
   --  @param x x coordinate of the start position (seed)
   --  @param y y coordinate of the start position (seed)
   --  @param fill_color fill color of the area
   --  @param bg_color background color of the area
   procedure Flood_Fill(Self : Instance;
                        X, Y : Lv.Area.Coord_T;
                        Fill_Color, BG_Color : Lv.Color.Color_T);

   -------------
   -- Imports --
   -------------

   pragma Import (C, Create, "lv_canvas_create");
   pragma Import (C, Set_Buffer, "lv_canvas_set_buffer");
   pragma Import (C, Set_Px, "lv_canvas_set_px");
   --  pragma Import (C, Set_Style, "lv_canvas_set_style");
   pragma Import (C, Get_Px, "lv_canvas_get_px");
   --  pragma Import (C, Get_Style, "lv_canvas_get_style");
   pragma Import (C, Copy_Buf, "lv_canvas_copy_buf");
   pragma Import (C, Mult_Buf, "lv_canvas_mult_buf");
   pragma Import (C, Draw_Circle, "lv_canvas_draw_circle");
   pragma Import (C, Draw_Line, "lv_canvas_draw_line");
   pragma Import (C, Draw_Triangle, "lv_canvas_draw_triangle");
   pragma Import (C, Draw_Rect, "lv_canvas_draw_rect");
   pragma Import (C, Draw_Polygon, "lv_canvas_draw_polygon");
   pragma Import (C, Fill_Polygon, "lv_canvas_fill_polygon");
   pragma Import (C, Boundary_Fill4, "lv_canvas_boundary_fill4");
   pragma Import (C, Flood_Fill, "lv_canvas_flood_fill");

end Lv.Objx.Canvas;
