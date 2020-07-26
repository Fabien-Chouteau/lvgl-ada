[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/lvgl_ada.json)](https://alire.ada.dev/crates/lvgl_ada.html)

# lvgl-ada

An Ada binding for the lvgl embedded GUI library version 5.3:
https://github.com/littlevgl/lvgl

# Usage

`lvgl-ada` is available in Alire (the Ada package manager). You can install it
following the instructions here:
https://github.com/alire-project/alire#installation

and then add `lvgl_ada` in the dependencies of your project:
`alr with lvgl_ada`

To use the `lvgl-ada` binding you have to extend one of the
`lvgl_ada_*bit_color.gpr` project file:

For instance:
```
project My_Lvlg extends "lvgl_ada_16bit_color.gpr" is
[...]
end My_Lvlg;
```

In this project you have to provide a `lv_conf.h` base on the
`lv_conf_template.h` provided with `lvgl` (see `lvlg` documentation for more
details).

# API

`lvgl-ada` is a thin binding, which means that the API is very similar to
original C API. The main difference is the package hierarchy.

```c
    lv_obj_t * label = lv_label_create(h, NULL);
    lv_label_set_text(label, "Primary");
```
Becomes:
```ada
   Lab : Lv.Label.Instance;
begin
   Lab := Lv.Label.Create (H, No_Obj);
   Lv.Label.Set_Text (Lab, New_String ("Primary"));
```

# Examples

You can find examples [here](https://github.com/Fabien-Chouteau/lvgl-ada-examples),
or `alr get lvgl_ada_examples`.
