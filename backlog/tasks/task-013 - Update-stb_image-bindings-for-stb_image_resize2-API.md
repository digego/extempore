---
id: task-013
title: Update stb_image bindings for stb_image_resize2 API
status: To Do
assignee: []
created_date: '2025-12-18 02:46'
labels:
  - graphics
  - external-libs
  - api-migration
dependencies: []
priority: low
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
The upstream stb library has replaced `stb_image_resize.h` with `stb_image_resize2.h`, which has a completely different API. The extemporelang/stb fork is pinned to an old version that still has the original resize API.

To update to the latest upstream stb, we need to:

1. Update the C wrapper in the extemporelang/stb fork (`stb_image.c`) to use `stb_image_resize2.h` instead of `stb_image_resize.h`
2. Update the xtlang bindings in `libs/external/stb_image.xtm` to match the new API

Key API changes:
- Functions now return pointers instead of int (e.g. `stbir_resize_uint8` returns `unsigned char*` instead of `int`)
- Different function names and signatures
- The old functions like `stbir_resize_uint8`, `stbir_resize_float`, `stbir_resize_uint8_srgb`, `stbir_resize_uint8_srgb_edgemode` all need updating

Current bindings in stb_image.xtm that need updating:
```
(bind-lib libstb_image stbir_resize_uint8 [i32,i8*,i32,i32,i32,i8*,i32,i32,i32,i32]*)
(bind-lib libstb_image stbir_resize_float [i32,float*,i32,i32,i32,float*,i32,i32,i32,i32]*)
(bind-lib libstb_image stbir_resize_uint8_srgb [i32,i8*,i32,i32,i32,i8*,i32,i32,i32,i32,i32,i32]*)
(bind-lib libstb_image stbir_resize_uint8_srgb_edgemode [i32,i8*,i32,i32,i32,i8*,i32,i32,i32,i32,i32,i32,stbir_edge]*)
```

First step should be to determine if these resize functions are actually used anywhere in the codebase or examples.
<!-- SECTION:DESCRIPTION:END -->
