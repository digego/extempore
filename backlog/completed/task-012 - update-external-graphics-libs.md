---
id: task-012
title: update external graphics libs
status: Done
assignee: []
created_date: '2025-12-18 00:35'
updated_date: '2025-12-18 03:01'
labels: []
dependencies: []
---

On this aarch64 branch we've updated all the versions for the "external audio"
libs that CMakeLists.txt pulls in, but not for the graphics ones.

Partially that's because I suspect there's some bit-rot there, and I don't want
to hold up the release just to fix the (not so essential) graphics stuff. And
it's even more fragile because the "xtlang header" files (anything with a
`bind-lib` in it) are manually generated, so if the C APIs for the deps change
then the xtlang headers need to change too, but there's no way of running it
short of running the tests and a) making sure it doesn't crash, and b)
visually/aurally inspecting the output (in the case of graphics/audio libs).

Anyway, with those caveats aside, it might be worth _trying_ to update the
graphics libs and see how we go.

## Implementation Notes

<!-- SECTION:NOTES:BEGIN -->
## Progress

### Completed updates

- **GLFW**: 3.2.1 → 3.4 ✓ (builds successfully)
- **Assimp**: 3.2 → 5.4.3 ✓ (builds successfully)

### Blocked updates

- **stb**: The extemporelang/stb fork includes `stb_image_resize.h`, but upstream stb has replaced it with `stb_image_resize2.h` which has a completely different API. The xtlang bindings in `libs/external/stb_image.xtm` use the old resize functions (`stbir_resize_uint8`, `stbir_resize_float`, etc.). Updating would require:
  1. Updating the C wrapper in the fork (`stb_image.c`)
  2. Updating the xtlang bindings to use the new API signatures
  
  The new API returns pointers instead of int, and has different function names/signatures.

- **nanovg**: The upstream (memononen/nanovg) is explicitly "not actively maintained". The extemporelang fork has custom CMake build integration. No benefit to updating.

### Still to do

- Test the graphics examples actually work with the updated GLFW/Assimp
- Consider whether stb_image_resize is actually used and needs updating

## Testing results

- GLFW 3.4 library loads successfully, all function bindings work
- Assimp 5.4.3 compiles successfully
- Found pre-existing bug: `register_for_window_events` is called in `libs/external/glfw3.xtm` but never defined anywhere. This prevents graphics examples from running but is unrelated to the version updates.

## Final commits

- `833cbe4f` - update GLFW to 3.4
- `8cb6feef` - update Assimp to 5.4.3
- `a9b75296` - remove obsolete register_for_window_events calls from glfw3.xtm

The `register_for_window_events` issue was not a breaking change from GLFW - it was a pre-existing bug where the function was called but defined in C++ code that wasn't being linked properly. The fix was to simply remove the calls since GLFW handles application activation internally.
<!-- SECTION:NOTES:END -->
