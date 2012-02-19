;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Simple Library Wrapper for SOIL
;;
;; http://www.lonesock.net/soil.html
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; load soil library
(define soillib (if (string=? "Linux" (sys:platform))
		    (sys:open-dylib "libSOIL.so.1")
		    (sys:open-dylib "soil.dll")))


(bind-lib soillib SOIL_load_OGL_texture [i32,i8*,i32,i32,i32]*)
;; (bind-lib soillib SOIL_load_OGL_cubemap [i32,i8*,i8*,i8*,i8*,i8*,i8*,i32,i32,i32]*)
;; (bind-lib soillib SOIL_load_OGL_single_cubemap [i32,i8*,i32,i32,i32]*)

(bind-lib soillib SOIL_load_image [i8*,i8*,i32*,i32*,i32*,i32]*)
;(bind-lib soillib SOIL_save_image [i32,i8*,i32,i32,i32,i32,i8*]*)
;(bind-lib solilib SOIL_save_screenshot [i32,i8*,i32,i32,i32,i32,i32]*)


(bind-val SOIL_FLAG_POWER_OF_TWO i32 1)
(bind-val SOIL_FLAG_MIPMAPS i32 2)
(bind-val SOIL_FLAG_TEXTURE_REPEATS i32 4)
(bind-val SOIL_FLAG_MULTIPLY_ALPHA i32 8)
(bind-val SOIL_FLAG_INVERT_Y i32 16)
(bind-val SOIL_FLAG_COMPRESS_TO_DXT i32 32)
(bind-val SOIL_FLAG_DDS_LOAD_DIRECT i32 64)
(bind-val SOIL_FLAG_NTSC_SAFE_RGB i32 128)
(bind-val SOIL_FLAG_CoCg_Y i32 256)
(bind-val SOIL_FLAG_TEXTURE_RECTANGLE i32 512)

(bind-val SOIL_LOAD_AUTO i32 0)
(bind-val SOIL_LOAD_L i32 1)
(bind-val SOIL_LOAD_LA i32 2)
(bind-val SOIL_LOAD_RGB i32 3)
(bind-val SOIL_LOAD_RGBA i32 4)

(bind-val SOIL_SAVE_TYPE_TGA i32 0)
(bind-val SOIL_SAVE_TYPE_BMP i32 1)
(bind-val SOIL_SAVE_TYPE_DDS i32 2)