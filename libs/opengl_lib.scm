;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Some OPENGL helper stuff
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(if (string=? (sys:platform) "Linux")
    (begin (llvm:compile "declare i32 @glGetUniformBlockIndex(i32, i8*);")
	   (llvm:compile "declare void @glUniformBlockBinding(i32, i32, i32);")
	   (llvm:compile "declare void @glBindBufferRange(i32,i32,i32,i32,i32);")))

(bind-val GL_ONE_MINUS_SRC_ALPHA i32 771)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load SOIL
(load "libs/image_lib.scm")

;; id of 0 means Create new texture
;; id > 0 means load image into existing texture
;; returns texture 
(definec gl-load-tex
  (lambda (path id)
    (let ((texid (SOIL_load_OGL_texture path SOIL_LOAD_AUTO id (+ SOIL_FLAG_MIPMAPS SOIL_FLAG_INVERT_Y)))) ; SOIL_FLAG_MULTIPLY_ALPHA
      (if (= texid 0)
	  (printf "Error loading: %s\n" path))
      texid)))


;; get texture-width
(definec gl-tex-width
  (lambda (tex)
    (glBindTexture GL_TEXTURE_RECTANGLE_ARB tex)
    (let ((v (stack-alloc i32)))
      (glGetTexLevelParameteriv GL_TEXTURE_RECTANGLE_ARB 0 GL_TEXTURE_WIDTH v)
      (pref v 0))))


;; get texture height
(definec gl-tex-height
  (lambda (tex)
    (glBindTexture GL_TEXTURE_RECTANGLE_ARB tex)
    (let ((v (stack-alloc i32)))
      (glGetTexLevelParameteriv GL_TEXTURE_RECTANGLE_ARB 0 GL_TEXTURE_HEIGHT v)
      (pref v 0))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load GLU
(define libglu (if (string=? "Linux" (sys:platform))
		   (sys:open-dylib "/usr/lib/libGLU.so")
		   (if (string=? "Windows" (sys:platform))
		       (sys:open-dylib "Glu32.dll")
		       (sys:open-dylib "/System/Library/Frameworks/OpenGL.framework/OpenGL"))))

(bind-lib libglu gluLookAt [void,double,double,double,double,double,double,double,double,double]*)
(bind-lib libglu gluPerspective [void,double,double,double,double]*)
(bind-lib libglu gluErrorString [i8*,i32]*)
			   


(definec gl-setup
  (lambda ()
    (glEnable GL_LIGHTING)
    (glEnable GL_LIGHT0)
    (let ((diffuse (heap-alloc 4 float))
	  (specular (heap-alloc 4 float))
	  (position (heap-alloc 4 float)))      
      (pset! diffuse 0 1.0)
      (pset! diffuse 1 1.0)
      (pset! diffuse 2 1.0)
      (pset! diffuse 3 1.0)
      (pset! specular 0 1.0)
      (pset! specular 2 1.0)
      (pset! specular 3 1.0)
      (pset! specular 4 1.0)
      (pset! position 0 100.0)
      (pset! position 1 100.0)
      (pset! position 2 100.0)
      (pset! position 3 0.0)

      (glLightfv GL_LIGHT0 GL_DIFFUSE diffuse)
      (glLightfv GL_LIGHT0 GL_SPECULAR specular)
      (glLightfv GL_LIGHT0 GL_POSITION position))
    
    (glEnable GL_DEPTH_TEST)
    (glShadeModel GL_FLAT)
    (glDisable GL_BLEND)
    (glBlendFunc GL_ONE GL_SRC_ALPHA)))


(definec gl-set-view
  (lambda (w:double h:double)
    (glViewport 0 0 (dtoi32 w) (dtoi32 h))
    (glMatrixMode 5889)
    (glLoadIdentity)
    (gluPerspective 27.0 (/ w h) 1.0 1000.0)
    (glMatrixMode 5888)
    (glEnable 2929)
    (gl-setup)
    1))

(definec gl-look-at
  (lambda (eyex eyey eyez centre-x centre-y centre-z up-x up-y up-z)
    (glLoadIdentity)
    (gluLookAt eyex eyey eyez centre-x centre-y centre-z up-x up-y up-z)))


(define gl-load-extensions
  (lambda ()
    (if (string=? (sys:platform) "Windows")
	(begin (println "Loading OpenGL Extensions for Windows!!!!!")
	       (gl:add-extension "glGetShaderiv")
	       (gl:add-extension "glGetShaderInfoLog")
	       (gl:add-extension "glGetProgramiv")
	       (gl:add-extension "glGetProgramInfoLog")
	       (gl:add-extension "glCreateShader")
	       (gl:add-extension "glCreateProgram")
	       (gl:add-extension "glShaderSource")
	       (gl:add-extension "glCompileShader")
	       (gl:add-extension "glAttachShader")
	       (gl:add-extension "glLinkProgram")
	       (gl:add-extension "glUseProgram")
	       (gl:add-extension "glUniform1f")
	       (gl:add-extension "glUniform1i")	   
	       (gl:add-extension "glUniform2f")
	       (gl:add-extension "glUniform1fv")
	       (gl:add-extension "glUniform2fv")	   	   	   
	       (gl:add-extension "glGetUniformLocation")
	       (gl:add-extension "glGetUniformBlockIndex")
	       (gl:add-extension "glUniformBlockBinding")
	       (gl:add-extension "glBindBufferRange")
	       (gl:add-extension "glUniform3f")))))
	   


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  GL DRAWING STUFF
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(definec circle-line
  (lambda (radius:double x:double y:double)
    (let ((k 0.0))
      (glLineWidth 3.0)
      (glBegin GL_LINE_LOOP)
      (dotimes (k 90.0)
	(let ((angle:double (/ (* k 2.0 PI) 90.0)))
	  (glVertex2d (+ x (* (cos angle) radius)) (+ y (* (sin angle) radius)))))
      (glEnd))))


(definec circle-whole
  (lambda (radius:double x:double y:double)
    (let ((k 0.0))
      (glBegin GL_TRIANGLE_FAN)
      (dotimes (k 90.0)
	(let ((angle:double (/ (* k 2.0 PI) 90.0)))
	  (glVertex2d (+ x (* (cos angle) radius)) (+ y (* (sin angle) radius)))))
      (glEnd))))

(definec cube-whole
  (let ((dlist -1))
    (lambda ()
      (if (> dlist -1)
	  (begin (glCallList dlist) 1)
	  (begin (set! dlist (glGenLists 1))
		 (glNewList dlist (+ GL_COMPILE 1))
		 (glBegin GL_QUADS)
		 ;; Front face
		 (glNormal3d 0.0 0.0 1.0)
		 (glVertex3d 0.0 0.0  1.0)
		 (glVertex3d 1.0 0.0  1.0)
		 (glVertex3d 1.0  1.0  1.0)
		 (glVertex3d 0.0  1.0  1.0)
		 ;; Back face
		 (glNormal3d 0.0 0.0 -1.0)
		 (glVertex3d 0.0 0.0 0.0)
		 (glVertex3d 0.0  1.0 0.0)
		 (glVertex3d 1.0  1.0 0.0)
		 (glVertex3d 1.0 0.0 0.0)
		 ;; Top face
		 (glNormal3d 0.0 1.0 0.0)
		 (glVertex3d 0.0  1.0 0.0)
		 (glVertex3d 0.0  1.0  1.0)
		 (glVertex3d 1.0  1.0  1.0)
		 (glVertex3d 1.0  1.0 0.0)
		 ;; Bottom face
		 (glNormal3d 0.0 -1.0 0.0)
		 (glVertex3d 0.0 0.0 0.0)
		 (glVertex3d 1.0 0.0 0.0)
		 (glVertex3d 1.0 0.0  1.0)
		 (glVertex3d 0.0 0.0  1.0)
		 ;; Right face
		 (glNormal3d 1.0 0.0 0.0)
		 (glVertex3d 1.0 0.0 0.0)
		 (glVertex3d 1.0  1.0 0.0)
		 (glVertex3d 1.0  1.0  1.0)
		 (glVertex3d 1.0 0.0  1.0)
		 ;; Left face
		 (glNormal3d -1.0 0.0 0.0)
		 (glVertex3d 0.0 0.0 0.0)
		 (glVertex3d 0.0 0.0  1.0)
		 (glVertex3d 0.0  1.0  1.0)
		 (glVertex3d 0.0  1.0 0.0)
		 (glEnd)
		 (glEndList)
		 1)))))