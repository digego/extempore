;; Binding functions
(define (setup-bindings)
  (define libglut (sys:open-dylib "/System/Library/Frameworks/GLUT.framework/GLUT"))
  (bind-lib libglut glutInitWindowPosition [void,i32,i32]*)
  (bind-lib libglut glutInitWindowSize [void,i32,i32]*)
  (bind-lib libglut glutInitDisplayMode [void,i32]*)
  (bind-lib libglut glutCreateWindow [i32,i8*]*)
  (bind-lib libglut glutDisplayFunc [void,i8*]*)
  (bind-lib libglut glutMainLoop [void]*)
  (bind-lib libglut glutPostRedisplay [void]*)
  (bind-lib libglut glutIdleFunc [void,i8*]*)
  )

;; Wrappers
(definec open-window
  (lambda (x y w h mode title)
    (glutInitWindowPosition x y)
    (glutInitWindowSize w h)
    (glutInitDisplayMode mode)
    (glutCreateWindow title)))

(definec glut-display-func
  (lambda (f:[void,i8*]*)
    (glutDisplayFunc f)))

(definec glut-idle-func
  (lambda (f:[void,i8*]*)
    (glutIdleFunc f)))

(definec glut-main-loop
  (lambda ()
    (glutMainLoop)))

;; Callbacks
(definec render-callback
  (lambda ()
    (glClear (bitwise-or GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
    (glBegin GL_TRIANGLES)
    (glVertex3f -0.5 -0.5 0.0)
    (glVertex3f 0.5 0.0 0.0)
    (glVertex3f 0.0 0.5 0.0)
    (glEnd)
    (glutSwapBuffers)
    '()))

(definec idle-callback
  (lambda ()
    (glutPostRedisplay)
    '()))

(llvm:get-globalvar "angle")

(setup-bindings)
(open-window 0 0 800 600 18 "foo")
(glut-display-func (llvm:get-native-function "render-callback"))
(glut-idle-func (llvm:get-native-function "idle-callback"))
(glut-main-loop)
