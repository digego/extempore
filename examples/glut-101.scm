;; Wrappers for C functions we want to call from Scheme
(define-wrapper glut-display-func glutDisplayFunc)
(define-wrapper glut-idle-func glutIdleFunc)
(define-wrapper glut-main-loop glutMainLoop)

(definec open-window
  (lambda (x y w h mode title)
    (glutInitWindowPosition x y)
    (glutInitWindowSize w h)
    (glutInitDisplayMode mode)
    (glutCreateWindow title)))


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

(open-window 0 0 800 600 18 "foo")
(glut-display-func (llvm:get-native-function "render-callback"))
(glut-idle-func (llvm:get-native-function "idle-callback"))
(glut-main-loop)
