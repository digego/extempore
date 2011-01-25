;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Simplest possible OpenGL example
;

;; code to open a window                                                        
(definec open-window
  (lambda (width height)
    (glfwInit)
    (glfwOpenWindow width height 0 0 0 0 0 0 65537)
    (glfwSwapInterval 0)))

;; a trivial opengl draw loop
;; need to call glfwSwapBuffers to flush                                        
(definec my-gl-loop
  (lambda (degree)
    (glClear (+ GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
    (glLoadIdentity)
    (glRotated degree 1.0 1.0 0.0)
    (glutWireCube 0.25)
    (glfwSwapBuffers)))

;; standard impromptu callback                                                
(define opengl-test
  (lambda (degree)
    (my-gl-loop degree)
    (callback (+ (now) 1000) 'opengl-test (+ degree 1.0))))

;; open window
(open-window 640 480)
;; start animating
(opengl-test 0)