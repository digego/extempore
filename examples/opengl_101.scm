;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Simplest possible OpenGL example
;
; Are you running VirtualBox!! (if yes read install notes)
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
    (glTranslated 0.0 -1.0 0.0)
    (dotimes (i 50)
       (glTranslated (/ (i64tod i) 100.0) 0.0 0.0) 
       (glRotated degree (/ (i64tod i) 50.0) 0.5 0.0)
       (glutWireCube 0.1))
    (glfwSwapBuffers)))

;; standard impromptu callback                                                
(define opengl-test
  (lambda (degree)
    (my-gl-loop degree)
    (callback (+ (now) 500) 'opengl-test (+ degree 1.0))))

;; open window
(open-window 640 480)
;; start animating
(opengl-test 0.0)