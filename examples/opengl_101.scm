;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Simplest possible OpenGL example
;
; Are you running VirtualBox!! (if yes read install notes)
; 

;; code to open a window                                                        
(definec open-window
  (lambda (width height)
    (if (> (glfwInit) 0)
	(if (> (glfwOpenWindow width height 0 0 0 0 0 0 65537) 0)
	    (begin (glfwSwapInterval 0) 1)
	    0)
	0)))

;; a trivial opengl draw loop
;; need to call glfwSwapBuffers to flush                                        
(definec my-gl-loop
  (lambda (degree)
    (glClear (+ GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
    (glLoadIdentity)
    (glTranslated 0.0 -1.0 0.0)
    (dotimes (i:double 1000.0)
       (glTranslated (/ i 2000.0) 0.0 0.0)
       (glColor3d (/ i 1500.0) 0.0 1.0)
       (glRotated degree (/ i 200000.0) 0.5 0.0)
       (glutWireCube 0.02))
    (glfwSwapBuffers)))

;; standard impromptu callback                                                
(define opengl-test
  (lambda (degree)
    (my-gl-loop degree)
    (callback (+ (now) 500) 'opengl-test (+ degree .01))))

;; open window
(if (> (open-window 640 480) 0)
    ;; start animating
    (opengl-test 110.0)
    (print-notification "Error opening window!"))
