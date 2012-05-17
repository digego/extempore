;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; A few small Cairo examples
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "libs/cairo_lib.scm")
(load "libs/opengl_lib.scm")

(define ctx (gl:make-ctx ":0.0" #f 0.0 0.0 900.0 600.0))

;; write hello world to a png file
(definec test1
  (lambda ()
    (let ((surface (cairo_image_surface_create CAIRO_FORMAT_ARGB32 300 200))
	  (cr (cairo_create surface)))
      (cairo_select_font_face cr "serif" CAIRO_FONT_SLANT_NORMAL CAIRO_FONT_WEIGHT_BOLD)
      (cairo_set_font_size cr 32.0)
      (cairo_set_source_rgb cr 0.0 0.0 1.0)
      (cairo_move_to cr 10.0 50.0)
      (cairo_show_text cr "Hello, world")
      (cairo_destroy cr)
      (cairo_surface_write_to_png surface "/tmp/hello.png")
      (cairo_surface_destroy surface)
      void)))

(test1)

;; draw animated circles to an opengl context
(definec test2
  (let ((surface (cairo_image_surface_create CAIRO_FORMAT_ARGB32 900 600))
	(cr (cairo_create surface))
	(i 0.0))	    
    (lambda (t:double)
      (cairo_set_source_rgba cr 0.0 0.0 0.0 1.0)
      (cairo_paint cr)
      (cairo_set_line_width cr 2.0)      
      (dotimes (i 200.0)
	(cairo_set_source_rgba cr (* .005 i) (* 0.005 i) 0.7 0.7)
	(cairo_arc cr
		   (+ 450.0 (* 350.0 (cos (* i t .00000003))))
		   (+ 300.0 (* 200.0 (sin (* i t .0000002))))
		   10.0 0.0 TWOPI)
	(cairo_stroke cr))
      (cairo_surface_flush surface)
      (cairo_image_surface_get_data surface))))

;; draw surface data returned from test2
(definec gl-draw
  (lambda ()
    (glLoadIdentity)
    (glClear (+ GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))    
    ;; use tex num 5 repeatedly
    (gl-load-tex 900 600 4 GL_UNSIGNED_BYTE (test2 (i64tod (now))) 5)
    (gl-draw-img -1.0 -1.0 2.0 2.0 0.0 5)
    void))

;; standard callback
(define cairo-animation
  (lambda (beat dur)
    (gl-draw)
    (gl:swap-buffers ctx)    
    (callback (*metro* (+ beat (* .5 dur))) 'cairo-animation (+ beat dur) dur)))

(cairo-animation (*metro* 'get-beat 4) 1/12)
