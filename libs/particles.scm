;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; An Incredibly Simple Particle System
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; number of particles
;; particle state
;; x positions
;; y positions
;; xv -> xvelocity
;; yv -> yvelocity
;; size -> particle size
;; r -> red
;; g -> green
;; b -> blue

(bind-type psystem <i64,i64*,float*,float*,float*,float*,float*,float*,float*,float*,float*,i32>)

;; psystem type helpers
(definec psystem_size (lambda (psys:psystem*) (tref psys 0)))
(definec psystem_states (lambda (psys:psystem*) (tref psys 1)))
(definec psystem_xs (lambda (psys:psystem*) (tref psys 2)))
(definec psystem_ys (lambda (psys:psystem*) (tref psys 3)))
(definec psystem_xvs (lambda (psys:psystem*) (tref psys 4)))
(definec psystem_yvs (lambda (psys:psystem*) (tref psys 5)))
(definec psystem_sizes (lambda (psys:psystem*) (tref psys 6)))
(definec psystem_reds (lambda (psys:psystem*) (tref psys 7)))
(definec psystem_greens (lambda (psys:psystem*) (tref psys 8)))
(definec psystem_blues (lambda (psys:psystem*) (tref psys 9)))
(definec psystem_alphas (lambda (psys:psystem*) (tref psys 10)))
(definec psystem_texid (lambda (psys:psystem*) (tref psys 11)))

;; create system
(definec psystem_create
  (lambda (number texture_id)
    (let ((psys (halloc psystem))
	  (states (halloc number i64))
	  (xs (halloc number float))
	  (ys (halloc number float))
	  (xvs (halloc number float))
	  (yvs (halloc number float))
	  (sizes (halloc number float))
	  (reds (halloc number float))
	  (greens (halloc number float))
	  (blues (halloc number float))
	  (alphas (halloc number float)))
      ;(printf "%p,%p,%p,%p,%p,%p,%p,%p,%p,%x\n" states xs ys xvs yvs sizes reds greens blues alphas)
      (tfill! psys number states xs ys xvs yvs sizes reds greens blues alphas texture_id)
      psys)))

;; destroy system
(definec psystem_destroy
  (lambda (psys:psystem*)
    (free (tref psys 1))
    (free (tref psys 2))
    (free (tref psys 3))
    (free (tref psys 4))
    (free (tref psys 5))
    (free (tref psys 6))
    (free (tref psys 7))
    (free (tref psys 8))
    (free (tref psys 9))
    (free (tref psys 10))
    (free psys)
    void))


;; draw system
(definec psystem_draw 250000000
  (let ((vertex_dat (zalloc 8000000 float))
	(texcoord_dat (zalloc 8000000 float))
	(color_dat (zalloc 16000000 float)))
    (lambda (psys:psystem*)
      (let ((i 0)
	    (size (psystem_size psys))
	    (texid (psystem_texid psys))
	    (states (psystem_states psys))
	    (state 0)
	    (xs (psystem_xs psys))
	    (ys (psystem_ys psys))
	    (xvs (psystem_xvs psys))
	    (yvs (psystem_yvs psys))
	    (sizes (psystem_sizes psys))
	    (reds (psystem_reds psys))
	    (greens (psystem_greens psys))
	    (blues (psystem_blues psys))
	    (alphas(psystem_alphas psys))
	    (cnt 0))
	
	(glEnable GL_BLEND)
	(glDisable GL_DEPTH_TEST)
	(glDepthMask GL_FALSE)
	(glBlendFunc GL_SRC_ALPHA  GL_ONE)
	
	(dotimes (i size)
	  (set! state (pref states i))
	  (if (> state 0)
	      (let ((cdat (pref-ptr color_dat (* cnt 16)))
		    (vdat (pref-ptr vertex_dat (* cnt 8)))
		    (tdat (pref-ptr texcoord_dat (* cnt 8)))
		    (s (pref sizes i))
		    (x (- (pref xs i) (* 0.5 s)))
		    (y (- (pref ys i) (* 0.5 s))))
		(pset! vdat 0 x) (pset! vdat 1 y)                    ;; v1
		(pset! vdat 2 (+ x s)) (pset! vdat 3 y)              ;; v2
		(pset! vdat 4 (+ x s)) (pset! vdat 5 (+ y s))        ;; v3
		(pset! vdat 6 x) (pset! vdat 7 (+ y s))              ;; v4
		(pset! tdat 0 0.0) (pset! tdat 1 0.0)                ;; t1
		(pset! tdat 2 1.0) (pset! tdat 3 0.0)                ;; t2
		(pset! tdat 4 1.0) (pset! tdat 5 1.0)                ;; t3
		(pset! tdat 6 0.0) (pset! tdat 7 1.0)                ;; t4
		(pset! cdat 0 (pref reds i))                         ;; red
		(pset! cdat 1 (pref greens i))                       ;; green
		(pset! cdat 2 (pref blues i))                        ;; blue
		(pset! cdat 3 (pref alphas i))
		(pset! cdat 4 (pref reds i))                         ;; red
		(pset! cdat 5 (pref greens i))                       ;; green
		(pset! cdat 6 (pref blues i))                        ;; blue
		(pset! cdat 7 (pref alphas i))
		(pset! cdat 8 (pref reds i))                         ;; red
		(pset! cdat 9 (pref greens i))                       ;; green
		(pset! cdat 10 (pref blues i))                        ;; blue
		(pset! cdat 11 (pref alphas i))
		(pset! cdat 12 (pref reds i))                         ;; red
		(pset! cdat 13 (pref greens i))                       ;; green
		(pset! cdat 14 (pref blues i))                        ;; blue
		(pset! cdat 15 (pref alphas i))
		(set! cnt (+ cnt 1)))))
	(glEnable GL_COLOR_MATERIAL)
	(if (> texid 0)
	    (begin (glEnable GL_TEXTURE_2D)
		   (glBindTexture GL_TEXTURE_2D texid)
		   (glTexEnvi GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_COMBINE)
		   1))

	(glEnableClientState GL_VERTEX_ARRAY)
	(glEnableClientState GL_COLOR_ARRAY)
	(glEnableClientState GL_TEXTURE_COORD_ARRAY)	
	
	(glVertexPointer 2 GL_FLOAT 0 (bitcast vertex_dat i8*))
	(glTexCoordPointer 2 GL_FLOAT 0 (bitcast texcoord_dat i8*))	
	(glColorPointer 4 GL_FLOAT 0 (bitcast color_dat i8*))	
	(glDrawArrays GL_QUADS 0 (* (i64toi32 cnt) 4))

	(glDisableClientState GL_VERTEX_ARRAY)
	(glDisableClientState GL_COLOR_ARRAY)
	(glDisableClientState GL_TEXTURE_COORD_ARRAY)

	(if (> texid 0)
	    (glDisable GL_TEXTURE_2D)
	    (glDisable GL_COLOR_MATERIAL))
	
	(glDepthMask GL_TRUE)
	(glEnable GL_DEPTH_TEST)
  	(glDisable GL_COLOR_MATERIAL)
	1))))


;; subtract 1 from state
;; updates x,y position from xv,yv
(definec psystem_update
  (lambda (psys:psystem*)
    (let ((size (psystem_size psys))
	  (states (psystem_states psys))
	  (xs (psystem_xs psys))
	  (ys (psystem_ys psys))	  
	  (xvs (psystem_xvs psys))
	  (yvs (psystem_yvs psys))
	  (i 0)
	  (cnt 0))
      (dotimes (i size)
	(if (> (pref states i) 0)
	    (begin (set! cnt (+ cnt 1))
		   (pset! states i (- (pref states i) 1))
		   (pset! xs i (+ (pref xs i) (pref xvs i)))
		   (pset! ys i (+ (pref ys i) (pref yvs i))))))
      cnt))) ;; return number of active particles
    
	