;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Some DSP examples.
;;
;; USE AT YOUR OWN RISK!
;;
;; Please keep in mind that these functions
;; are provided as EXAMPLES ONLY.  They are
;; things that I've thown together and are
;; not to be considered "production" in any way.
;; In particular they are very very inefficient!!
;;
;; C-x h     ; selections whole buffer
;; C-x C-r   ; eval each expression in turn
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; sine oscillator function
(definec make-oscil
  (lambda (phase)
     (lambda (amp freq)
       (let ((inc (* TWOPI (/ freq *samplerate*))))
	 (set! phase (+ phase inc))
	 (* amp (sin phase)))))))


;; square oscillator
(definec make-square
   (lambda (phase)   
      (let ((osc (make-oscil phase))
            (n 50.0))         
	(lambda (amp freq)
            (* amp (tanh (* n (osc 1.0 freq))))))))


;; saw oscillator
(definec make-saw
  (lambda ()
    (let ((p 0.0)
	  (dp 1.0)
	  (x 0.0)
	  (leak 0.995)
	  (saw 0.0))
      (lambda (amp freq)
	(let* ((qmax (* 0.5 (/ *samplerate* freq)))
	       (dc (/ -0.498 qmax)))
	  (set! p (+ p dp))
	  (if (< p 0.0) 
	      (begin (set! p (- 0.0 p))
		     (set! dp (- 0.0 dp)))
	      (if (> p qmax)
		  (begin (set! p (+ qmax (- qmax p)))
			 (set! dp (- 0.0 dp)))))
	  (set! x (* PI p))
	  (if (< x 0.000001) (set! x 0.00001))
	  (set! saw (* leak (+ saw (+ dc (/ (sin x) x)))))
	  (* amp saw))))))


;; pulse train
(definec make-pulse
   (lambda ()
      (let ((time -1.0)
	    (width 100.0))
	(lambda (amp:double freq)
	  (let ((period (/ *samplerate* freq)))
	    (set! time (+ time 1.0))
	    (if (< (modulo time period) width)
		amp
		0.0))))))


;; delay with interpolation
(definec make-comb
   (lambda (max-delay)
      (let ((line (make-array max-delay double))
	    (in-head 0)
	    (out-head 0)
	    (delay_ (i64tod max-delay))
	    (delay (i64tod max-delay))
	    (alpha 0.0)
	    (om_alpha 1.0)
	    (in 1.0)
	    (out 0.5))
	(dotimes (i max-delay) (aset! line i 0.0))
	(lambda (x:double)
	  (if (<> delay delay_)
	      (begin (set! delay_ delay)		 
		     (set! alpha (- delay (floor delay)))
		     (set! om_alpha (- 1.0 alpha))
		     (set! out-head (- (+ max-delay in-head)
				       (dtoi64 delay)))))
	  (let* ((ih:i64 (modulo in-head max-delay))
		 (oh:i64 (modulo out-head max-delay))
		 (delayed1 (aref line oh))
		 (delayed2 (aref line (modulo (+ oh 1) max-delay)))
		 (delayed (+ (* alpha delayed1) (* om_alpha delayed2))) 
		 (y (+ (* in x) (* out delayed))))
	    (aset! line ih y)
	    (set! in-head (+ ih 1))
	    (set! out-head (+ oh 1))
	    y)))))


;; flanger
(definec make-flanger
  (lambda (delay mod-phase mod-range mod-rate)
    (let ((comb (make-comb (dtoi64 (+ delay mod-range))))
	  (mod (make-oscil mod-phase)))
      (lambda (x:double)
	(comb.delay (+ delay (mod mod-range mod-rate)))
	(comb x)))))   


;; chorus fx
(definec make-chorus
  (lambda (delay mod-phase mod-range mod-rate)
    (let ((comb1 (make-comb (dtoi64 (+ delay mod-range))))
	  (comb2 (make-comb (dtoi64 (+ delay mod-range))))
	  (comb3 (make-comb (dtoi64 (+ delay mod-range))))
	  (mrng1 mod-range)
	  (mrng2 (* (random) mod-range))
	  (mrng3 (* (random) mod-range))
	  (mrte1 mod-rate)
	  (mrte2 (* mod-rate 1.372))
	  (mrte3 (* mod-rate 0.792))
	  (dly1 delay)
	  (dly2 (* (random) delay))
	  (dly3 (* (random) delay))
	  (mod1 (make-oscil mod-phase))
	  (mod2 (make-oscil mod-phase))
	  (mod3 (make-oscil mod-phase)))
      (comb1.in .5)
      (comb2.in .5)
      (comb3.in .5)
      (lambda (x:double)
	(comb1.delay (+ dly1 (mod1 mrng1 mrte1)))
	(comb2.delay (+ dly2 (mod2 mrng2 mrte2)))
	(comb3.delay (+ dly3 (mod3 mrng3 mrte3)))
	(+ (comb1 x)
	   (comb2 x)
	   (comb3 x))))))
 

;; tap delay
(definec tap-delay
   (lambda (max-delay num-of-taps)
      (let ((line (make-array max-delay double))
	    (taps (make-array num-of-taps i64))
	    (delay max-delay)
	    (time 0))
         (lambda (x:double)
            (let ((y 0.0)
                  (n (modulo time delay))
                  (gain (/ 1.0 (i64tod num-of-taps))))
               (aset! line n x)
               (dotimes (i num-of-taps)
                  (set! y (+ y (* gain (aref line (modulo (+ (aref taps i) n) delay))))))
               (set! time (+ time 1))
               y)))))


;; allpass
(definec make-allpass
   (lambda (delay)
      (let ((inline (make-array delay double))
            (outline (make-array delay double))
            (time 0))
         (lambda (x g)
            (let* ((n (modulo time delay))
                   (dy (aref outline n))
                   (dx (aref inline n))
                   (y (+ (* -1.0 g x)
                         dx
                         (* g dy))))
               (aset! inline n x)
               (aset! outline n y)
               (set! time (+ time 1))
               y)))))


;; a very dodgy bitcrusher
(definec make-crusher
  (lambda (bits)
    (let ((amp 1.0))
      (lambda (in)
	(* amp (/ (floor (* in (pow 2. bits))) 
		  (pow 2. bits)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BiQuad coefficient formulae from 
;; Audio EQ Cookbook Robert Bristow-Johnson
;;
;; http://www.musicdsp.org/files/Audio-EQ-Cookbook.txt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; biquad low-pass filter
(definec make-lpf
  (lambda ()
    (let* ((y1 0.0)
	   (y2 0.0)
	   (x1 0.0)
	   (x2 0.0)
	   (b0 0.0)
	   (b1 0.0)
	   (b2 0.0)
	   (a0 0.0)
	   (a1 0.0)
	   (a2 0.0)
	   (res 0.25)
	   (oldres 0.0)
	   (oldfreq 0.0))
      (lambda (x freq)
	;; if frequency changes
	;; recalculate coefficients
	(if (or (<> freq oldfreq)
		(<> res oldres))
	    (let* ((omega (* TWOPI (/ freq *samplerate*)))
		   (sino (sin omega))
		   (coso (cos omega))
		   (alpha (/ sino (* 2.0 res))))
	      (set! oldfreq freq)
	      (set! oldres res)
	      (set! b0 (/ (- 1.0 coso) 2.0))
	      (set! b1 (- 1.0 coso))
	      (set! b2 b0)
	      (set! a0 (+ 1.0 alpha))
	      (set! a1 (* -2.0 coso))
	      (set! a2 (- 1.0 alpha))))
	(let ((y (- (+ (* (/ b0 a0) x)
		       (* (/ b1 a0) x1)
		       (* (/ b2 a0) x2))
		    (* (/ a1 a0) y1)
		    (* (/ a2 a0) y2))))
	  (set! y2 y1)
	  (set! y1 y)
	  (set! x2 x1)
	  (set! x1 x)
	  y)))))


;; biquad high-pass filter
(definec make-hpf
   (lambda ()
      (let* ((y1 0.0)
             (y2 0.0)
             (x1 0.0)
             (x2 0.0)
             (b0 0.0)
             (b1 0.0)
             (b2 0.0)
             (a0 0.0)
             (a1 0.0)
             (a2 0.0)
	     (res 0.25)
	     (oldres 0.0)
             (oldfreq 0.0))
         (lambda (x freq)
            ;; if frequency changes
            ;; recalculate coefficients
            (if (or (<> freq oldfreq)
		    (<> res oldres))
                (let* ((omega (* TWOPI (/ freq *samplerate*)))
                       (sino (sin omega))
                       (coso (cos omega))
                       (alpha (/ sino (* 2.0 res))))
                   (set! oldfreq freq)
                   (set! oldres res)
                   (set! b0 (/ (+ 1.0 coso) 2.0))
                   (set! b1 (* -1.0 (+ 1.0 coso)))
                   (set! b2 b0)
                   (set! a0 (+ 1.0 alpha))
                   (set! a1 (* -2.0 coso))
                   (set! a2 (- 1.0 alpha))))
            (let ((y (- (+ (* (/ b0 a0) x)
                           (* (/ b1 a0) x1)
                           (* (/ b2 a0) x2))
                        (* (/ a1 a0) y1)
                        (* (/ a2 a0) y2))))
               (set! y2 y1)
               (set! y1 y)
               (set! x2 x1)
               (set! x1 x)
               y)))))


;; biquad band-pass filter
(definec make-bpf
   (lambda () 
      (let* ((y1 0.0)
             (y2 0.0)
             (x1 0.0)
             (x2 0.0)
             (b0 0.0)
             (b1 0.0)
             (b2 0.0)
             (a0 0.0)
             (a1 0.0)
             (a2 0.0)
	     (bandwidth 0.5)
             (oldfreq 0.0)
             (oldbw 0.0))
         ;; bandwidth in octaves
         (lambda (x freq)
            ;; if frequency or bandwidth change
            ;; recalculate coefficients
            (if (or (<> freq oldfreq)
                    (<> bandwidth oldbw))
                (let* ((omega (* 1.0 TWOPI (/ freq *samplerate*)))
                       (sino (sin omega))
                       (coso (cos omega))
                       (alpha (* sino (sinh (* (/ (log2 2.0) 2.0)
                                               bandwidth
                                               (/ omega sino))))))
                   (set! oldfreq freq)
                   (set! oldbw bandwidth)
                   (set! b0 alpha)
                   (set! b1 0.0) 
                   (set! b2 (* -1.0 b0))
                   (set! a0 (+ 1.0 alpha))
                   (set! a1 (* -2.0 coso))
                   (set! a2 (- 1.0 alpha))))
            (let ((y (- (+ (* (/ b0 a0) x)
                           (* (/ b1 a0) x1)
                           (* (/ b2 a0) x2))
                        (* (/ a1 a0) y1)
                        (* (/ a2 a0) y2))))
               (set! y2 y1)
               (set! y1 y)
               (set! x2 x1)
               (set! x1 x)
               y)))))


;; biquad notch filter
(definec make-notch
   (lambda () 
      (let* ((y1 0.0)
             (y2 0.0)
             (x1 0.0)
             (x2 0.0)
             (b0 0.0)
             (b1 0.0)
             (b2 0.0)
             (a0 0.0)
             (a1 0.0)
             (a2 0.0)	     
	     (bandwidth 0.5) ; in ocatves
             (oldfreq 0.0)
             (oldbw 0.0))
         ;; bandwidth in octaves
         (lambda (x freq)
            ;; if frequency or bandwidth change
            ;; recalculate coefficients
            (if (or (<> freq oldfreq)
                    (<> bandwidth oldbw))
                (let* ((omega (* TWOPI (/ freq *samplerate*)))
                       (sino (sin omega))
                       (coso (cos omega))
                       (alpha (* sino (sinh (* (/ (log2 2.0) 2.0)
                                               bandwidth
                                               (/ omega sino))))))
                   (set! oldfreq freq)
                   (set! oldbw bandwidth)
                   (set! b0 1.0)
                   (set! b1 (* -2.0 coso)) 
                   (set! b2 b0)
                   (set! a0 (+ 1.0 alpha))
                   (set! a1 b1)
                   (set! a2 (- 1.0 alpha))))
            (let ((y (- (+ (* (/ b0 a0) x)
                           (* (/ b1 a0) x1)
                           (* (/ b2 a0) x2))
                        (* (/ a1 a0) y1)
                        (* (/ a2 a0) y2))))
               (set! y2 y1)
               (set! y1 y)
               (set! x2 x1)
               (set! x1 x)
               y)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; envelope stuff
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(definec make-line
  (lambda (x1:double y1:double x2 y2)
    (let* ((m (if (= 0.0 (- x2 x1)) 
		  0.0 
		  (/ (- y2 y1) (- x2 x1))))
	   (c (- y2 (* m x2))))
      (lambda (time) (+ (* m time) c)))))
        

(definec envelope-segments
   (lambda (points:double* num-of-points:i64)
      (let ((lines (make-array num-of-points [double,double]*)))
         (dotimes (k num-of-points)
             (let* ((idx (* k 2))
                    (x1 (aref points (+ idx 0)))
                    (y1 (aref points (+ idx 1)))
                    (x2 (aref points (+ idx 2)))
                    (y2 (aref points (+ idx 3))))
	       (aset! lines k (make-line x1 y1 x2 y2))))
	 lines)))

(definec make-envelope
   (lambda (points:double* num-of-points)
      (let ((klines:[double,double]** (envelope-segments points num-of-points))
            (line-length num-of-points))
         (lambda (time)
            (let ((res -1.0))
               (dotimes (k num-of-points)
                  (let ((line (aref klines k))
                        (time-point (aref points (* k 2))))
                     (if (or (= time time-point)
                             (< time-point time))
                         (set! res (line time)))))
               res)))))

;; make a convenience wrapper for asr
(definec make-adsr
  (lambda (start-time atk-dur dky-dur sus-dur rel-dur peek-amp sus-amp)
    (let* ((points 6)
	   (data (make-array (* points 2) double)))
      (aset! data 0 start-time)
      (aset! data 1 0.0)
      (aset! data 2 (+ start-time atk-dur)) ;; point data
      (aset! data 3 peek-amp)
      (aset! data 4 (+ start-time atk-dur dky-dur))
      (aset! data 5 sus-amp)
      (aset! data 6 (+ start-time atk-dur dky-dur sus-dur))
      (aset! data 7 sus-amp)
      (aset! data 8 (+ start-time atk-dur dky-dur sus-dur rel-dur))
      (aset! data 9 0.0)
      (aset! data 10 (+ start-time atk-dur dky-dur sus-dur rel-dur 1)) ;; this to flatten out at 0.0
      (aset! data 11 0.0)
      (let ((f (make-envelope data points)))
	(lambda (time:double)
	  (f time))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; INSTRUMENT STUFF
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(definec make-note
  (lambda (start-time freq:double amp:double dur 
		      attack decay release sus-amp
		      nstarts:double*
		      idx:i64 kernel:[double,double,double,double,double]*)
    (let ((env (if (< (+ attack decay) dur)
		   (make-adsr start-time attack decay (- dur (+ attack decay)) release 1.0 sus-amp)
		   (make-adsr start-time 0.0 0.0 dur release 1.0 sus-amp))))
      (lambda (sample:double time:double channel:double)
	(if (> time (+ start-time dur release))
	    (begin (aset! nstarts idx 9999999999999.0) 0.0))
	(kernel (- time start-time) channel freq (* (env time) amp))))))


(define-macro (define-instrument name note-kernel effect-kernel)
  `(definec ,name
     (let* ((poly 48)
	    (notes (make-array poly [double,double,double,double]*))
	    (attack 200.0)
	    (decay 200.0)
	    (release 1000.0)
	    (sustain 0.6) ;; amplitude of the sustain
	    (note-starts (make-array poly double))
	    (new-note (lambda (start freq dur amp)
			(let ((free-note -1))
			  (dotimes (i poly) ;; check for free poly spot           
				   (if (> (aref note-starts i) 9999999999998.0)
				       (set! free-note i)))
			  (if (> free-note -1) ;; if we found a free poly spot assign a note  
			      (begin (aset! notes free-note
					    (make-note start freq amp dur
						       attack decay release sustain
						       note-starts free-note
						       (,note-kernel)))
				     (aset! note-starts free-note start)
				     1)
			      0)))))
       (dotimes (ii poly) ;; sets all notes to inactive
		(aset! note-starts ii 9999999999999.0))
       (lambda (in:double time:double chan:double dat:double*)
	 (let ((out 0.0))
	   (dotimes (k poly) ;; sum all active notes          
		    (if (< (aref note-starts k) time)
			(set! out (+ out (* 0.3 ((aref notes k) in time chan))))))
	   (,effect-kernel out time chan dat))))))


(definec _synth-note
  (lambda (time inst:[double,double,double,double,double*]* freq amp dur)
    ((inst.new-note:[i64,double,double,double,double]*) time freq dur amp)))


(define midi2frq    
  (lambda (pitch)            
    (* 220.0 (expt 2.0 (/ (- pitch 48.0) 12.0)))))


;; playnote wrapper
(define-macro (play-note time inst pitch vol dur)
  `(let ((zone (sys:create-mzone (* 1024 1024)))
	 (default-zone *impc:zone*)	 
	 (duration (* 1.0 ,dur))) ; (* ,dur (* *samplerate* (/ 60 (*metro* 'get-tempo))))))
     (sys:destroy-mzone zone (+ duration (* 3.0 *samplerate*)))
     (set! *impc:zone* zone)
     (_synth-note (integer->real ,time) 
		 (llvm:get-native-closure ,(symbol->string inst))
		 (midi2frq ,pitch)
		 (/ ,vol 127.0)
		 duration)
     (set! *impc:zone* default-zone)))

;; make synth defaults
(definec default-note
  (lambda ()
    (let ((sawl (make-saw))
	  (sawr (make-saw)))
      (lambda (tim:double chan:double freq:double amp:double)
	(if (< chan 1.0)
	    (* amp (sawl amp freq))
	    (* amp (sawr amp freq)))))))

(definec default-effect
  (lambda (in:double time:double chan:double dat:double*)
    in))

;; make default "synth" instrument
(definec default-synth-note
  (lambda ()
    (let ((oscl (make-oscil 0.0))
	  (oscl3 (make-oscil 0.0))
	  (oscl2 (make-oscil 0.0))
	  (oscr (make-oscil 0.25))
	  (oscr3 (make-oscil 0.25))
	  (oscr2 (make-oscil 0.25)))
      (lambda (time:double chan:double freq:double amp:double)
	(if (< chan 1.0)
	    (* amp (+ (oscl2 0.8 (+ freq (* 10.0 (random))))
		      (oscl 0.8 (+ freq (oscl3 200.0 (* freq 1.001))))))
	    (* amp (+ (oscr2 0.8 (+ freq (* 5.0 (random))))
		      (oscr 0.8 (+ freq (oscr3 400.0 (* freq 0.99)))))))))))


(definec default-synth-effect
  (let ((dleft (dtoi64 (* 0.125 *samplerate*)))
	(combl (make-comb dleft))
	(dright (dtoi64 (* 0.33333333 *samplerate*)))
	(combr (make-comb dright))
	(chorusl (make-chorus 700.0 0.0 200.0 0.1))
	(chorusr (make-chorus 700.0 0.5 200.0 0.1)))
    (combl.out .3)
    (combr.out .3)
    (lambda (in:double time:double chan:double dat:double*)      
      (cond ((< chan 1.0) 
	     (combl (chorusl in)))
	    ((< chan 2.0) 
	     (combr (chorusr in)))
	    (else 0.0)))))

;; define default instrument called synth
(define-instrument synth default-synth-note default-synth-effect)

;; setup default synth to play
(definec:dsp dsp
  (lambda (in time chan dat)
    (cond ((< chan 2.0) (synth in time chan dat))
	  (else 0.0))))

;; you should set dsp manually
;(dsp:set! dsp)

(print)
(print-notification "You will probably want to now call (dsp:set! dsp) if this is the start of a new session")
