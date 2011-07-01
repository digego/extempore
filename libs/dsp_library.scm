;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; A DSP Library.
;;
;; Please keep in mind that these functions
;; are provided as EXAMPLES ONLY.  They are
;; things that I've thown together and are
;; not to be considered "production" in any way.
;; In particular they are very very inefficient!!
;;
;; You will need to have libsndfile installed
;; before evaluating this file.
;;
;; Please feel free to fix things and contribute
;; extras juicy bits and pieces
;;
;; C-x h     ; selects whole buffer
;; C-x C-r   ; eval selection
;;
;; Contains:
;; Sine, Square, Saw, Pulse waves
;; Delay, TapDelay, Comb (variable length delay)
;; AllPass, Reverb, Flanger and Chorus
;; LowPass, HighPass, BandPass and Notch filters
;; BitCrusher
;;
;; Instrument abstraction
;; + A default 'synth' and a default 'sampler'
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; silence
(definec:dsp dsp
  (lambda (in time chan dat)
    0.0))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; high limit
;; low limit
;; then value
(definec range-limit
  (lambda (h:double l:double v:double)
    (if (< v l) l
	(if (> v h) h
	    v))))

(definec make-oscil
  (lambda (phase)
    (lambda (amp freq)
      (let ((inc (* TWOPI (/ freq *samplerate*))))
	(set! phase (+ phase inc))
	(* amp (sin phase))))))

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


;; iir comb without interpolation
;; more efficient than comb if you
;; don't need variable length
(definec make-delay
  (lambda (max-delay)
    (let ((line (heap-alloc max-delay double))
	  (time 0)
	  (delay max-delay)
	  (in 0.5)
	  (out 0.5))
      (lambda (x:double)
	(let* ((n (modulo time delay))
	       (delayed (aref line n))
	       (y (+ (* in x) (* out delayed))))
	  (aset! line n y)
	  (set! time (+ time 1))
	  y)))))


;; iir comb with interpolation
(definec make-comb
  (lambda (max-delay)
    (let ((line (heap-alloc max-delay double))
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


;; chorus
(definec make-chorus
  (lambda (phase)
    (let ((delay 700.0)
	  (range 200.0)
	  (rate 0.1)
	  (comb1 (make-comb (dtoi64 (+ delay range))))
	  (comb2 (make-comb (dtoi64 (+ delay range))))
	  (comb3 (make-comb (dtoi64 (+ delay range))))
	  (mrng1 range)
	  (mrng2 (* (random) range))
	  (mrng3 (* (random) range))
	  (mrte1 rate)
	  (mrte2 (* rate 1.372))
	  (mrte3 (* rate 0.792))
	  (dly1 delay)
	  (dly2 (* (random) delay))
	  (dly3 (* (random) delay))
	  (mod1 (make-oscil phase))
	  (mod2 (make-oscil phase))
	  (mod3 (make-oscil phase)))
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
    (let ((line (heap-alloc max-delay double))
	  (taps (heap-alloc num-of-taps i64))
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
    (let ((inline (heap-alloc delay double))
	  (outline (heap-alloc delay double))
	  (time 0)
	  (g 0.9))
      (lambda (x)
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


;; a dodgy reverb
(definec make-reverb
  (lambda (size) ; size in ms
    (let ((ms (/ *samplerate* 1000.0))
	  (wet .25)
	  (dly1 (make-delay (dtoi64 (* ms (* .192 size)))))
	  (dly2 (make-delay (dtoi64 (* ms (* .373 size)))))
	  (dly3 (make-delay (dtoi64 (* ms (* .671 size)))))
	  (dly4 (make-delay (dtoi64 (* ms (* .712 size)))))
	  (ap1 (make-allpass (dtoi64 (* ms size))))
	  (ap2 (make-allpass (dtoi64 (* ms (* .329 size))))))
      (ap1.g .8)
      (ap2.g .8)
      (lambda (in)
	(let ((wetin (* in wet)))
	  (+ (* in (- 1.0 wet)) (ap1 (ap2 (+ (dly1 wetin)
					     (dly2 wetin)
					     (dly3 wetin)
					     (dly4 wetin))))))))))


;; a dodgy bitcrusher
(definec make-crusher
  (lambda (bits)
    (let ((amp 1.0))
      (lambda (in)
	(* amp (/ (floor (* in (pow 2. bits))) 
		  (pow 2. bits)))))))


;; a dodgy amp distortion
(definec make-distort
  (lambda (gain)
    (let ((lim 0.5))
      (lambda (in)
	(range-limit lim (* -1.0 lim) (* gain in))))))


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
    (let ((lines (heap-alloc num-of-points [double,double]*)))
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
	   (data (heap-alloc (* points 2) double)))
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

;; make-note should probably use relative not absolute time?
;; if we decide to keep this we can remove start-time

;; absolute time
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
	    (begin (aset! nstarts idx 9999999999999.0) 0.0)
	    (kernel (- time start-time) channel freq (* (env time) amp)))))))


;; relative time
(definec make-note
  (lambda (start-time:double freq:double amp:double dur 
		      attack decay release sus-amp
		      nstarts:double*
		      idx:i64 kernel:[double,double,double,double,double]*)
    (let ((env (if (< (+ attack decay) dur)
		   (make-adsr 0.0 attack decay (- dur (+ attack decay)) release 1.0 sus-amp)
		   (make-adsr 0.0 0.0 0.0 dur release 1.0 sus-amp)))
	  (t 0.0))
      (lambda (sample:double time:double channel:double)
	(if (< channel 1.0) (set! t (+ t 1.0)))
	(if (< t (+ dur release))
	    (kernel t channel freq (* (env t) amp))
	    (begin (aset! nstarts idx 9999999999999.0) 0.0))))))


(define-macro (define-instrument name note-kernel effect-kernel)
  `(definec ,name
     (let* ((poly 48)
	    (notes (heap-alloc poly [double,double,double,double]*))
	    (attack 200.0)
	    (decay 200.0)
	    (release 1000.0)
	    (sustain 0.6) ;; amplitude of the sustain
	    (gain 2.0)
	    (active 0)
	    (note-starts (heap-alloc poly double))
	    (new-note (lambda (start freq dur amp)
			(let ((free-note -1))
			  (dotimes (i poly) ;; check for free poly spot           
			    (if (> (aref note-starts i) 9999999999998.0)
				(set! free-note i)))
			  (if (= 0 active)
			      (begin (dotimes (iii poly) (aset! note-starts iii 9999999999999.0))
				     (set! free-note -1)))
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
	   (* gain (,effect-kernel out time chan dat)))))))


(definec _synth-note
  (lambda (time inst:[double,double,double,double,double*]* freq amp dur)
    ((inst.new-note:[i64,double,double,double,double]*) time freq dur amp)))
  

(definec midi2frq    
  (lambda (pitch)            
    (* 440.0 (pow 2.0 (/ (- pitch 69.0) 12.0)))))

(definec frq2midi
  (lambda (freq)            
    (+ (* 12.0 (log2 (/ freq 440.0))) 69.0)))

;; playnote wrapper
(define-macro (play-note time inst pitch vol dur)
  `(let ((zone (sys:create-mzone (* 1024 1024)))
	 (default-zone *impc:zone*)	 
	 (duration (* 1.0 ,dur))) ; (* ,dur (* *samplerate* (/ 60 (*metro* 'get-tempo))))))
     (sys:destroy-mzone zone (+ duration (* 5 60.0 *samplerate*))) ; 3 minutes later?
     (set! *impc:zone* zone)
     (_synth-note (integer->real ,time) 
		  (llvm:get-native-closure ,(symbol->string inst))
		  (midi2frq (* 1.0 ,pitch))
		  (/ (exp (/ ,vol 26.222)) 127.0)
		  duration)
     (set! *impc:zone* default-zone)))

;; make synth defaults
(definec default-note
  (lambda ()
    (let ((sawl (make-saw))
	  (sawr (make-saw)))
      (lambda (time:double chan:double freq:double amp:double)
	(if (< chan 1.0)
	    (* amp (/ 200.0 freq) (sawl amp freq))
	    (* amp (/ 200.0 freq) (sawr amp freq)))))))

(definec default-effect
  (lambda (in:double time:double chan:double dat:double*)
    in))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Default synth stuff
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(definec synth-note
  (let ((res 1.0)
	(res_ 0.0)
	(fxamp 0.0))
    (lambda ()
      (let ((oscl (make-oscil 0.0))
	    (oscl3 (make-oscil 0.0))
	    (oscl2 (make-oscil 0.0))
	    (oscr (make-oscil 0.25))
	    (saw1 (make-saw))
	    (saw2 (make-saw))
	    (lpf1 (make-lpf))
	    (lpf2 (make-lpf))
	    (oscr3 (make-oscil 0.25))
	    (oscr2 (make-oscil 0.25)))
	(lpf2.res res)
	(lpf1.res res)	
	(lambda (time:double chan:double freq:double amp:double)
	  (if (<> res res_)
	      (begin (lpf2.res res)
		     (lpf1.res res)
		     (set! res_ res)))
	  (if (< chan 1.0)
	      (* amp (/ 5.0 (log freq)) ;; frequency scale amplitude
		 (+ (oscl2 1.0 (+ freq (* 10.0 (random))))
		    (lpf1 (saw1 fxamp freq) (* 5.0 freq))
		    (oscl 0.8 (+ freq (oscl3 (* 2.01 freq)
					     (* freq 1.01))))))
	      (* amp (/ 5.0 (log freq)) ;; frequency scale amplitude	       
		 (+ (oscr2 1.0 (+ freq (* 5.0 (random))))
		    (lpf2 (saw2 fxamp freq) (* 3.0 freq))
		    (oscr 0.8 (+ freq (oscr3 (* 0.99 freq)
					     (* freq 0.99))))))))))))


(definec synth-fx
  (let ((dleft (dtoi64 (* 0.125 *samplerate*)))
	(dlyl (make-delay dleft))
	(dright (dtoi64 (* 0.33333333 *samplerate*)))
	(dlyr (make-delay dright))
	(pan .5)
	(pan_old pan)
	(wet_ .0)
	(wet .3))
    (lambda (in:double time:double chan:double dat:double*)
      (if (< pan_old pan) ;; interpolate pan
	  (set! pan_old (+ pan_old .001))
	  (set! pan_old (- pan_old .001)))
      (if (<> wet wet_)
	  (begin (dlyl.out wet)
		 (dlyr.out wet)
		 (set! wet_ wet)))
      (cond ((< chan 1.0) 
	     (dlyl (* 2.0 pan_old in)))
	    ((< chan 2.0)
	     (dlyr (* 2.0 (- 1.0 pan_old) in)))
	    (else 0.0)))))

;; define default instrument called synth
(define-instrument synth synth-note synth-fx)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Audio File Reading Stuff
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; load highgui dynamic library
(define libsndfile (if (string=? "Linux" (sys:platform))
		       (sys:open-dylib "libsndfile.so.1")
		       (sys:open-dylib "libsndfile.1.dylib")))


;; bind 3 sndfile lib functions
(bind-lib libsndfile sf_open [i8*,i8*,i32,<i64,i32,i32,i32,i32,i32>*]*)
(bind-lib libsndfile sf_read_double [i64,i8*,double*,i64]*)
(bind-lib libsndfile sf_seek [i64,i8*,i64,i32]*)
(bind-lib libsndfile sf_strerror [i8*,i8*]*)

;; size of audio data in file (in bytes)
(definec print-audio-file-info
  (lambda (fname)
    (let ((info (make-tuple i64 i32 i32 i32 i32 i32))
	  (audiofile (sf_open fname 16 info))
	  (channels (i32toi64 (tref info 2))))
      (printf "---------------\n")
      (printf "filename:\t %s\n" fname)
      (printf "errors:\t\t %s\n" (sf_strerror audiofile))
      (printf "samplerate:\t %d\n" (tref info 1))
      (printf "channels:\t %d\n" (tref info 2))
      (printf "frames:\t\t %lld\n" (tref info 0))
      (printf "seconds:\t %f\n"
	      (/ (i64tod (/ (tref info 0) (i32toi64 (tref info 2))))
		 (i32tod (tref info 1)))))))

;; an audio buffer reader
(definec read-audio-data
  (lambda (fname dat offset num)
    ;(printf "in: %s %p %lld %lld\n" fname dat offset num)
    (let ((info (make-tuple i64 i32 i32 i32 i32 i32))
	  (audiofile (sf_open fname 16 info))
	  (cnt (sf_seek audiofile offset 0))
	  (samples-read (sf_read_double audiofile dat num)))
      samples-read)))


;; helper function for adding sample data to sampler
;; this assumes stereo files at the moment!!
;; passing 0 for length means read to end of file.
;;
;; for example:
;; ;; this reads the whole file into index 60
;; (set-sampler-data sampler "/tmp/piano-C.aif" 60 0 0) 
;; ;; this reads 5000 frames starting 1000 frames into the file
;; (set-sampler-data sampler "/tmp/piano-C.aif" 60 1000 5000) 
(definec set-sample-data_
  (lambda (inst:[double,double,double,double*]* fname index offset length)    
    (let ((info (make-tuple i64 i32 i32 i32 i32 i32))
	  (audiofile (sf_open fname 16 info))
	  (channels (i32toi64 (tref info 2)))		     
	  (num (if (= 0 length)
		   (* (- (tref info 0) offset) channels)
		   (* length channels))))
      (if (<> null audiofile)
	  (let ((adat_ (malloc (* num 8)))
		(adat (bitcast adat_ double*))
		(samples (inst.samples:|128,double*|*))
		(samples-length (inst.samples-length:|128,i64|*))
		(read (read-audio-data fname adat (* offset channels) num)))
	    (if (<> 0 (aref samples-length index))
		(begin (free (aref samples index)) 1))
	    (aset! samples index adat)
	    (aset! samples-length index (/ read channels)) ;num)
	    (printf "read %lld(frames):%f(k) into sampler index: %lld\n" (/ read channels) (/ (i64tod (* num 8)) 1024.) index)
	    1)
	  (begin (printf "%s\n" (sf_strerror audiofile))
		 0)))))


;; passing a length of 0 will read the whole file
(define-macro (set-sampler-index inst fname index offset length)
  `(set-sample-data_ (llvm:get-native-closure ,(symbol->string inst))
		     ,fname
		     (real->integer ,index)
		     (real->integer ,offset)
		     (real->integer ,length)))


;; helper functions for setting an individual samples offset
;; i.e. set sample index 60 to start at 40000 samples into the audio buffer
(definec set-sample-offset_
  (lambda (sampler:[double,double,double,double*]* index:i64 offset:i64)
    (let ((offsets (sampler.samples-offsets:|128,i64|*)))
      (aset! offsets index offset)
      1)))


;; (set-sample-offset sampler 60 50000)
;; this would offset the sample at index 60 by 50000 samples
(define-macro (set-sample-offset inst index offset)
  `(set-sample-offset_ (llvm:get-native-closure "sampler")
		       (real->integer ,index)
		       (real->integer ,offset)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; SAMPLER STUFF
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(definec hermite-interp
  (lambda (fractional y1:double x0 x1 x2)
    (let ((c (* 0.5 (- x1 y1)))
	  (v (- x0 x1))
	  (w (+ c v))
	  (a (+ w v (* (- x2 x0) 0.5)))
	  (b (+ w a)))
      (+ (* (+ (* (- (* a fractional) b) fractional) c) fractional) x0))))


;; make synth defaults
(definec sampler-note
  (lambda (samples:|128,double*|* samples-length:|128,i64|* samples-offsets:|128,i64|* index)
    (let ((idx-freq (midi2frq (i64tod index)))
	  (phase (i64tod (aref samples-offsets index)))) ;; phase unit is audio frames
      (lambda (time:double chan:double freq:double amp:double)
	(let ((rate (/ freq idx-freq))
	      (pos (if (< chan 1.0) ;; only increment once per frame
		       (set! phase (+ phase rate))
		       phase))
	      (posi (dtoi64 (floor pos)))
	      (posr (modulo pos 1.0))
	      (posx (+ (* posi 2) (dtoi64 chan)))
	      (length (- (aref samples-length index) 10))
	      (dat (aref samples index))
	      (y1 (if (or (> posi length) (< posi 1)) 0.0 (aref dat (- posx 2)))) ; assume stereo
	      (x0 (if (> posi length) 0.0 (aref dat posx)))
	      (x1 (if (> (+ posi 1) length) 0.0 (aref dat (+ posx 2)))) ; assume stereo
	      (x2 (if (> (+ posi 2) length) 0.0 (aref dat (+ posx 4))))) ; assume stereo
	  (* amp (hermite-interp posr y1 x0 x1 x2)))))))


(definec sampler-fx
  (let ((reverbl (make-reverb 80.0))
	(reverbr (make-reverb 79.0))	
	(pan .5)
	(pan_ pan)
	(wet .25)
	(wet_ wet))
    ;; some reverb?
    (reverbl.wet wet)
    (reverbr.wet wet)
    (lambda (in:double time:double chan:double dat:double*)      
      (if (<> wet wet_)
	  (begin (reverbl.wet wet)
		 (reverbr.wet wet)
		 (set! wet_ wet)))
      (if (< pan pan_)
	  (set! pan_ (- pan_ .001))
	  (set! pan_ (+ pan_ .001)))
      (if (< chan 1.0)
	  (reverbl (* 1.5 (* pan_ in)))
	  (reverbr (* 1.5 (* (- 1.0 pan_) in)))))))


;; make sampler instrument
(define-macro (define-sampler name note-kernel effect-kernel)
  `(definec ,name
     (let* ((poly 48)
	    (samples (make-array 128 double*)) ;; 128 samples
	    (samples-length (make-array 128 i64)) ;; 128 samples
	    (samples-offsets (make-array 128 i64)) ;; 128 samples
	    (notes (heap-alloc poly [double,double,double,double]*))
	    (attack 200.0)
	    (decay 200.0)
	    (release 1000.0)
	    (sustain 1.0) ;; amplitude of the sustain
	    (gain 2.0)
	    (active 0)
	    (note-starts (heap-alloc poly double))
	    (new-note (lambda (start freq dur amp)
			(let ((free-note -1)
			      (idx (dtoi64 (floor (frq2midi freq))))
			      (closest 1000000)
			      (new-idx idx))
			  (dotimes (i poly) ;; check for free poly spot           
			    (if (> (aref note-starts i) 9999999999998.0)
				(set! free-note i)))
			  (if (= 0 active)
			      (begin (dotimes (iii poly) (aset! note-starts iii 9999999999999.0))
				     (set! free-note -1)))
			  (if (> free-note -1) ;; if we found a free poly spot assign a note  
			      (begin (dotimes (idxi 128)
				       (let ((v (llabs (- idx idxi))))
					 (if (and (<> (aref samples-length idxi) 0)
						  (< v closest))
				      (begin (set! new-idx idxi)
					     (set! closest v) 0))))
				     (aset! notes free-note
					    (make-note start freq amp dur
						       attack decay release sustain
						       note-starts free-note
						       (,note-kernel samples samples-length samples-offsets new-idx)))
				     (aset! note-starts free-note start)
				     1)
			      0)))))
       (dotimes (kk 128)
	 (aset! samples-offsets kk 0)
	 (aset! samples-length kk 0))
       (dotimes (ii poly) ;; sets all notes to inactive
	 (aset! note-starts ii 9999999999999.0))
       (lambda (in:double time:double chan:double dat:double*)
	 (let ((out 0.0))
	   (dotimes (k poly) ;; sum all active notesx   
	     (if (< (aref note-starts k) time)
		 (set! out (+ out (* 0.3 ((aref notes k) in time chan))))))
	   (* gain (,effect-kernel out time chan dat)))))))


;; create a default sampler
(define-sampler sampler sampler-note sampler-fx)

(define audio-file-regex-match
  (lambda (fname)
    (if (regex:match? fname "^.*[ABCDEFGabcdefg][#]?[0-9].*(wav|aif|aiff|ogg)$")
	(let ((result (regex:matched fname "([ABCDEFGabcdefg])(.*)([0-9])")))
	  (if (null? result) #f
	      (let ((sharp (if (regex:match? (caddr result) "#") #t #f))
		    (offset (+ 12 (* (string->number (cadddr result)) 12)))
		    (pc (case (modulo (- (modulo (char->integer (car (string->list (cadr result)))) 16) 3) 7)
			  ((0) 0) ((1) 2) ((2) 4) ((3) 5) ((4) 7) ((5) 9) ((6) 11))))
		(if sharp (+ pc offset 1) (+ offset pc))))))))
	      

;; must be stereo samples of type wav aif or ogg
(define-macro (load-sampler sampler path)
  `(let ((files (sys:directory-list ,path)))
     (for-each (lambda (f)
		 (if (regex:match? f "^([0-9]*)\.(wav|aif|aiff|ogg)$")
		     (let ((result (regex:matched f "^([0-9]*)\.(wav|aif|aiff|ogg)$")))
		       (set-sampler-index ,sampler (string-append ,path "/" f)
					  (string->number (cadr result)) 0 0))
		     (let ((result (audio-file-regex-match f)))
		       (if (number? result)
			   (set-sampler-index ,sampler (string-append ,path "/" f)
					      result 0 0)))))
	       files)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Default DSP
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; setup default synth to play
(definec:dsp dsp
  (lambda (in time chan dat)
    (cond ((< chan 2.0)
	   (+ (synth in time chan dat)
	      (sampler in time chan dat)))
	  (else 0.0))))


(ipc:call (ipc:get-process-name) 'print)
(ipc:call (ipc:get-process-name) 'ascii-print-color 1 7 10) 
(ipc:call (ipc:get-process-name) 'print "All set to go!\n")
(ipc:call (ipc:get-process-name) 'synth.active 1)
(ipc:call (ipc:get-process-name) 'ascii-print-color 0 7 10)
(ipc:call (ipc:get-process-name) 'print "'Synth' active = true\n")
(ipc:call (ipc:get-process-name) 'sampler.active 1)
(ipc:call (ipc:get-process-name) 'print "'Sampler' active = true\n")
(ipc:call (ipc:get-process-name) 'print)
(ipc:call (ipc:get-process-name) 'print "You have two default instruments loaded 'synth' and 'sampler'\n")
(ipc:call (ipc:get-process-name) 'print)
(ipc:call (ipc:get-process-name) 'callback 0 '_dsp:set! 'dsp)
