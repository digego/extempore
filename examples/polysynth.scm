;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This example implements a basic polyphonic synth from scratch!
;;
;; This code shows everything involved in building a polyphonic 
;; synth.  Nothing is hidden - no stinking UGen abstractions here ;)
;;
;; There is a lot of code here to not do all that much! 
;; But again it's important to keep in mind that there are no
;; abstractions here. All the infrastructure to make this work
;; is in this file.
;;
;; This example is provided for clarity not performance!
;; It is certainly possible to do this more efficiently.
;;
;; NOTE: This code has a memory leak.  This is kinda deliberate
;; as I don't really want to get into the additional complexity of the 
;; memory management code in this example.

;; 
;; First we need to make some utility functions
;;

;;
;; First we will need enveloping
;;
(definec envelope-segments
   (lambda (points:double* num-of-points:i64)
      (let ((lines (make-array num-of-points [double,double]*)))
         (dotimes (k num-of-points)
             (let* ((idx (* k 2))
                    (x1 (aref points (+ idx 0)))
                    (y1 (aref points (+ idx 1)))
                    (x2 (aref points (+ idx 2)))
                    (y2 (aref points (+ idx 3)))
                    (m (if (= 0.0 (- x2 x1)) 0.0 (/ (- y2 y1) (- x2 x1))))
                    (c (- y2 (* m x2)))
                    (l (lambda (time) (+ (* m time) c))))
                (aset! lines k l)))
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


;; we will also need oscillators!
(definec make-oscil
   (lambda (phase)
      (lambda (amp freq)
         (let ((inc (* 3.141592 (* 2.0 (/ freq 44100.)))))
            (set! phase (+ phase inc))
            (* amp (sin phase))))))


;; and a pulse train
(definec make-pulse
   (lambda ()
      (let ((time -1.0))
         (lambda (freq width amp:double)
            (let ((period (/ 44100. freq)))
               (set! time (+ time 1.0))
               (if (< (modulo time period) width)
                   amp
                   0.0))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; On with the real work!


;; Creates a new monophonic 'note'
;;
;; this is the dsp code for a single note
;; includes envelope, panning and modulation
;;
;; individual notes tell the synth when they're done
;;
(definec make-note
  (lambda (start-time freq dur amp width speed nstarts:double* idx:i64)
    (let ((env (make-adsr start-time 300.0 300.0 
			  (- dur 600.0) ; subtract atk and dky
			  500.0 1.0 0.3))
	  (osc (make-oscil 0.0))	  
	  (mod1 (make-oscil 0.0))
	  (mod2 (make-oscil 0.0))
	  (posl (make-oscil 0.0))
	  (posr (make-oscil 0.5))
	  (pulse (make-pulse))
	  (width-mod (make-oscil 0.0))
	  (tmp 0.0))
      (lambda (sample:double time:double channel:double)
	(if (> time (+ start-time dur 500.0)) ;; add adsr release amt
	    (begin (aset! nstarts idx 9999999999999.0) 
		   0.0) 
	    (if (< channel 1.0)
		(begin (set! tmp (* (env time) 
				    (+ (osc (+ amp (mod2 0.1 5.0))
					    (+ freq (mod1 5.0 (/ freq 20.0))))
				       (pulse freq (+ width (width-mod width speed)) 0.2))))
		       (* (posl 0.5 .5) tmp))
		(if (< channel 2.0)
		    (* (posr 0.5 0.5) tmp)
		    0.0)))))))

;; A polyphonic synth
;;
;; Poly synth is responsible for tracking notes
;; new-note "method" instantiates new notes
;;
;; synth holds all notes and tells 'em when to start
;; all "active" notes are summed for output
;;
(definec poly-synth
  (let* ((poly 24)
	 (notes (make-array poly [double,double,double,double]*))
         (note-starts (make-array poly double))
	 (pulse-width 4.0)
	 (pulse-speed 1.0)
	 (new-note (lambda (start freq dur amp)
		     (let ((free-note -1))			 
		       (dotimes (i poly) ;; check for free poly spot
			 (if (> (aref note-starts i) 9999999999998.0)
			     (set! free-note i)))
		       (if (> free-note -1) ;; if we found a free poly spot assign a note
			   (begin (aset! notes free-note 
					 (make-note start freq dur amp 
						    pulse-width pulse-speed
						    note-starts free-note))
				  (aset! note-starts free-note start)		 
				  1)
			   0)))))
    (dotimes (ii poly) ;; sets all notes to inactive
       (aset! note-starts ii 9999999999999.0))
    (lambda (in:double time:double chan:double dat:double*)
      (let ((out 0.0)
	    (pw pulse-width) ;; pw here is a temporary hack - don't ask!
	    (f new-note)) ;; f here is a temporary hack - don't ask!
	(dotimes (k poly) ;; sum all active notes
	    (if (< (aref note-starts k) time)
		(set! out (+ out (* 0.3 ((aref notes k) in time chan))))))
	out))))


;; a helper function for calling poly-synths "new-note method"
(definec synth-note
  (lambda (start freq dur amp)
    ((poly-synth.new-note:[i64,double,double,double,double]*) start freq dur amp)))

;; a helper for setting new-note's "pulse-width slot"
(definec synth-set-pw
  (lambda (width)
    (let ((f (poly-synth.new-note:[i64,double,double,double,double]*)))
      (f.pulse-width:double width))))

;; a helper for setting new-note's "pulse-width slot"
(definec synth-set-pspeed
  (lambda (speed)
    (let ((f (poly-synth.new-note:[i64,double,double,double,double]*)))
      (f.pulse-speed:double speed))))

;; set synth as primary dsp code
(dsp:set! "poly-synth")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; now normal scheme code
;; to "play" the synth

(define midi2frq
  (lambda (pitch)
    (* 220.0 (expt 2.0 (/ (- pitch 48.0) 12.0)))))
        
;; make something looking like a traditional impromptu play-note
(define play-note
  (lambda (time pitch vol dur)
    (synth-note time (midi2frq pitch) 
		(* dur (* 44100.0 (/ 60 (*metro* 'get-tempo)))) 
		(/ vol 127.))))

;;
;; play some notes
;;
;; try making some changes
;; 
(define loop
  (lambda (beat)
    (for-each (lambda (p k)
		;; some detuning for fun :) (up to 20cents out)
		(play-note (*metro* (+ beat k)) (+ 12 p (* .2 (random))) 10 (* (- 3 k) .6))
		(play-note (*metro* (+ 1 beat k)) (+ 0 p) 40 (* (- 3 k) .75))
		(play-note (*metro* beat) (- p 12) 90 (* 3 .9)))
	      (random '((51 55 60)
			(53 56 72)			
			(50 55 59 65)))
	      '(0 2/3 3/2 1))
    (callback (*metro* (+ beat 2)) 'loop (+ beat 3))))

;; start playing notes
(loop (*metro* 'get-beat 4.0))

;; for randomly changing pulse width
(define loop-pulse-width
  (lambda (beat)
    (let ((v (* 20.0 (random))))
      (println 'set-pulse-width: v)
      (synth-set-pw v)
      (callback (*metro* (+ beat 16)) 'loop-pulse-width (+ beat 16)))))

(loop-pulse-width (*metro* 'get-beat 4.0))

;; for randomly changing pulse speed
(define loop-pulse-speed
  (lambda (beat)
    (let ((v (* 10.0 (random))))
      (println 'set-pulse-speed: v)
      (synth-set-pspeed v)
      (callback (*metro* (+ beat 12)) 'loop-pulse-speed (+ beat 12)))))

(loop-pulse-speed (*metro* 'get-beat 4.0))