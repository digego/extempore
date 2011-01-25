;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Simplest possible audio example
;

;; compile sample by sample dsp code
(definec dsp
  (lambda (in:double time:double channel:double	data:double*)
    (random)))

;; set compiled function named "dsp" to be the dsp callback
(dsp:set! "dsp")

;; recompile dsp to produce two sine wave in left and right
(definec dsp
  (lambda (in:double time:double channel:double	data:double*)
    (cond	((= channel 1.0)
           (* 0.3 (sin (* 3.14152 2.0 time (/ 440.0 44100.0)))))
          ((= channel 0.0)
           (* 0.3 (sin (* 3.14152 2.0 time (/ 660.0 44100.0)))))
          (else	0.0))))

;; abstract out an oscillator function                                          
(definec make-oscil
   (lambda (phase)
      (lambda (amp freq)
         (let ((inc (* 3.141592 (* 2.0 (/ freq 44100.)))))
            (set! phase (+ phase inc))
            (* amp (sin phase))))))

;; slightly more complex example
(definec dsp
  (let ((osc1 (make-oscil 0.))
        (osc1b (make-oscil 0.))
        (osc2 (make-oscil 0.))
        (osc2b (make-oscil 0.))
        (osc3 (make-oscil 0.))
        (osc3b (make-oscil 0.))
        (osc4 (make-oscil 0.))
        (osc4b (make-oscil 0.))
	(osc5 (make-oscil 0.)))
    (lambda (a:double b:double c:double d:double*)
      (cond ((= 0.0 c) ;; left channel                                          
             (+ (osc1 0.2 60.0)
                (osc2 0.2 220.0)
                (osc3 0.2 (+ 400. (osc5 200. .1)))
                (osc4 0.1 900.0)))
            ((=	1.0 c) ;; right channel                                         
             (* .1 (random)))
            (else 0.0))))) ;; any other channel                                 
