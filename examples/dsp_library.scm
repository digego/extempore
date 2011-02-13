;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Some basic dsp library functions
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
            (* amp (sin phase))))))


;; square oscillator
(definec make-square
   (lambda ()   
      (let ((osc (make-oscil 0.0))
            (n 50.0))
         (lambda (amp freq)
            (* amp (tanh (* n (osc 1.0 freq))))))))


;; pulse train
(definec make-pulse
   (lambda ()
      (let ((time -1.0))
         (lambda (freq width amp:double)
            (let ((period (/ *samplerate* freq)))
               (set! time (+ time 1.0))
               (if (< (modulo time period) width)
                   amp
                   0.0))))))


;; iir comb
(definec make-comb
   (lambda (delay)
      (let ((line (make-array delay double))
            (time 0))
         (lambda (x:double a b)
            (let* ((n (modulo time delay))
                   (delayed (aref line n))                   
                   (y (+ (* a x) (* b delayed))))
               (aset! line n y)
               (set! time (+ time 1))
               y)))))

;; tap delay
(definec tap-delay
   (lambda (delay)
      (let ((line (make-array delay double))
            (time 0))
         (lambda (x:double taps:i64* num-of-taps)
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
             (oldfreq 0.0))
         (lambda (x freq)
            ;; if frequency changes
            ;; recalculate coefficients
            (if (<> freq oldfreq)
                (let* ((omega (* TWOPI (/ freq *samplerate*)))
                       (q 0.5)
                       (sino (sin omega))
                       (coso (cos omega))
                       (alpha (/ sino (* 2.0 q))))
                   (set! oldfreq freq)
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
             (oldfreq 0.0))
         (lambda (x freq)
            ;; if frequency changes
            ;; recalculate coefficients
            (if (<> freq oldfreq)
                (let* ((omega (* TWOPI (/ freq *samplerate*)))
                       (q 0.5)
                       (sino (sin omega))
                       (coso (cos omega))
                       (alpha (/ sino (* 2.0 q))))
                   (set! oldfreq freq)
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
             (oldfreq 0.0)
             (oldbw 0.0))
         ;; bandwidth in octaves
         (lambda (x freq bandwidth)
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
             (oldfreq 0.0)
             (oldbw 0.0))
         ;; bandwidth in octaves
         (lambda (x freq bandwidth)
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