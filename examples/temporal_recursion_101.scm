;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Good old temporal recursion at it's simplest
;

(define	tr
  (lambda (t x rate)
    (print t x)
    (callback (+ t (/ rate .99)) 'tr (+ t rate) x rate)))

(let ((t (now)))
  tr t "a" (* 4 44100))
  (tr t " b" (* 2 44100))
  (tr t "  c" 44100))
