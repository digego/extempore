(define libsndfile (sys:open-dylib "libsndfile.so.1"))

(bind-lib libsndfile sf_open [i8*,i8*,i32,<i64,i32,i32,i32,i32,i32>*]*)
(bind-lib libsndfile sf_read_double [i64,i8*,double*,i64]*)
(bind-lib libsndfile sf_seek [i64,i8*,i64,i32]*)

(definec read-audio-file
  (lambda (fname dat offset num)
    (let ((info (make-tuple i64 i32 i32 i32 i32 i32))
          (audiofile (sf_open fname 16 info))
          (channels (i32toi64 (tref info 2)))
          (num-to-read (* num channels))
          (num-to-offset (* offset channels))
          (cnt (sf_seek audiofile num-to-offset 0))
          (samples-read (sf_read_double audiofile dat num-to-read)))
      dat)))

(define audio-data (sys:make-cptr (* 44100 8 2 20)))
(bind-scm adat double* audio-data)

(definec:dsp dsp
  (let ((sample 0.0)
        (lpf (make-lpf))
        (mod (make-oscil 0.0))
        (audio (read-audio-file "chant.wav" adat
                                (* 2 44100) (* 44100 10))))
    (lambda (in time chan dat)
      (lpf.res 10.0)
      (let ((pos (modulo time (* 8.0 44100.0))))
        (lpf (aref audio (dtoi64 (+ chan (* pos 2.0))))
             (+ 5000.0 (mod 3000.0 10.0)))))))