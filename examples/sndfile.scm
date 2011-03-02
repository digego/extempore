;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Basic audiofile reading example
;;
;; This simple example uses libsndfile for
;; reading in an ogg file for sample playback
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; load highgui dynamic library
(define libsndfile (sys:open-dylib "libsndfile.so.1"))

;; bind 3 sndfile lib functions
(lib-bind libsndfile sf_open [i8*,i8*,i32,<i64,i32,i32,i32,i32,i32>*]*)
(lib-bind libsndfile sf_read_double [i64,i8*,double*,i64]*)
(lib-bind libsndfile sf_seek [i64,i8*,i64,i32]*)

;; an audio buffer reader
(definec read-audio-file
  (lambda (fname dat offset num)
    (printf "file name: %s\n" fname)
    (let ((info (make-tuple i64 i32 i32 i32 i32 i32))
	  (audiofile (sf_open fname 16 info))
	  (channels (i32toi64 (tref info 2)))
	  (num-to-read (* num channels))
	  (num-to-offset (* offset channels))
	  (cnt (sf_seek audiofile num-to-offset 0))       
	  (samples-read (sf_read_double audiofile dat num-to-read)))
      (printf "samplerate: %d\n" (tref info 1))
      (printf "channels: %d\n" (tref info 2))
      (printf "samples read: %lld\n" samples-read)
      dat)))

;; setup some space to hold audio data
(define audio-data (sys:make-cptr (* 44100 8 2 20)))
;; bind space for the compiler to access
(scm-bind adat double* audio-data)

;; dsp function
(definec:dsp dsp  
  (let ((sample 0.0)
	(audio (read-audio-file "/tmp/your_sound_file.ogg" adat (* 2 44100) (* 44100 10))))
    (lambda (in time chan dat)
      (let ((pos1 (modulo time (* 2.2 44100.0)))
	    (pos2 (modulo time (* 2.15 44100.0))))
	(+ (aref audio (dtoi64 (+ chan (* pos1 2.0))))
	   (* .4 (aref audio (dtoi64 (+ chan (* pos2 2.0))))))))))

;; set dsp
(dsp:set! dsp)