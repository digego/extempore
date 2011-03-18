;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This example shows how to use the builtin sampler
;;
;; You will first need to load and compile dsp_library.scm
;;
;; Then you're OK to go
;;
;; NOTE [at the moment compiling in a secondary thread is a little
;;      [flakey.  I'm working on this so in the mean time you'll
;;      [just have to put up with the audio delays while compiling
;;


;; first find a stereo audio file of some kind (not mp3 or aac)
;; ogg wav or aif should all be fine

;; then load up a few excerpts from the file
;; set-sampler-index takes
;; 1st: the instrument to load into (the default sampler is called 'sampler')
;; 2nd: the audio file to load from
;; 3rd: an index (from 0-127) to load into
;;      this should be the base frequency of the sample
;;      in other words a middle C sample should be loaded into 60.
;; 4th: an offset in samples (frames without channels)
;; 5th: a duration or length in samples to read

;; first let's just read in at one index
;; we'll choose middle C - 60
;; make sure your audio file is long enough for the params below!!
(set-sampler-index sampler "/tmp/audio.ogg" 60 500000 1000000)

;; playing back at 60 should playback without pitch shift
(play-note (now) sampler 60 80 100000)

;; anything else will pitch shift
;; floating point is OK
(play-note (now) sampler 67.25 80 100000)


;; a loop
(define loop
  (lambda (beat dur)
    (play sampler (random 48 72) 80 dur)
    (callback (*metro* (+ beat (* .5 dur))) 'loop
	      (+ beat dur)
	      dur)))

;; start loop
(loop (*metro* 'get-beat 4) 1)


;; now load in another subsample into a different index
;; make sure your audio file is long enough for the params below!!
(set-sampler-index sampler "/tmp/audio.ogg" 24 2100000 1000000)


;; then redefine loop using new sample load
;; old stuff keeps playing
;; notice that the sampler chooses the closest
;; available index to pitch shift to.
(define loop
  (lambda (beat dur)
    (play sampler 24 80 dur)    
    (play sampler (random 48 72) 80 dur)
    (callback (*metro* (+ beat (* .5 dur))) 'loop
	      (+ beat dur)
	      dur)))

;; etc..