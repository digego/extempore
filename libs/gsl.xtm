;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; GNU Scientific Library
;;
;; This assumes 64bit sorry!
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define gsllib (if (string=? "Linux" (sys:platform))
		   (sys:open-dylib "libgsl.so.0")
		   (if (string=? "OSX" (sys:platform))
		       (sys:open-dylib "libgsl.0.dylib")
		       (print-error "tell me where to find the gsl dynamic library on your platform here!"))))


;; SOME TYPES FOR GSL
(bind-type gsl_complex <|2,double|>)
(bind-alias gsl_complex_packed double*)
(bind-alias gsl_complex_packed_array double*)
(bind-alias gsl_complex_packed_ptr double*)
(bind-alias gsl_const_complex_packed double*)
(bind-alias gsl_const_complex_packed_array double*)
(bind-alias gsl_const_complex_packed_ptr double*)
(bind-type gsl_sf_result <double,double>)
(bind-type gsl_sf_result_e10 <double,double,i32>)
(bind-type gsl_permutation <size_t,size_t*>)
(bind-type gsl_combination <size_t,size_t,size_t*>)
(bind-type gsl_block <size_t,double*>)
(bind-type gsl_block_complex <size_t,double*>)
(bind-type gsl_vector <size_t,size_t,double*,gsl_block*,i32>)
(bind-type gsl_vector_complex <size_t,size_t,double*,gsl_block_complex*,i32>)
(bind-type gsl_matrix <size_t,size_t,size_t,double*,gsl_block*,i32>)
(bind-type gsl_matrix_complex <size_t,size_t,size_t,double*,gsl_block_complex*,i32>)

(bind-type gsl_rng_type <i8*,i64,i64,size_t,[void,i8*,i64]*,[i64,i8*]*,[double,i8*]>)
(bind-type gsl_rng <gsl_rng_type*,i8*>)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  RANDOM NUMBERS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(bind-lib gsllib gsl_rng_alloc [gsl_rng*,gsl_rng_type*]*)
(bind-lib gsllib gsl_rng_set [void,gsl_rng*,i64]*) ;; i64 is the seed (default seed is 0)
(bind-lib gsllib gsl_rng_free [void,gsl_rng*]*)

(bind-lib gsllib gsl_rng_get [i64,gsl_rng*]*)
(bind-lib gsllib gsl_rng_uniform [double,gsl_rng*]*)
(bind-lib gsllib gsl_rng_uniform_pos [double,gsl_rng*]*)
(bind-lib gsllib gsl_rng_uniform_int [i64,gsl_rng*,i64]*)

(bind-lib gsllib gsl_rng_name [i8*,gsl_rng*]*)
(bind-lib gsllib gsl_rng_max [i64,gsl_rng*]*)
(bind-lib gsllib gsl_rng_min [i64,gsl_rng*]*)
(bind-lib gsllib gsl_rng_state [i8*,gsl_rng*]*)
(bind-lib gsllib gsl_rng_size [size_t,gsl_rng*]*)
(bind-lib gsllib gsl_rng_types_setup [gsl_rng_type**]*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make a stupidly simple default rng
;; gsl_rng state maintained by closure
(definec gsl_rng
  (let ((rngs (gsl_rng_types_setup))
	(rng (gsl_rng_alloc (pref rngs 0))))
    (lambda ()
      (gsl_rng_uniform rng))))

;; this will seed rng in gsl_rng closure above
(definec gsl_rng_seed
  (lambda (seed)
    (let ((rng (gsl_rng.rng:gsl_rng*)))
      (gsl_rng_set rng seed))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  COMPLEX NUMBERS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(bind-lib gsllib gsl_complex_rect [gsl_complex,double,double]*)
(bind-lib gsllib gsl_complex_polar [gsl_complex,double,double]*)

(definec GSL_REAL (lambda (z:gsl_complex) (aref (tref z 0) 0)))
(definec GSL_IMAG (lambda (z:gsl_complex) (aref (tref z 0) 1)))
(definec GSL_SET_COMPLEX
  (lambda (z:gsl_complex* x y)
    (aset! (tref-ptr z 0) 0 x)
    (aset! (tref-ptr z 0) 1 y)
    z))

(definec GSL_SET_REAL (lambda (z:gsl_complex* x) (aset! (tref-ptr z 0) 0 x) z))
(definec GSL_SET_IMAG (lambda (z:gsl_complex* y) (aset! (tref-ptr z 0) 1 y) z))
(bind-lib gsllib gsl_complex_arg [double,gsl_complex]*)
(bind-lib gsllib gsl_complex_abs [double,gsl_complex]*)
(bind-lib gsllib gsl_complex_logabs [double,gsl_complex]*)

;; complex ops
(bind-lib gsllib gsl_complex_add [gsl_complex,gsl_complex,gsl_complex]*)
(bind-lib gsllib gsl_complex_sub [gsl_complex,gsl_complex,gsl_complex]*)
(bind-lib gsllib gsl_complex_mul [gsl_complex,gsl_complex,gsl_complex]*)
(bind-lib gsllib gsl_complex_div [gsl_complex,gsl_complex,gsl_complex]*)
(bind-lib gsllib gsl_complex_add_real [gsl_complex,gsl_complex,double]*)
(bind-lib gsllib gsl_complex_sub_real [gsl_complex,gsl_complex,double]*)
(bind-lib gsllib gsl_complex_mul_real [gsl_complex,gsl_complex,double]*)
(bind-lib gsllib gsl_complex_div_real [gsl_complex,gsl_complex,double]*)
(bind-lib gsllib gsl_complex_add_imag [gsl_complex,gsl_complex,double]*)
(bind-lib gsllib gsl_complex_sub_imag [gsl_complex,gsl_complex,double]*)
(bind-lib gsllib gsl_complex_mul_imag [gsl_complex,gsl_complex,double]*)
(bind-lib gsllib gsl_complex_div_imag [gsl_complex,gsl_complex,double]*)
(bind-lib gsllib gsl_complex_conjugate [gsl_complex,gsl_complex]*)
(bind-lib gsllib gsl_complex_inverse [gsl_complex,gsl_complex]*)
(bind-lib gsllib gsl_complex_negative [gsl_complex,gsl_complex]*)

(bind-lib gsllib gsl_complex_sqrt [gsl_complex,gsl_complex]*)
(bind-lib gsllib gsl_complex_sqrt_real [gsl_complex,double]*)
(bind-lib gsllib gsl_complex_pow [gsl_complex,gsl_complex,gsl_complex]*)
(bind-lib gsllib gsl_complex_pow_real [gsl_complex,gsl_complex,double]*)
(bind-lib gsllib gsl_complex_exp [gsl_complex,gsl_complex]*)
(bind-lib gsllib gsl_complex_log [gsl_complex,gsl_complex]*)
(bind-lib gsllib gsl_complex_log10 [gsl_complex,gsl_complex]*)
(bind-lib gsllib gsl_complex_log_b [gsl_complex,gsl_complex,gsl_complex]*)

(bind-lib gsllib gsl_complex_sin [gsl_complex,gsl_complex]*)
(bind-lib gsllib gsl_complex_cos [gsl_complex,gsl_complex]*)
(bind-lib gsllib gsl_complex_tan [gsl_complex,gsl_complex]*)
(bind-lib gsllib gsl_complex_sec [gsl_complex,gsl_complex]*)
(bind-lib gsllib gsl_complex_csc [gsl_complex,gsl_complex]*)
(bind-lib gsllib gsl_complex_cot [gsl_complex,gsl_complex]*)

(bind-lib gsllib gsl_complex_arcsin [gsl_complex,gsl_complex]*)
(bind-lib gsllib gsl_complex_arccos [gsl_complex,gsl_complex]*)
(bind-lib gsllib gsl_complex_arctan [gsl_complex,gsl_complex]*)
(bind-lib gsllib gsl_complex_arcsec [gsl_complex,gsl_complex]*)
(bind-lib gsllib gsl_complex_arccsc [gsl_complex,gsl_complex]*)
(bind-lib gsllib gsl_complex_arccot [gsl_complex,gsl_complex]*)

(bind-lib gsllib gsl_complex_arcsin_real [gsl_complex,double]*)
(bind-lib gsllib gsl_complex_arccos_real [gsl_complex,double]*)
(bind-lib gsllib gsl_complex_arcsec_real [gsl_complex,double]*)
(bind-lib gsllib gsl_complex_arccsc_real [gsl_complex,double]*)

(bind-lib gsllib gsl_complex_sinh [gsl_complex,gsl_complex]*)
(bind-lib gsllib gsl_complex_cosh [gsl_complex,gsl_complex]*)
(bind-lib gsllib gsl_complex_tanh [gsl_complex,gsl_complex]*)
(bind-lib gsllib gsl_complex_sech [gsl_complex,gsl_complex]*)
(bind-lib gsllib gsl_complex_csch [gsl_complex,gsl_complex]*)
(bind-lib gsllib gsl_complex_coth [gsl_complex,gsl_complex]*)

(bind-lib gsllib gsl_complex_arcsinh [gsl_complex,gsl_complex]*)
(bind-lib gsllib gsl_complex_arccosh [gsl_complex,gsl_complex]*)
(bind-lib gsllib gsl_complex_arctanh [gsl_complex,gsl_complex]*)
(bind-lib gsllib gsl_complex_arcsech [gsl_complex,gsl_complex]*)
(bind-lib gsllib gsl_complex_arccsch [gsl_complex,gsl_complex]*)
(bind-lib gsllib gsl_complex_arccoth [gsl_complex,gsl_complex]*)

(bind-lib gsllib gsl_complex_arccosh_real [gsl_complex,double]*)
(bind-lib gsllib gsl_complex_arctanh_real [gsl_complex,double]*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; some simple helpers for complex numbers in extempore
;;
(definec Crect
  (lambda (real imag)
    (let ((num:gsl_complex* (alloc))
	  (n (tref-ptr num 0))) ;
      (aset! n 0 real)
      (aset! n 1 imag)
      num)))

(definec Cpolar
  (lambda (mag arg)
    (let ((num:gsl_complex* (alloc))
	  (n (tref-ptr num 0))
	  (real (* mag (cos arg)))
	  (imag (* mag (sin arg))))
      (aset! n 0 real)
      (aset! n 1 real)
      num)))

(definec Creal
  (lambda (num:gsl_complex*)
    (aref (tref num 0) 0)))

(definec Cimag
  (lambda (num:gsl_complex*)
    (aref (tref num 0) 1)))

(definec Cmag
  (lambda (num:gsl_complex*)
    (let ((real (aref (tref num 0) 0))
	  (imag (aref (tref num 0) 1)))
      (sqrt (+ (* real real) (* imag imag))))))

(definec Carg
  (lambda (num:gsl_complex*)
    (let ((real (aref (tref num 0) 0))
	  (imag (aref (tref num 0) 1)))
      (atan2 real imag))))

(definec Cadd
  (lambda (num1:gsl_complex* num2:gsl_complex*)
    (let ((n1 (bitcast num1 |2,double|*))
	  (n2 (bitcast num2 |2,double|*))
	  (num3:gsl_complex* (alloc))
	  (n3 (bitcast num3 |2,double|*)))
      (aset! n3 0 (+ (aref n1 0) (aref n2 0)))
      (aset! n3 1 (+ (aref n1 1) (aref n2 1)))
      num3)))

(definec CaddR
  (lambda (num1:gsl_complex* num2:double)
    (let ((n1 (bitcast num1 |2,double|*))
	  (num3:gsl_complex* (alloc))
	  (n3 (bitcast num3 |2,double|*)))
      (aset! n3 0 (+ (aref n1 0) num2))
      (aset! n3 1 (+ (aref n1 1) num2))
      num3)))

(definec Csub
  (lambda (num1:gsl_complex* num2:gsl_complex*)
    (let ((n1 (bitcast num1 |2,double|*))
	  (n2 (bitcast num2 |2,double|*))
	  (num3:gsl_complex* (alloc))
	  (n3 (bitcast num3 |2,double|*)))
      (aset! n3 0 (- (aref n1 0) (aref n2 0)))
      (aset! n3 1 (- (aref n1 1) (aref n2 1)))
      num3)))

(definec CsubR
  (lambda (num1:gsl_complex* num2:double)
    (let ((n1 (bitcast num1 |2,double|*))
	  (num3:gsl_complex* (alloc))
	  (n3 (bitcast num3 |2,double|*)))
      (aset! n3 0 (- (aref n1 0) num2))
      (aset! n3 1 (- (aref n1 1) num2))
      num3)))

(definec Cmul
  (lambda (num1:gsl_complex* num2:gsl_complex*)
    (let ((n1 (bitcast num1 |2,double|*))
	  (n2 (bitcast num2 |2,double|*))
	  (num3:gsl_complex* (alloc))
	  (n3 (bitcast num3 |2,double|*))
	  (ac (* (aref n1 0) (aref n2 0)))
	  (ad (* (aref n1 0) (aref n2 1)))
	  (bc (* (aref n1 1) (aref n2 0)))
	  (bd (* (aref n1 1) (aref n2 1))))
      (aset! n3 0 (- ac bd))
      (aset! n3 1 (+ ad bc))
      num3)))

(definec CmulR
  (lambda (num1:gsl_complex* num2:double)
    (let ((n1 (bitcast num1 |2,double|*))
	  (num3:gsl_complex* (alloc))
	  (n3 (bitcast num3 |2,double|*)))
      (aset! n3 0 (* (aref n3 0) num2))
      (aset! n3 1 (* (aref n3 1) num2))
      num3)))

(definec Cdiv
  (lambda (num1:gsl_complex* num2:gsl_complex*)
    (let ((num3:gsl_complex* (alloc))
          (res (tref (gsl_complex_div (pref num1 0) (pref num2 0)) 0)))
      (aset! (tref-ptr num3 0) 0 (aref res 0))
      (aset! (tref-ptr num3 0) 1 (aref res 1))
      num3)))

(definec Cconj
  (lambda (num1:gsl_complex*)
    (let ((num3:gsl_complex* (alloc))
	  (cres (gsl_complex_conjugate (pref num1 0)))
          (res (tref cres 0)))
      (aset! (tref-ptr num3 0) 0 (aref res 0))
      (aset! (tref-ptr num3 0) 1 (aref res 1))
      num3)))

(definec Cinverse
  (lambda (num1:gsl_complex*)
    (let ((num3:gsl_complex* (alloc))
	  (cres (gsl_complex_inverse (pref num1 0)))
          (res (tref cres 0)))
      (aset! (tref-ptr num3 0) 0 (aref res 0))
      (aset! (tref-ptr num3 0) 1 (aref res 1))
      num3)))

(definec Cnegative
  (lambda (num1:gsl_complex*)
    (let ((num3:gsl_complex* (alloc))
	  (cres (gsl_complex_negative (pref num1 0)))
          (res (tref cres 0)))
      (aset! (tref-ptr num3 0) 0 (aref res 0))
      (aset! (tref-ptr num3 0) 1 (aref res 1))
      num3)))

(definec Csqrt
  (lambda (num1:gsl_complex*)
    (let ((num3:gsl_complex* (alloc))
	  (cres (gsl_complex_sqrt (pref num1 0)))
          (res (tref cres 0)))
      (aset! (tref-ptr num3 0) 0 (aref res 0))
      (aset! (tref-ptr num3 0) 1 (aref res 1))
      num3)))

(definec Cpow
  (lambda (num1:gsl_complex* num2:gsl_complex*)
    (let ((num3:gsl_complex* (alloc))
	  (cres (gsl_complex_pow (pref num1 0) (pref num2 0)))
          (res (tref cres 0)))
      (aset! (tref-ptr num3 0) 0 (aref res 0))
      (aset! (tref-ptr num3 0) 1 (aref res 1))
      num3)))

(definec CpowR
  (lambda (num1:gsl_complex* num2:double)
    (let ((num3:gsl_complex* (alloc))
	  (cres (gsl_complex_pow_real (pref num1 0) num2))
          (res (tref cres 0)))
      (aset! (tref-ptr num3 0) 0 (aref res 0))
      (aset! (tref-ptr num3 0) 1 (aref res 1))
      num3)))

(definec Cexp
  (lambda (num1:gsl_complex*)
    (let ((num3:gsl_complex* (alloc))
	  (cres (gsl_complex_exp (pref num1 0)))
          (res (tref cres 0)))
      (aset! (tref-ptr num3 0) 0 (aref res 0))
      (aset! (tref-ptr num3 0) 1 (aref res 1))
      num3)))

(definec Clog
  (lambda (num1:gsl_complex*)
    (let ((num3:gsl_complex* (alloc))
	  (cres (gsl_complex_log (pref num1 0)))
          (res (tref cres 0)))
      (aset! (tref-ptr num3 0) 0 (aref res 0))
      (aset! (tref-ptr num3 0) 1 (aref res 1))
      num3)))

(definec Csin
  (lambda (num1:gsl_complex*)
    (let ((num3:gsl_complex* (alloc))
	  (cres (gsl_complex_sin (pref num1 0)))
          (res (tref cres 0)))
      (aset! (tref-ptr num3 0) 0 (aref res 0))
      (aset! (tref-ptr num3 0) 1 (aref res 1))
      num3)))

(definec Ccos
  (lambda (num1:gsl_complex*)
    (let ((num3:gsl_complex* (alloc))
	  (cres (gsl_complex_cos (pref num1 0)))
          (res (tref cres 0)))
      (aset! (tref-ptr num3 0) 0 (aref res 0))
      (aset! (tref-ptr num3 0) 1 (aref res 1))
      num3)))

(definec Ctan
  (lambda (num1:gsl_complex*)
    (let ((num3:gsl_complex* (alloc))
	  (cres (gsl_complex_tan (pref num1 0)))
          (res (tref cres 0)))
      (aset! (tref-ptr num3 0) 0 (aref res 0))
      (aset! (tref-ptr num3 0) 1 (aref res 1))
      num3)))

(definec Csec
  (lambda (num1:gsl_complex*)
    (let ((num3:gsl_complex* (alloc))
	  (cres (gsl_complex_sec (pref num1 0)))
          (res (tref cres 0)))
      (aset! (tref-ptr num3 0) 0 (aref res 0))
      (aset! (tref-ptr num3 0) 1 (aref res 1))
      num3)))

(definec Ccsc
  (lambda (num1:gsl_complex*)
    (let ((num3:gsl_complex* (alloc))
	  (cres (gsl_complex_csc (pref num1 0)))
          (res (tref cres 0)))
      (aset! (tref-ptr num3 0) 0 (aref res 0))
      (aset! (tref-ptr num3 0) 1 (aref res 1))
      num3)))

(definec Ccot
  (lambda (num1:gsl_complex*)
    (let ((num3:gsl_complex* (alloc))
	  (cres (gsl_complex_cot (pref num1 0)))
          (res (tref cres 0)))
      (aset! (tref-ptr num3 0) 0 (aref res 0))
      (aset! (tref-ptr num3 0) 1 (aref res 1))
      num3)))

(definec Casin
  (lambda (num1:gsl_complex*)
    (let ((num3:gsl_complex* (alloc))
	  (cres (gsl_complex_arcsin (pref num1 0)))
          (res (tref cres 0)))
      (aset! (tref-ptr num3 0) 0 (aref res 0))
      (aset! (tref-ptr num3 0) 1 (aref res 1))
      num3)))

(definec Cacos
  (lambda (num1:gsl_complex*)
    (let ((num3:gsl_complex* (alloc))
	  (cres (gsl_complex_arccos (pref num1 0)))
          (res (tref cres 0)))
      (aset! (tref-ptr num3 0) 0 (aref res 0))
      (aset! (tref-ptr num3 0) 1 (aref res 1))
      num3)))

(definec Catan
  (lambda (num1:gsl_complex*)
    (let ((num3:gsl_complex* (alloc))
	  (cres (gsl_complex_arctan (pref num1 0)))
          (res (tref cres 0)))
      (aset! (tref-ptr num3 0) 0 (aref res 0))
      (aset! (tref-ptr num3 0) 1 (aref res 1))
      num3)))

(definec Casec
  (lambda (num1:gsl_complex*)
    (let ((num3:gsl_complex* (alloc))
	  (cres (gsl_complex_arcsec (pref num1 0)))
          (res (tref cres 0)))
      (aset! (tref-ptr num3 0) 0 (aref res 0))
      (aset! (tref-ptr num3 0) 1 (aref res 1))
      num3)))

(definec Cacsc
  (lambda (num1:gsl_complex*)
    (let ((num3:gsl_complex* (alloc))
	  (cres (gsl_complex_arccsc (pref num1 0)))
          (res (tref cres 0)))
      (aset! (tref-ptr num3 0) 0 (aref res 0))
      (aset! (tref-ptr num3 0) 1 (aref res 1))
      num3)))

(definec Cacot
  (lambda (num1:gsl_complex*)
    (let ((num3:gsl_complex* (alloc))
	  (cres (gsl_complex_arccot (pref num1 0)))
          (res (tref cres 0)))
      (aset! (tref-ptr num3 0) 0 (aref res 0))
      (aset! (tref-ptr num3 0) 1 (aref res 1))
      num3)))

(definec Csinh
  (lambda (num1:gsl_complex*)
    (let ((num3:gsl_complex* (alloc))
	  (cres (gsl_complex_sinh (pref num1 0)))
          (res (tref cres 0)))
      (aset! (tref-ptr num3 0) 0 (aref res 0))
      (aset! (tref-ptr num3 0) 1 (aref res 1))
      num3)))

(definec Ccosh
  (lambda (num1:gsl_complex*)
    (let ((num3:gsl_complex* (alloc))
	  (cres (gsl_complex_cosh (pref num1 0)))
          (res (tref cres 0)))
      (aset! (tref-ptr num3 0) 0 (aref res 0))
      (aset! (tref-ptr num3 0) 1 (aref res 1))
      num3)))

(definec Ctanh
  (lambda (num1:gsl_complex*)
    (let ((num3:gsl_complex* (alloc))
	  (cres (gsl_complex_tanh (pref num1 0)))
          (res (tref cres 0)))
      (aset! (tref-ptr num3 0) 0 (aref res 0))
      (aset! (tref-ptr num3 0) 1 (aref res 1))
      num3)))

(definec Csech
  (lambda (num1:gsl_complex*)
    (let ((num3:gsl_complex* (alloc))
	  (cres (gsl_complex_sech (pref num1 0)))
          (res (tref cres 0)))
      (aset! (tref-ptr num3 0) 0 (aref res 0))
      (aset! (tref-ptr num3 0) 1 (aref res 1))
      num3)))

(definec Ccsch
  (lambda (num1:gsl_complex*)
    (let ((num3:gsl_complex* (alloc))
	  (cres (gsl_complex_csch (pref num1 0)))
          (res (tref cres 0)))
      (aset! (tref-ptr num3 0) 0 (aref res 0))
      (aset! (tref-ptr num3 0) 1 (aref res 1))
      num3)))

(definec Ccoth
  (lambda (num1:gsl_complex*)
    (let ((num3:gsl_complex* (alloc))
	  (cres (gsl_complex_coth (pref num1 0)))
          (res (tref cres 0)))
      (aset! (tref-ptr num3 0) 0 (aref res 0))
      (aset! (tref-ptr num3 0) 1 (aref res 1))
      num3)))

(definec Casinh
  (lambda (num1:gsl_complex*)
    (let ((num3:gsl_complex* (alloc))
	  (cres (gsl_complex_arcsinh (pref num1 0)))
          (res (tref cres 0)))
      (aset! (tref-ptr num3 0) 0 (aref res 0))
      (aset! (tref-ptr num3 0) 1 (aref res 1))
      num3)))

(definec Cacosh
  (lambda (num1:gsl_complex*)
    (let ((num3:gsl_complex* (alloc))
	  (cres (gsl_complex_arccosh (pref num1 0)))
          (res (tref cres 0)))
      (aset! (tref-ptr num3 0) 0 (aref res 0))
      (aset! (tref-ptr num3 0) 1 (aref res 1))
      num3)))

(definec Catanh
  (lambda (num1:gsl_complex*)
    (let ((num3:gsl_complex* (alloc))
	  (cres (gsl_complex_arctanh (pref num1 0)))
          (res (tref cres 0)))
      (aset! (tref-ptr num3 0) 0 (aref res 0))
      (aset! (tref-ptr num3 0) 1 (aref res 1))
      num3)))

(definec Casech
  (lambda (num1:gsl_complex*)
    (let ((num3:gsl_complex* (alloc))
	  (cres (gsl_complex_arcsech (pref num1 0)))
          (res (tref cres 0)))
      (aset! (tref-ptr num3 0) 0 (aref res 0))
      (aset! (tref-ptr num3 0) 1 (aref res 1))
      num3)))

(definec Cacsch
  (lambda (num1:gsl_complex*)
    (let ((num3:gsl_complex* (alloc))
	  (cres (gsl_complex_arccsch (pref num1 0)))
          (res (tref cres 0)))
      (aset! (tref-ptr num3 0) 0 (aref res 0))
      (aset! (tref-ptr num3 0) 1 (aref res 1))
      num3)))

(definec Cacoth
  (lambda (num1:gsl_complex*)
    (let ((num3:gsl_complex* (alloc))
	  (cres (gsl_complex_arccoth (pref num1 0)))
          (res (tref cres 0)))
      (aset! (tref-ptr num3 0) 0 (aref res 0))
      (aset! (tref-ptr num3 0) 1 (aref res 1))
      num3)))

;; print rectangular
(definec CprintR
  (lambda (num:gsl_complex*)
    (if (< (Cimag num) 0.0)
	(printf "(%f - i%f)\n" (Creal num) (fabs (Cimag num)))
	(printf "(%f + i%f)\n" (Creal num) (Cimag num)))))

;; print polar (radians)
(definec CprintP
  (lambda (num:gsl_complex*)
    (printf "(%f cis %f)\n" (Cmag num) (Carg num))))

;; print polar (radians) ... same as above
(definec CprintPr
  (lambda (num:gsl_complex*)
    (printf "(%f cis %f)\n" (Cmag num) (Carg num))))

;; print polar (degrees)
(definec CprintPd
  (lambda (num:gsl_complex*)
    (printf "(%f cis %f)\n" (Cmag num) (* 57.295791 (Carg num)))))
   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  POLYNOMIALS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(bind-lib gsllib gsl_poly_eval [double,double*,i32,double]*)
(bind-lib gsllib gsl_poly_complex_eval [gsl_complex,double*,i32,gsl_complex]*)
(bind-lib gsllib gsl_complex_poly_complex_eval [gsl_complex*,i32,gsl_complex]*)
(bind-lib gsllib gsl_poly_eval_derivs [i32,double*,size_t,double,double*,size_t]*)

(bind-lib gsllib gsl_poly_dd_init [i32,double*,double*,double*,size_t]*)
(bind-lib gsllib gsl_poly_dd_eval [double,double*,double*,size_t,double]*)
(bind-lib gsllib gsl_poly_dd_taylor [i32,double*,double,double*,double*,size_t,double*]*)

(bind-lib gsllib gsl_poly_solve_quadratic [i32,double,double,double,double*,double*]*)
(bind-lib gsllib gsl_poly_complex_solve_quadratic [i32,double,double,double,gsl_complex*,gsl_complex*]*)
(bind-lib gsllib gsl_poly_solve_cubic [i32,double,double,double,double*,double*,double*]*)
(bind-lib gsllib gsl_poly_complex_solve_cubic [i32,double,double,double,gsl_complex*,gsl_complex*,gsl_complex*]*)

(bind-alias gsl_poly_complex_workspace i8)
(bind-lib gsllib gsl_poly_complex_workspace_alloc [gsl_poly_complex_workspace*,size_t]*)
(bind-lib gsllib gsl_poly_complex_workspace_free [void,gsl_poly_complex_workspace*]*)
(bind-lib gsllib gsl_poly_complex_solve [i32,double*,size_t,gsl_poly_complex_workspace*,gsl_complex_packed_ptr]*)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Special Functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; trig functions
(bind-lib gsllib gsl_sf_sin [double,double]*)
(bind-lib gsllib gsl_sf_cos [double,double]*)
(bind-lib gsllib gsl_sf_hypot [double,double]*)
(bind-lib gsllib gsl_sf_sinc [double,double]*)

(bind-lib gsllib gsl_sf_complex_sin_e [i32,double,double,gsl_sf_result*,gsl_sf_result*]*)
(bind-lib gsllib gsl_sf_complex_cos_e [i32,double,double,gsl_sf_result*,gsl_sf_result*]*)
(bind-lib gsllib gsl_sf_complex_logsin_e [i32,double,double,gsl_sf_result*,gsl_sf_result*]*)

(bind-lib gsllib gsl_sf_lnsinh [double,double]*)
(bind-lib gsllib gsl_sf_lncosh [double,double]*)

(bind-lib gsllib gsl_sf_polar_to_rect [double,double,gsl_sf_result*,gsl_sf_result*]*)
(bind-lib gsllib gsl_sf_rect_to_polar [double,double,gsl_sf_result*,gsl_sf_result*]*)

(bind-lib gsllib gsl_sf_angle_restrict_symm [double,double]*)
(bind-lib gsllib gsl_sf_angle_restrict_pos [double,double]*)

;; zeta functions
(bind-lib gsllib gsl_sf_zeta_int [double,i32]*)
(bind-lib gsllib gsl_sf_zeta [double,double]*)
(bind-lib gsllib gsl_sf_zetam1_int [double,i32]*)
(bind-lib gsllib gsl_sf_zetam1 [double,double]*)
(bind-lib gsllib gsl_sf_hzeta [double,double,double]*)
(bind-lib gsllib gsl_sf_eta_int [double,i32]*)
(bind-lib gsllib gsl_sf_eta [double,double]*)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Permutations and Combintations
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(bind-lib gsllib gsl_permutation_alloc [gsl_permutation*,size_t]*)
(bind-lib gsllib gsl_permutation_calloc [gsl_permutation*,size_t]*)
(bind-lib gsllib gsl_permutation_init [void,gsl_permutation*]*)
(bind-lib gsllib gsl_permutation_free [void,gsl_permutation*]*)
(bind-lib gsllib gsl_permutation_memcpy [i32,gsl_permutation*,gsl_permutation*]*)

(bind-lib gsllib gsl_permutation_get [size_t,gsl_permutation*,size_t]*)
(bind-lib gsllib gsl_permutation_swap [i32,gsl_permutation*,size_t,size_t]*)

(bind-lib gsllib gsl_permutation_size [size_t,gsl_permutation*]*)
(bind-lib gsllib gsl_permutation_data [size_t,gsl_permutation*]*)
(bind-lib gsllib gsl_permutation_valid [i32,gsl_permutation*]*)

(bind-lib gsllib gsl_permutation_reverse [void,gsl_permutation*]*)
(bind-lib gsllib gsl_permutation_inverse [i32,gsl_permutation*,gsl_permutation*]*)
(bind-lib gsllib gsl_permutation_next [i32,gsl_permutation*]*)
(bind-lib gsllib gsl_permutation_prev [i32,gsl_permutation*]*)

(bind-lib gsllib gsl_permute [i32,size_t*,double*,size_t,size_t]*)
(bind-lib gsllib gsl_permute_inverse [i32,size_t*,double*,size_t,size_t]*)
(bind-lib gsllib gsl_permute_vector [i32,gsl_permutation*,gsl_vector*]*)
(bind-lib gsllib gsl_permute_vector_inverse [i32,gsl_permutation*,gsl_vector*]*)
(bind-lib gsllib gsl_permutation_mul [i32,gsl_permutation*,gsl_permutation*,gsl_permutation*]*)

(bind-lib gsllib gsl_permutation_fwrite [i32,i8*,gsl_permutation*]*)
(bind-lib gsllib gsl_permutation_fread [i32,i8*,gsl_permutation*]*)

(bind-lib gsllib gsl_permutation_linear_to_canonical [i32,gsl_permutation*,gsl_permutation*]*)
(bind-lib gsllib gsl_permutation_canonical_to_linear [i32,gsl_permutation*,gsl_permutation*]*)
(bind-lib gsllib gsl_permutation_inversions [size_t,gsl_permutation*]*)
(bind-lib gsllib gsl_permutation_linear_cycles [size_t,gsl_permutation*]*)
(bind-lib gsllib gsl_permutation_canonical_cycles [size_t,gsl_permutation*]*)

(bind-lib gsllib gsl_combination_alloc [gsl_combination*,size_t,size_t]*)
(bind-lib gsllib gsl_combination_calloc [gsl_combination*,size_t,size_t]*)
(bind-lib gsllib gsl_combination_init_first [void,gsl_combination*]*)
(bind-lib gsllib gsl_combination_init_last [void,gsl_combination*]*)
(bind-lib gsllib gsl_combination_free [void,gsl_combination*]*)
(bind-lib gsllib gsl_combination_memcpy [i32,gsl_combination*,gsl_combination*]*)
(bind-lib gsllib gsl_combination_get [size_t,gsl_combination*,size_t]*)

(bind-lib gsllib gsl_combination_n [size_t,gsl_combination*]*)
(bind-lib gsllib gsl_combination_k [size_t,gsl_combination*]*)
(bind-lib gsllib gsl_combination_data [size_t*,gsl_combination*]*)
(bind-lib gsllib gsl_combination_valid [i32,gsl_combination*]*)

(bind-lib gsllib gsl_combination_next [i32,gsl_combination*]*)
(bind-lib gsllib gsl_combination_prev [i32,gsl_combination*]*)

(bind-lib gsllib gsl_combination_fwrite [i32,i8*,gsl_combination*]*)
(bind-lib gsllib gsl_combination_fread [i32,i8*,gsl_combination*]*)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  MATRIX AND VECTOR STUFF
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; blocks

(bind-lib gsllib gsl_block_alloc [gsl_block*,size_t]*)
(bind-lib gsllib gsl_block_calloc [gsl_block*,size_t]*)
(bind-lib gsllib gsl_block_free [void,gsl_block*]*)
;; where i8* is FILE*
(bind-lib gsllib gsl_block_fwrite [i32,i8*,gsl_block*]*)
(bind-lib gsllib gsl_block_fread [i32,i8*,gsl_block*]*)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; vectors

;; size,stride,data,block,owner
(bind-lib gsllib gsl_vector_alloc [gsl_vector*,size_t]*)
(bind-lib gsllib gsl_vector_calloc [gsl_vector*,size_t]*)
(bind-lib gsllib gsl_vector_free [void,gsl_vector*]*)
(bind-lib gsllib gsl_vector_get [double,gsl_vector*,size_t]*)
(bind-lib gsllib gsl_vector_set [void,gsl_vector*,size_t,double]*)
(bind-lib gsllib gsl_vector_ptr [double*,gsl_vector*,size_t]*)
(bind-lib gsllib gsl_vector_set_all [void,gsl_vector*,double]*)
(bind-lib gsllib gsl_vector_set_zero [void,gsl_vector*]*)
(bind-lib gsllib gsl_vector_set_basis [i32,gsl_vector*,size_t]*)
;; where i8* is FILE*
(bind-lib gsllib gsl_vector_fwrite [i32,i8*,gsl_vector*]*)
(bind-lib gsllib gsl_vector_fread [i32,i8*,gsl_vector*]*)

(bind-lib gsllib gsl_vector_memcpy [i32,gsl_vector*,gsl_vector*]*)
(bind-lib gsllib gsl_vector_swap [i32,gsl_vector*,gsl_vector*]*)
(bind-lib gsllib gsl_vector_swap_elements [i32,gsl_vector*,size_t,size_t]*)
(bind-lib gsllib gsl_vector_reverse [i32,gsl_vector*]*)

;; result ends up in first vector (2nd vector unchanged)
(bind-lib gsllib gsl_vector_add [i32,gsl_vector*,gsl_vector*]*)
(bind-lib gsllib gsl_vector_sub [i32,gsl_vector*,gsl_vector*]*)
(bind-lib gsllib gsl_vector_mul [i32,gsl_vector*,gsl_vector*]*)
(bind-lib gsllib gsl_vector_div [i32,gsl_vector*,gsl_vector*]*)
(bind-lib gsllib gsl_vector_scale [i32,gsl_vector*,double]*)
(bind-lib gsllib gsl_vector_add_constant [i32,gsl_vector*,double]*)
(bind-lib gsllib gsl_vector_max [double,gsl_vector*]*)
(bind-lib gsllib gsl_vector_min [double,gsl_vector*]*)
(bind-lib gsllib gsl_vector_max_index [size_t,gsl_vector*]*)
(bind-lib gsllib gsl_vector_min_index [size_t,gsl_vector*]*)

(bind-lib gsllib gsl_vector_isnull [i32,gsl_vector*]*)
(bind-lib gsllib gsl_vector_ispos [i32,gsl_vector*]*)
(bind-lib gsllib gsl_vector_isneg [i32,gsl_vector*]*)
(bind-lib gsllib gsl_vector_isnonneg [i32,gsl_vector*]*)
;; (bind-lib gsllib gsl_vector_equal [i32,gsl_vector*,gsl_vector*]*)

;; VECTOR SORTING
(bind-lib gsllib gsl_sort [void,double*,size_t,size_t]*)
(bind-lib gsllib gsl_sort_vector [void,gsl_vector*]*)
(bind-lib gsllib gsl_sort_index [void,size_t*,double*,size_t,size_t]*)
(bind-lib gsllib gsl_sort_vector_index [i32,gsl_permutation*,gsl_vector*]*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Helper functions for Extmepore vectors
;;

(definec Valloc
  (lambda (size:size_t)
    (gsl_vector_alloc size)))

(definec Vfree
  (lambda (v)
    (gsl_vector_free v)))

(definec Vref
  (lambda (v idx)
    (gsl_vector_get v idx)))

(definec Vset
  (lambda (v idx val)
    (gsl_vector_set v idx val)))

(definec Vcopy
  (lambda (v)
    (let ((v2 (Valloc (tref v 0))))
      (gsl_vector_memcpy v2 v)
      v2)))

(definec Vadd
  (lambda (v1 v2)
    (let ((v3 (Vcopy v1)))
      (gsl_vector_add v3 v2)
      v3)))

(definec Vsub
  (lambda (v1 v2)
    (let ((v3 (Vcopy v1)))
      (gsl_vector_sub v3 v2)
      v3)))

(definec Vmul
  (lambda (v1 v2)
    (let ((v3 (Vcopy v1)))
      (gsl_vector_mul v3 v2)
      v3)))

(definec Vdiv
  (lambda (v1 v2)
    (let ((v3 (Vcopy v1)))
      (gsl_vector_div v3 v2)
      v3)))

(definec Vscale
  (lambda (v1 s)
    (let ((v3 (Vcopy v1)))
      (gsl_vector_scale v3 s)
      v3)))

(definec Vsetall
  (lambda (v1 s)    
    (gsl_vector_set_all v1 s)
    v1))

(definec Vdot
  (lambda (v1 v2)
    (let ((size:size_t (tref v1 0))
	  (i:size_t 0)
	  (tot 0.0)
	  (v3 (Vmul v1 v2)))
      (dotimes (i size)	
	(set! tot (+ tot (gsl_vector_get v3 i))))
      tot)))

(definec VtoPtr
  (lambda (v1)
    (let ((size:size_t (tref v1 0))
	  (ptr:double* (zalloc (* 8 size)))
	  (i 0))
      (dotimes (i size)
	(pset! ptr i (gsl_vector_get v1 i)))
      ptr)))

;; make a vector from an array of doubles
;; number of doubles is elements
(definec Vmk
  (lambda (ptr:double* elements:size_t)
    (let ((v1 (Valloc elements))
	  (i:size_t 0))
      (dotimes (i elements)
	(gsl_vector_set v1 i (pref ptr i)))
      v1)))    

;; for 3d only
(definec Vcross
  (lambda (v1 v2)
    (let ((size:size_t (tref v1 0))
	  (v3 (Valloc 3)))
      (gsl_vector_set_zero v3)
      (if (<> size 3)
	  (begin (printf "Vcross for 3d vectors only!\n")
		 v3)
	  (begin (gsl_vector_set v3 0 (- (* (gsl_vector_get v1 1)
					    (gsl_vector_get v2 2))
					 (* (gsl_vector_get v1 2)
					    (gsl_vector_get v2 1))))
		 (gsl_vector_set v3 1 (- (* (gsl_vector_get v1 2)
					    (gsl_vector_get v2 0))
					 (* (gsl_vector_get v1 0)
					    (gsl_vector_get v2 2))))
		 (gsl_vector_set v3 2 (- (* (gsl_vector_get v1 0)
					    (gsl_vector_get v2 1))
					 (* (gsl_vector_get v1 1)
					    (gsl_vector_get v2 0))))
		 v3)))))


(definec Vfill
  (lambda (v1 data:double*)
    (let ((size:size_t (tref v1 0))
	  (i:size_t 0))
      (dotimes (i size)
	(gsl_vector_set v1 i (pref data i)))
      v1)))

(definec Vprint
  (lambda (v1)    
    (let ((size:size_t (tref v1 0))
	  (s (salloc 1024 i8))
	  (loc:i32 0)
	  (i:size_t 0))
      (sprintf s "[ ")
      (set! loc (i64toi32 2))
      (dotimes (i size)
	(set! loc (+ loc (sprintf (pref-ptr s loc) "%.5f " (gsl_vector_get v1 i)))))
      (sprintf (pref-ptr s loc) "]\n")
      (printf "%s" s)
      void)))

(definec Vmk3
  (lambda (a b c)
    (let ((v1 (Valloc 3)))
      (gsl_vector_set v1 0 a)
      (gsl_vector_set v1 1 b)
      (gsl_vector_set v1 2 c)
      v1)))

(definec Vmk4
  (lambda (a b c d)
    (let ((v1 (Valloc 4)))
      (gsl_vector_set v1 0 a)
      (gsl_vector_set v1 1 b)
      (gsl_vector_set v1 2 c)
      (gsl_vector_set v1 3 d)      
      v1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; matrices
;;
;; rows first, columns second.
;;


(bind-lib gsllib gsl_matrix_alloc [gsl_matrix*,size_t,size_t]*)
(bind-lib gsllib gsl_matrix_calloc [gsl_matrix*,size_t,size_t]*)
(bind-lib gsllib gsl_matrix_free [void,gsl_matrix*]*)
(bind-lib gsllib gsl_matrix_get [double,gsl_matrix*,size_t,size_t]*)
(bind-lib gsllib gsl_matrix_set [void,gsl_matrix*,size_t,size_t,double]*)
(bind-lib gsllib gsl_matrix_ptr [double*,gsl_matrix*,size_t,size_t]*)
(bind-lib gsllib gsl_matrix_set_all [void,gsl_matrix*,double]*)
(bind-lib gsllib gsl_matrix_set_zero [void,gsl_matrix*]*)
(bind-lib gsllib gsl_matrix_set_identity [void,gsl_matrix*]*)
;; where i8* is FILE*
(bind-lib gsllib gsl_matrix_fwrite [i32,i8*,gsl_matrix*]*)
(bind-lib gsllib gsl_matrix_fread [i32,i8*,gsl_matrix*]*)

;; dest first src second
(bind-lib gsllib gsl_matrix_memcpy [i32,gsl_matrix*,gsl_matrix*]*)
;; swap elements of a and b (by copy)
(bind-lib gsllib gsl_matrix_swap [i32,gsl_matrix*,gsl_matrix*]*)

;; get set by row/col
(bind-lib gsllib gsl_matrix_get_row [i32,gsl_vector*,gsl_matrix*,size_t]*)
(bind-lib gsllib gsl_matrix_get_col [i32,gsl_vector*,gsl_matrix*,size_t]*)
(bind-lib gsllib gsl_matrix_set_row [i32,gsl_matrix*,size_t,gsl_vector*]*)
(bind-lib gsllib gsl_matrix_set_col [i32,gsl_matrix*,size_t,gsl_vector*]*)

;; exchanging rows and cols
(bind-lib gsllib gsl_matrix_swap_rows [i32,gsl_matrix*,size_t,size_t]*)
(bind-lib gsllib gsl_matrix_swap_columns [i32,gsl_matrix*,size_t,size_t]*)
(bind-lib gsllib gsl_matrix_swap_rowcol [i32,gsl_matrix*,size_t,size_t]*)
(bind-lib gsllib gsl_matrix_transpose_memcpy [i32,gsl_matrix*,gsl_matrix*]*)
(bind-lib gsllib gsl_matrix_transpose [i32,gsl_matrix*]*)

;; matrix ops
(bind-lib gsllib gsl_matrix_add [i32,gsl_matrix*,gsl_matrix*]*)
(bind-lib gsllib gsl_matrix_sub [i32,gsl_matrix*,gsl_matrix*]*)
(bind-lib gsllib gsl_matrix_mul_elements [i32,gsl_matrix*,gsl_matrix*]*)
(bind-lib gsllib gsl_matrix_div_elements [i32,gsl_matrix*,gsl_matrix*]*)
(bind-lib gsllib gsl_matrix_scale [i32,gsl_matrix*,double]*)
(bind-lib gsllib gsl_matrix_add_constant [i32,gsl_matrix*,double]*)
(bind-lib gsllib gsl_matrix_max [double,gsl_matrix*]*)
(bind-lib gsllib gsl_matrix_min [double,gsl_matrix*]*)
(bind-lib gsllib gsl_matrix_max_index [void,gsl_matrix*,size_t*,size_t*]*)
(bind-lib gsllib gsl_matrix_min_index [void,gsl_matrix*,size_t*,size_t*]*)

(bind-lib gsllib gsl_matrix_isnull [i32,gsl_matrix*]*)
(bind-lib gsllib gsl_matrix_ispos [i32,gsl_matrix*]*)
(bind-lib gsllib gsl_matrix_isneg [i32,gsl_matrix*]*)
(bind-lib gsllib gsl_matrix_isnonneg [i32,gsl_matrix*]*)
;(bind-lib gsllib gsl_matrix_equal [i32,gsl_matrix*,gsl_matrix*]*)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Linear Algebra
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(bind-lib gsllib gsl_linalg_LU_decomp [i32,gsl_matrix*,gsl_permutation*,i32*]*)
(bind-lib gsllib gsl_linalg_complex_LU_decomp [i32,gsl_matrix_complex*,gsl_permutation*,i32*]*)
(bind-lib gsllib gsl_linalg_LU_solve [i32,gsl_matrix*,gsl_permutation*,gsl_vector*,gsl_vector*]*)
(bind-lib gsllib gsl_linalg_complex_LU_solve [i32,gsl_matrix_complex*,gsl_permutation*,gsl_vector_complex*,gsl_vector_complex*]*)
(bind-lib gsllib gsl_linalg_LU_svx [i32,gsl_matrix*,gsl_permutation*,gsl_vector*]*)
(bind-lib gsllib gsl_linalg_complex_LU_svx [i32,gsl_matrix_complex*,gsl_permutation*,gsl_vector_complex*]*)
(bind-lib gsllib gsl_linalg_LU_refine [i32,gsl_matrix*,gsl_matrix*,gsl_permutation*,gsl_vector*,gsl_vector*,gsl_vector*]*)
(bind-lib gsllib gsl_linalg_complex_LU_refine [i32,gsl_matrix_complex*,gsl_matrix_complex*,gsl_permutation*,gsl_vector_complex*,gsl_vector_complex*,gsl_vector_complex*]*)
(bind-lib gsllib gsl_linalg_LU_invert [i32,gsl_matrix*,gsl_permutation*,gsl_matrix*]*)
(bind-lib gsllib gsl_linalg_complex_LU_invert [i32,gsl_matrix_complex*,gsl_permutation*,gsl_matrix_complex*]*)
(bind-lib gsllib gsl_linalg_LU_det [double,gsl_matrix*,i32]*)
(bind-lib gsllib gsl_linalg_complex_LU_det [double,gsl_matrix_complex*,i32]*)
(bind-lib gsllib gsl_linalg_LU_lndet [double,gsl_matrix*]*)
(bind-lib gsllib gsl_linalg_complex_LU_lndet [double,gsl_matrix_complex*]*)
(bind-lib gsllib gsl_linalg_LU_sgndet [double,gsl_matrix*,i32]*)
(bind-lib gsllib gsl_linalg_complex_LU_sgndet [double,gsl_matrix_complex*,i32]*)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Statistics
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(bind-lib gsllib gsl_stats_mean [double,double*,size_t,size_t]*)
(bind-lib gsllib gsl_stats_variance [double,double*,size_t,size_t]*)
(bind-lib gsllib gsl_stats_variance_m [double,double*,size_t,size_t,double]*)
(bind-lib gsllib gsl_stats_sd [double,double*,size_t,size_t]*)
(bind-lib gsllib gsl_stats_sd_m [double,double*,size_t,size_t,double]*)
(bind-lib gsllib gsl_stats_tss [double,double*,size_t,size_t]*)
(bind-lib gsllib gsl_stats_tss_m [double,double*,size_t,size_t,double]*)
(bind-lib gsllib gsl_stats_variance_with_fixed_mean [double,double*,size_t,size_t,double]*)
(bind-lib gsllib gsl_stats_sd_with_fixed_mean [double,double*,size_t,size_t,double]*)
(bind-lib gsllib gsl_stats_absdev [double,double*,size_t,size_t]*)
(bind-lib gsllib gsl_stats_absdev_m [double,double*,size_t,size_t,double]*)
(bind-lib gsllib gsl_stats_skew [double,double*,size_t,size_t]*)
(bind-lib gsllib gsl_stats_skew_m_sd [double,double*,size_t,size_t,double,double]*)
(bind-lib gsllib gsl_stats_kurtosis [double,double*,size_t,size_t]*)
(bind-lib gsllib gsl_stats_kurtosis_m_sd [double,double*,size_t,size_t,double,double]*)

(bind-lib gsllib gsl_stats_lag1_autocorrelation [double,double*,size_t,size_t]*)
(bind-lib gsllib gsl_stats_lag1_autocorrelation_m [double,double*,size_t,size_t,double]*)
(bind-lib gsllib gsl_stats_covariance [double,double*,size_t,double*,size_t,size_t]*)
(bind-lib gsllib gsl_stats_covariance_m [double,double*,size_t,double*,size_t,size_t,double,double]*)
(bind-lib gsllib gsl_stats_correlation [double,double*,size_t,double*,size_t,size_t]*)

(bind-lib gsllib gsl_stats_wmean [double,double*,size_t,double*,size_t,size_t]*)
(bind-lib gsllib gsl_stats_wvariance [double,double*,size_t,double*,size_t,size_t]*)
(bind-lib gsllib gsl_stats_wvariance_m [double,double*,size_t,double*,size_t,size_t,double]*)
(bind-lib gsllib gsl_stats_wsd [double,double*,size_t,double*,size_t,size_t]*)
(bind-lib gsllib gsl_stats_wsd_m [double,double*,size_t,double*,size_t,size_t,double]*)
(bind-lib gsllib gsl_stats_wtss [double,double*,size_t,double*,size_t,size_t]*)
(bind-lib gsllib gsl_stats_wtss_m [double,double*,size_t,double*,size_t,size_t,double]*)
(bind-lib gsllib gsl_stats_wvariance_with_fixed_mean [double,double*,size_t,double*,size_t,size_t,double]*)
(bind-lib gsllib gsl_stats_wsd_with_fixed_mean [double,double*,size_t,double*,size_t,size_t,double]*)
(bind-lib gsllib gsl_stats_wabsdev [double,double*,size_t,double*,size_t,size_t]*)
(bind-lib gsllib gsl_stats_wabsdev_m [double,double*,size_t,double*,size_t,size_t,double]*)
(bind-lib gsllib gsl_stats_wskew [double,double*,size_t,double*,size_t,size_t]*)
(bind-lib gsllib gsl_stats_wskew_m_sd [double,double*,size_t,double*,size_t,size_t,double,double]*)
(bind-lib gsllib gsl_stats_wkurtosis [double,double*,size_t,double*,size_t,size_t]*)
(bind-lib gsllib gsl_stats_wkurtosis_m_sd [double,double*,size_t,double*,size_t,size_t,double,double]*)

(bind-lib gsllib gsl_stats_max [double,double*,size_t,size_t]*)
(bind-lib gsllib gsl_stats_min [double,double*,size_t,size_t]*)
(bind-lib gsllib gsl_stats_max_index [size_t,double*,size_t,size_t]*)
(bind-lib gsllib gsl_stats_min_index [size_t,double*,size_t,size_t]*)

(bind-lib gsllib gsl_stats_median_from_sorted_data [double,double*,size_t,size_t]*)
(bind-lib gsllib gsl_stats_quantile_from_sorted_data [double,double*,size_t,size_t,double]*)
