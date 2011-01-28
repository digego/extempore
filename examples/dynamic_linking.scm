;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; A short demonstration of Extempore's ability to 
;; link with dynamic libraries at runtime!
;;
;; The importance of this example is to demonstrate
;; Extempore's ability to bind to arbitrary dynamic
;; libraries at runtime.  In this case OpenCV.
;;
;; There are some limitations on this - for example
;; at present only C libraries (not objc or C++)
;; can be dynamically bound at runtime and understood
;; by the extempore compiler.  There are also some
;; limitations imposed by restrictions in the
;; extempore compiler - particularly function 
;; pointers (callbacks) - however, these are high on 
;; the must-fix list.
;; 
;; Of course there are ways to bind C++ and ObjC 
;; libraries for static use.  However, this method
;; of dynamic binding at runtime presents a very easy 
;; and flexible mechanism for language extension
;; on-the-fly, and certainly provides a way to
;; rapidly develop library extensions.  This nice
;; thing is that this extension can happen within
;; the Extempore runtime itself - rather than requiring
;; static compilation at the project level.
;; 
;; This little bit of code packs a pretty big punch
;; 1) Opens the OpenCV HighGUI dynamic library
;; 2) Binds 4 function definitions that we will use
;; 3) Compiles text-opencv using these 4 functions
;; 4) Runs the code translating the supplied image


;; load highgui dynamic library
(define highguilib (sys:open-dylib "libhighgui.so.4"))
                               
;; bind the symbols we need so that the
;; extempore compiler can find (and understand) them
(dynamic-bind highguilib cvLoadImage [i8*,i8*,i32]*)
(dynamic-bind highguilib cvCloneImage [i8*,i8*]*)
(dynamic-bind highguilib cvCanny [void,i8*,i8*,double,double,i32]*)
(dynamic-bind highguilib cvSaveImage [i32,i8*,i8*]*)

;; load image and apply canny edge detection
(definec test-opencv
   (lambda (path-to-in-img path-to-out-img)
     (let* ((imgin (cvLoadImage path-to-in-img 0))
            (imgout (cvCloneImage imgin)))
       (cvCanny imgin imgout 400.0 100.0 3)
       (cvSaveImage path-to-out-img imgout))))

;; GO!
(test-opencv "/tmp/my-image.jpg" "/tmp/my-new-image.jpg")

;; We're done - close dylib
(sys:close-dylib highguilib)
