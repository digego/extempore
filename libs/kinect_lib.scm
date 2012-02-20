;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Kinect for Windows
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define kinectlib (if (string=? "Linux" (sys:platform))
		       (println "Kinect Not Supported on Linux")
		       (if (string=? "Windows" (sys:platform))
			   (sys:open-dylib "kinectlib.dll")
			   (println "Kinect not supported on OSX"))))

(bind-type Vector4 <float,float,float,float>)
(bind-type NUI_SKELETON_DATA <i32,i32,i32,i32,Vector4,|20,Vector4|,|20,i32|,i32>)
(bind-type NUI_SKELETON_FRAME <i64,i32,i32,Vector4,Vector4,|6,NUI_SKELETON_DATA|>)
(bind-type NUI_TRANSFORM_SMOOTH_PARAMETERS <float,float,float,float,float>)

(bind-val NUI_INITIALIZE_FLAG_USES_AUDIO i32 268435456)
(bind-val NUI_INITIALIZE_FLAG_USES_DEPTH_AND_PLAYER_INDEX i32 1)
(bind-val NUI_INITIALIZE_FLAG_USES_COLOR i32 2)
(bind-val NUI_INITIALIZE_FLAG_USES_SKELETON i32 8)
(bind-val NUI_INITIALIZE_FLAG_USES_DEPTH i32 32)
(bind-val NUI_INITIALIZE_DEFAULT_HARDWARE_THREAD i32 4294967295)

(bind-val NUI_SKELETON_POSITION_HIP_CENTER i32 0)
(bind-val NUI_SKELETON_POSITION_SPINE i32 1)
(bind-val NUI_SKELETON_POSITION_SHOULDER_CENTER i32 2)
(bind-val NUI_SKELETON_POSITION_HEAD i32 3)
(bind-val NUI_SKELETON_POSITION_SHOULDER_LEFT i32 4)
(bind-val NUI_SKELETON_POSITION_ELBOW_LEFT i32 5)
(bind-val NUI_SKELETON_POSITION_WRIST_LEFT i32 6)
(bind-val NUI_SKELETON_POSITION_HAND_LEFT i32 7)
(bind-val NUI_SKELETON_POSITION_SHOULDER_RIGHT i32 8)
(bind-val NUI_SKELETON_POSITION_ELBOW_RIGHT i32 9)
(bind-val NUI_SKELETON_POSITION_WRIST_RIGHT i32 10)
(bind-val NUI_SKELETON_POSITION_HAND_RIGHT i32 11)
(bind-val NUI_SKELETON_POSITION_HIP_LEFT i32 12)
(bind-val NUI_SKELETON_POSITION_KNEE_LEFT i32 13) 
(bind-val NUI_SKELETON_POSITION_ANKLE_LEFT i32 14)
(bind-val NUI_SKELETON_POSITION_FOOT_LEFT i32 15)
(bind-val NUI_SKELETON_POSITION_HIP_RIGHT i32 16)
(bind-val NUI_SKELETON_POSITION_KNEE_RIGHT i32 17)
(bind-val NUI_SKELETON_POSITION_ANKLE_RIGHT i32 18)
(bind-val NUI_SKELETON_POSITION_FOOT_RIGHT i32 19)
(bind-val NUI_SKELETON_POSITION_COUNT i32 20)

(bind-lib kinectlib kinect_start [i32,i32]*)
(bind-lib kinectlib kinect_get_skeleton [i32,i64,NUI_SKELETON_FRAME*]*)
(bind-lib kinectlib kinect_shutdown [void]*)
(bind-lib kinectlib kinect_smooth [i32,NUI_SKELETON_FRAME*,NUI_TRANSFORM_SMOOTH_PARAMETERS*]*)

(definec kinect-start
  (lambda (v:i32)    
    (printf "kinect started: %d\n" (kinect_start NUI_INITIALIZE_FLAG_USES_SKELETON))))

(definec kinect-shutdown
  (lambda ()
    (kinect_shutdown)))

(definec kinect-smooth-params
  (let ((smooth_params (zalloc 1 NUI_TRANSFORM_SMOOTH_PARAMETERS)))
    (lambda (skeleton smoothing correction prediction jitter_radius max_deviation_radius)
      (tfill! smooth_params smoothing correction prediction jitter_radius max_deviation_radius)
      (kinect_smooth skeleton smooth_params))))

(definec kinect-smooth
  (let ((smooth_params (zalloc 1 NUI_TRANSFORM_SMOOTH_PARAMETERS)))
    (tfill! smooth_params (dtof 0.5) (dtof 0.5) (dtof 0.5) (dtof 0.05) (dtof 0.04))
    (lambda (skeleton)
      (kinect_smooth skeleton smooth_params))))
