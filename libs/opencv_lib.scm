;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Library for opencv
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ocv_core_lib
  (if (string=? "Linux" (sys:platform))
      (sys:open-dylib "libopencv_core.so")
      (print-error "tell me where to find the opencv core dynamic library on your platform here!")))

(define ocv_hgui_lib
  (if (string=? "Linux" (sys:platform))
      (sys:open-dylib "libopencv_highgui.so")
      (print-error "tell me where to find the opencv core dynamic library on your platform here!")))

(define ocv_features_lib
  (if (string=? "Linux" (sys:platform))
      (sys:open-dylib "libopencv_features2d.so")
      (print-error "tell me where to find the opencv core dynamic library on your platform here!")))


(define ocv_iproc_lib
  (if (string=? "Linux" (sys:platform))
      (sys:open-dylib "libopencv_imgproc.so")
      (print-error "tell me where to find the opencv core dynamic library on your platform here!")))


;; image depth constants
(bind-val IPL_DEPTH_1U  i32 1)
(bind-val IPL_DEPTH_8U  i32 8)
(bind-val IPL_DEPTH_16U i32 16)
(bind-val IPL_DEPTH_32F i32 32)

(bind-val IPL_DEPTH_8S i32 (+ 2147483648 8))
(bind-val IPL_DEPTH_16S i32 (+ 2147483648 16))
(bind-val IPL_DEPTH_32S i32 (+ 2147483648 32))

;; (define ocv_video_lib
;;   (if (string=? "Linux" (sys:platform))
;;       (sys:open-dylib "libopencv_video.so")
;;       (print-error "tell me where to find the opencv core dynamic library on your platform here!")))

(bind-alias CvArr i8)
(bind-alias CvSeq i8) ;; cvseq is really complex struct
(bind-alias CvFont i8) ;; cvfont is really complex struct
(bind-alias CvGraph i8) ;; cvgraph is really complex struct
(bind-alias CvGraphScanner i8) ;; CvGraphScanner is really complex struct
(bind-alias CvHistogram i8) ;; CvGraphScanner is really complex struct

(bind-val CV_LOAD_IMAGE_COLOR     i32  1)
(bind-val CV_LOAD_IMAGE_GRAYSCALE i32  0)
(bind-val CV_LOAD_IMAGE_UNCHANGED i32 -1)

(bind-type CvPoint <i32,i32>)
(bind-type CvSize <i32,i32>)
(bind-type CvScalar <|4,double|>)
(bind-type CvRect <i32,i32,i32,i32>)
(bind-type CvSize2D32f <float,float>)
(bind-type CvPoint2D32f <float,float>)
(bind-type CvPoint3D32f <float,float,float>)
(bind-type CvPoint2D64f <double,double>)
(bind-type CvPoint3D64f <double,double,double>)
(bind-type CvBox2D <float,CvPoint2D32f,CvSize2D32f>)
(bind-type CvMemBlock <CvMemBlock*,CvMemBlock*>)
(bind-type CvMemStorage <CvMemBlock*,CvMemBlock*,CvMemStorage*,i32,i32>)
(bind-type CvMemStoragePos <CvMemBlock*,i32>)
(bind-type CvSeqBlock <CvSeqBlock*,CvSeqBlock*,i32,i32,i32>)
(bind-type CvSlice <i32,i32>)
(bind-type CvSetElem <i32,CvSetElem*>)
(bind-type CvSet <i32,CvSetElem*>)
(bind-type CvGraphVtx <i32,i8*>)
(bind-type CvGraphEdge <i32,float,|2,CvGraphEdge*|,|2,CvGraphVtx*|*>)
(bind-type CvString <i32,i8*>)

;; typedef struct CvHistogram
;; {
;;     int     type;
;;     CvArr*  bins;
;;     float   thresh[CV_MAX_DIM][2]; /* for uniform histograms */
;;     float** thresh2; /* for non-uniform histograms */
;;     CvMatND mat; /* embedded matrix header for array histograms */
;; }
(bind-type CvMatND <i32,i32,i32*,i8*,|32,<i32,i32>|>)
(bind-type CvHistogram <i32,CvArr*,|64,float|,float**,CvMatND>)

;; typedef struct _IplImage
;; {
;;     int  nSize;                    -> 0
;;     int  ID;
;;     int  nChannels;                -> 2 number of channels
;;     int  alphaChannel;
;;     int  depth;                    -> 4 depth (in bits)
;;     char colorModel[4];            -> 5
;;     char channelSeq[4];
;;     int  dataOrder;
;;     int  origin;
;;     int  align;
;;     int  width;                    -> 10  width
;;     int  height;                   -> 11  height
;;     struct _IplROI *roi;
;;     struct _IplImage *maskROI;
;;     void  *imageId;
;;     struct _IplTileInfo *tileInfo; -> 15
;;     int  imageSize;
;;     char *imageData;               -> 17 image data
;;     int  widthStep;
;;     int  BorderMode[4];     
;;     int  BorderConst[4];           -> 20
;;     char *imageDataOrigin;
;; }
(bind-type IplImage <i32,i32,i32,i32,i32,|4,i8|,|4,i8|,i32,i32,i32,i32,i32,i8*,i8*,i8*,i8*,i32,i8*,i32,|4,i32|,|4,i32|,i8*>)

;; data related stuff
(bind-lib ocv_core_lib cvCreateMemStorage [CvMemStorage*,i32]*)
(bind-lib ocv_core_lib cvMemStorageAlloc [i8*,CvMemStorage*,i32]*)
(bind-lib ocv_core_lib cvClearMemStorage [void,CvMemStorage*]*)
(bind-lib ocv_core_lib cvClearSeq [void,CvSeq*]*)
(bind-lib ocv_core_lib cvClearSet [void,CvSet*]*)
(bind-lib ocv_core_lib cvCloneGraph [CvGraph*,CvGraph*,CvMemStorage*]*)
(bind-lib ocv_core_lib cvCreateGraph [CvGraph*,i32,i32,i32,i32,CvMemStorage*]*)
(bind-lib ocv_core_lib cvCreateGraphScanner [CvGraphScanner*,CvGraph*,i8*,i32]*)
(bind-lib ocv_core_lib cvCvtSeqToArray [i8*,CvSeq*,i8*,CvSlice]*)
(bind-lib ocv_core_lib cvEndWriteSeq [CvSeq*,i8*]*)
(bind-lib ocv_core_lib cvFindGraphEdge [i8*,CvGraph*,i32,i32]*)
(bind-lib ocv_core_lib cvFindGraphEdgeByPtr [CvGraphEdge*,CvGraph*,CvGraphVtx*,CvGraphVtx*]*)
;(bind-lib ocv_core_lib cvGetGraphVtx [CvGraphVtx*,CvGraph*,i32]*)
(bind-lib ocv_core_lib cvGetSeqElem [i8*,CvSeq*,i32]*)
;(bind-lib ocv_core_lib cvGetSetElem [CvSetElem*,CvSet*,i32]*)
(bind-lib ocv_core_lib cvGraphAddEdge [i32,CvGraph*,i32,i32,CvGraphEdge*,CvGraphEdge**]*)
(bind-lib ocv_core_lib cvGraphAddEdgeByPtr [i32,CvGraph*,CvGraphVtx*,CvGraphVtx*,CvGraphEdge*,CvGraphEdge**]*)
(bind-lib ocv_core_lib cvGraphAddVtx [i32,CvGraph*,CvGraphVtx*,CvGraphVtx*]*)
;(bind-lib ocv_core_lib cvGraphEdgeIdx [i32,CvGraph*,CvGraphEdge*]*)
(bind-lib ocv_core_lib cvGraphRemoveEdge [void,CvGraph*,i32,i32]*)
(bind-lib ocv_core_lib cvGraphRemoveEdgeByPtr [void,CvGraph*,CvGraphVtx*,CvGraphVtx*]*)
(bind-lib ocv_core_lib cvGraphRemoveVtx [i32,CvGraph*,i32]*)
(bind-lib ocv_core_lib cvGraphRemoveVtxByPtr [i32,CvGraph*,CvGraphVtx*]*)
(bind-lib ocv_core_lib cvGraphVtxDegree [i32,CvGraph*,i32]*)
(bind-lib ocv_core_lib cvGraphVtxDegreeByPtr [i32,CvGraph*,CvGraphVtx*]*)
;(bind-lib ocv_core_lib cvGraphVtxIdx [i32,CvGraph*,CvGraphVtx*]*)
(bind-lib ocv_core_lib cvNextGraphItem [i32,CvGraphScanner*]*)
(bind-lib ocv_core_lib cvNextTreeNode [i8*,i8*]*)
(bind-lib ocv_core_lib cvPrevTreeNode [i8*,i8*]*)
(bind-lib ocv_core_lib cvReleaseGraphScanner [void,CvGraphScanner**]*)
(bind-lib ocv_core_lib cvReleaseMemStorage [void,CvMemStorage**]*)
(bind-lib ocv_core_lib cvSeqElemIdx [i32,CvSeq*,i8*,CvSeqBlock**]*)
(bind-lib ocv_core_lib cvSeqInsert [i8*,CvSeq*,i32,i8*]*)
(bind-lib ocv_core_lib cvSeqInsertSlice [void,CvSeq*,i32,CvArr*]*)
(bind-lib ocv_core_lib cvSeqInvert [void,CvSeq*]*)
(bind-lib ocv_core_lib cvSeqPop [void,CvSeq*,i8*]*)
(bind-lib ocv_core_lib cvSeqPopFront [void,CvSeq*,i8*]*)
(bind-lib ocv_core_lib cvSeqPush [i8*,CvSeq*,i8*]*)
(bind-lib ocv_core_lib cvSeqPushFront [i8*,CvSeq*,i8*]*)
(bind-lib ocv_core_lib cvSeqRemove [void,CvSeq*,i32]*)
(bind-lib ocv_core_lib cvSeqRemoveSlice [void,CvSeq*,CvSlice*]*)
(bind-lib ocv_core_lib cvSeqSearch [i8*,CvSeq*,i8*,i8*,i32,i32*,i8*]*)
(bind-lib ocv_core_lib cvSeqSlice [CvSeq*,CvSeq*,CvSlice,CvMemStorage*,i32]*)
(bind-lib ocv_core_lib cvSeqSort [void,CvSeq*,i8*,i8*]*)
(bind-lib ocv_core_lib cvSetAdd [i32,CvSet*,CvSetElem*,CvSetElem**]*)
;(bind-lib ocv_core_lib cvSetNew [CvSetElem*,CvSet*]*)
(bind-lib ocv_core_lib cvSetRemove [void,CvSet*,i32]*)
;(bind-lib ocv_core_lib cvSetRemoveByPtr [void,CvSet*,i8*]*)
(bind-lib ocv_core_lib cvStartAppendToSeq [void,CvSeq*,i8*]*)
(bind-lib ocv_core_lib cvStartReadSeq [void,CvSeq*,i8*,i32]*)
(bind-lib ocv_core_lib cvStartWriteSeq [i32,i32,i32,CvMemStorage*,i8*]*)
(bind-lib ocv_core_lib cvTreeToNodeSeq [CvSeq*,i8*,i32,CvMemStorage*]*)
(bind-lib ocv_core_lib cvCreateData [void,CvArr*]*)
(bind-lib ocv_core_lib cvReleaseData [void,CvArr*]*)
(bind-lib ocv_core_lib cvCreateImage [IplImage*,CvSize,i32,i32]*)
(bind-lib ocv_core_lib cvCloneImage [IplImage*,IplImage*]*)
(bind-lib ocv_core_lib cvReleaseImage [void,IplImage**]*)
(bind-lib ocv_core_lib cvCreateMat [i8*,i32,i32,i32]*)
(bind-lib ocv_core_lib cvSetImageCOI [void,IplImage*,i32]*)
(bind-lib ocv_core_lib cvSetImageROI [void,IplImage*,CvRect]*)

(bind-lib ocv_core_lib cvGetReal1D [double,CvArr*,i32]*)
(bind-lib ocv_core_lib cvGetReal2D [double,CvArr*,i32,i32]*)
(bind-lib ocv_core_lib cvGetReal3D [double,CvArr*,i32,i32,i32]*)
(bind-lib ocv_core_lib cvGetRealND [double,CvArr*,i32*]*)

;(bind-lib ocv_core_lib cvCloneSeq [CvSeq*,CvSeq*,CvMemStorage*]*)

;; i32 ->
;; 0 = flip around x
;; 1 = flip around y
;; -1 =  flip both x and y
(bind-lib ocv_core_lib cvFlip [void,CvArr*,CvArr*,i32]*)
(bind-lib ocv_core_lib cvGetSize [CvSize,CvArr*]*)

;; drawing functions
(bind-lib ocv_core_lib cvCircle [void,CvArr*,CvPoint,i32,CvScalar,i32,i32,i32]*)
(bind-lib ocv_core_lib cvClipLine [i32,CvSize,CvPoint*,CvPoint*]*)
(bind-lib ocv_core_lib cvDrawContours [void,CvArr*,CvSeq*,CvScalar,CvScalar,i32,i32,i32]*)
(bind-lib ocv_core_lib cvEllipse [void,CvArr*,CvPoint,CvSize,double,double,double,CvScalar,i32,i32,i32]*)
;(bind-lib ocv_core_lib cvEllipseBox [void,CvArr*,CvBox2D,CvScalar,i32,i32,i32]*)
(bind-lib ocv_core_lib cvFillConvexPoly [void,CvArr*,CvPoint*,i32,CvScalar,i32,i32]*)
(bind-lib ocv_core_lib cvFillPoly [void,CvArr*,CvPoint**,i32*,i32,CvScalar,i32,i32]*)
(bind-lib ocv_core_lib cvGetTextSize [void,i8*,CvFont*,CvSize*,i32*]*)
(bind-lib ocv_core_lib cvInitFont [void,CvFont*,i32,double,double,double,i32,i32]*)
(bind-lib ocv_core_lib cvInitLineIterator [i32,CvArr*,CvPoint,CvPoint,i8*,i32,i32,i32]*)
(bind-lib ocv_core_lib cvLine [void,CvArr*,CvPoint,CvPoint,CvScalar,i32,i32,i32]*)
(bind-lib ocv_core_lib cvPolyLine [void,CvArr*,CvPoint**,i32*,i32,i32,CvScalar,i32,i32,i32]*)
(bind-lib ocv_core_lib cvPutText [void,CvArr*,i8*,CvPoint,CvFont*,CvScalar]*)
(bind-lib ocv_core_lib cvRectangle [void,CvArr*,CvPoint,CvPoint,CvScalar,i32,i32,i32]*)
(bind-func cvMakeColour (lambda (r g b) ;; returns CvScalar
			  (let ((s:CvScalar* (salloc)) (a (tref-ptr s 0)))
			    (aset! a 0 b) (aset! a 1 g) (aset! a 0 r) (pref s 0))))
			    

;; void cvCanny(const CvArr* image, CvArr* edges, double threshold1, double threshold2, int aperture_size=3)
(bind-lib ocv_features_lib cvCanny [void,CvArr*,CvArr*,double,double,i32]*)
(bind-lib ocv_hgui_lib cvLoadImage [IplImage*,i8*,i32]*)
;(bind-lib ocv_hgui_lib cvCaptureFromFile [i8*,i8*]*)
(bind-lib ocv_hgui_lib cvGrabFrame [i32,i8*]*)
(bind-lib ocv_iproc_lib cvCvtColor [void,CvArr*,CvArr*,i32]*)

;; histograms
;; CvHistogram* cvCreateHist(int dims, int* sizes, int type, float** ranges=NULL, int uniform=1)¶
(bind-lib ocv_iproc_lib cvCreateHist [CvHistogram*,i32,i32*,i32,float**,i32]*)
(bind-lib ocv_iproc_lib cvCompareHist [double,CvHistogram*,CvHistogram*,i32]*)
(bind-lib ocv_iproc_lib cvCalcProbDensity [void,CvHistogram*,CvHistogram*,CvHistogram*,double]*)
(bind-lib ocv_iproc_lib cvCopyHist [void,CvHistogram*,CvHistogram**]*)
(bind-lib ocv_iproc_lib cvNormalizeHist [void,CvHistogram*,double]*)
(bind-lib ocv_iproc_lib cvReleaseHist [void,CvHistogram**]*)
(bind-lib ocv_iproc_lib cvSetHistBinRanges [void,CvHistogram*,float**,i32]*)
(bind-lib ocv_iproc_lib cvThreshHist [void,CvHistogram*,double]*)

;; void cvCalcHist(IplImage** image, CvHistogram* hist, int accumulate=0, const CvArr* mask=NULL)¶
(bind-lib ocv_iproc_lib cvCalcArrHist [void,IplImage**,CvHistogram*,i32,CvArr*]*)

(bind-func cvGetHistValue_1D
  (lambda (hist:CvHistogram* idx:i32)
    (let ((bins (tref hist 1)))
      (dtof (cvGetReal1D bins idx)))))

(bind-func cvGetHistValue_2D
  (lambda (hist:CvHistogram* idx1:i32 idx2:i32)
    (let ((bins (tref hist 1)))
      (dtof (cvGetReal2D bins idx1 idx2)))))

(bind-func cvGetHistValue_3D
  (lambda (hist:CvHistogram* idx1:i32 idx2:i32 idx3:i32)
    (let ((bins (tref hist 1)))
      (dtof (cvGetReal3D bins idx1 idx2 idx3)))))


;; image processing
;; void cvIntegral(const CvArr* image, CvArr* sum, CvArr* sqsum=NULL, CvArr* tiltedSum=NULL)¶
(bind-lib ocv_iproc_lib cvIntegral [void,CvArr*,CvArr*,CvArr*,CvArr*]*)
;;void cvFilter2D(const CvArr* src, CvArr* dst, const CvMat* kernel, CvPoint anchor=cvPoint(-1, -1))¶
(bind-lib ocv_iproc_lib cvFilter2D [void,CvArr*,CvArr*,i8*,CvPoint]*)

