;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Horde3D Bindings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define hordelib (if (string=? "Linux" (sys:platform))
		     (sys:open-dylib "libHorde3D.so")
		     (sys:open-dylib "libHorde3D.dylib")))

(define hordeutlib (if (string=? "Linux" (sys:platform))
		       (sys:open-dylib "libHorde3DUtils.so")
		       (sys:open-dylib "libHorde3DUtils.dylib")))


(bind-type H3DRes i32)
(bind-type H3DNode i32)
(bind-type H3DOption i32)
(bind-type bool i1)

(bind-lib hordelib h3dInit [bool]*)
(bind-lib hordelib h3dRelease [void]*)
(bind-lib hordelib h3dSetOption [i8,H3DOption,float]*)
(bind-lib hordelib h3dAddResource [H3DRes,i32,i8*,i32]*)
(bind-lib hordelib h3dAddCameraNode [H3DNode,H3DNode,i8*,H3DRes]*)
(bind-lib hordelib h3dAddLightNode [H3DNode,H3DNode,i8*,H3DRes,i8*,i8*]*)
(bind-lib hordelib h3dAddNodes [H3DNode,H3DNode,H3DRes]*)
(bind-lib hordelib h3dRemoveNode [void,H3DNode]*)
(bind-lib hordelib h3dSetNodeTransform [void,H3DNode,float,float,float,float,float,float,float,float,float]*)
(bind-lib hordelib h3dSetupModelAnimStage [void,H3DNode,i32,H3DRes,i32,i8*,bool]*)
(bind-lib hordelib h3dFindNodes [i32,H3DNode,i8*,i32]*)
(bind-lib hordelib h3dFindResource [H3DRes,i32,i8*]*)
(bind-lib hordelib h3dGetNodeFindResult [H3DNode,i32]*)
(bind-lib hordelib h3dGetNodeAABB [void,H3DNode,float*,float*,float*,float*,float*,float*]*)
(bind-lib hordelib h3dSetNodeParamF [void,H3DNode,i32,i32,float]*)
(bind-lib hordelib h3dSetMaterialUniform [bool,H3DRes,i8*,float,float,float,float]*)
(bind-lib hordelib h3dSetModelAnimParams [void,H3DNode,i32,float,float]*)
(bind-lib hordelib h3dAddEmitterNode [H3DNode,H3DNode,i8*,H3DRes,H3DRes,i32,i32]*)
(bind-lib hordelib h3dAdvanceEmitterTime [void,H3DNode,float]*)
(bind-lib hordelib h3dGetNodeParamI [i32,H3DNode,i32]*)
(bind-lib hordelib h3dSetNodeParamI [void,H3DNode,i32,i32]*)
(bind-lib hordelib h3dRender [void,H3DNode]*)
(bind-lib hordelib h3dFinalizeFrame [void]*)
(bind-lib hordelib h3dClearOverlays [void]*)
(bind-lib hordelib h3dResizePipelineBuffers [void,H3DRes,i32,i32]*)
(bind-lib hordelib h3dSetupCameraView [void,H3DNode,float,float,float,float]*)
(bind-lib hordelib h3dGetMessage [i8*,i32*,float*]*)
(bind-lib hordelib h3dLoadResource [bool,H3DRes,i8*,i32]*)
(bind-lib hordelib h3dIsResLoaded [bool,H3DRes]*)
(bind-lib hordelib h3dUnloadResource [void,H3DRes]*)

(bind-lib hordeutlib h3dutDumpMessages [bool]*)
(bind-lib hordeutlib h3dutLoadResourcesFromDisk [bool,i8*]*)

;; global contants

;; EngineOptions
(bind-val H3DOptions_MaxLogLevel i32 1)
(bind-val H3DOptions_MaxNumMessages i32 2)
(bind-val H3DOptions_TriLinearFiltering i32 3)
(bind-val H3DOptions_MaxAnisotropy i32 4)
(bind-val H3DOptions_TexCompression i32 5)
(bind-val H3DOptions_SRGBLinearization i32 6)
(bind-val H3DOptions_LoadTextures i32 7)
(bind-val H3DOptions_FastAnimation i32 8)
(bind-val H3DOptions_ShadowMapSize i32 9)
(bind-val H3DOptions_SampleCount i32 10)
(bind-val H3DOptions_WireframeMode i32 11)
(bind-val H3DOptions_DebugViewMode i32 12)
(bind-val H3DOptions_DumpFailedShaders i32 13)
(bind-val H3DOptions_GatherTimeStats i32 14)

;; ResourceTypes
(bind-val H3DResTypes_Undefined i32 0)
(bind-val H3DResTypes_SceneGraph i32 1)
(bind-val H3DResTypes_Geometry i32 2)
(bind-val H3DResTypes_Animation i32 3)
(bind-val H3DResTypes_Material i32 4)
(bind-val H3DResTypes_Code i32 5)
(bind-val H3DResTypes_Shader i32 6)
(bind-val H3DResTypes_Texture i32 7)
(bind-val H3DResTypes_ParticleEffect i32 8)
(bind-val H3DResTypes_Pipeline i32 9)

;; SceneNodeTypes
(bind-val H3DNodeTypes_Undefined i32 0)
(bind-val H3DNodeTypes_Group i32 1)
(bind-val H3DNodeTypes_Model i32 2)
(bind-val H3DNodeTypes_Mesh i32 3)
(bind-val H3DNodeTypes_Joint i32 4)
(bind-val H3DNodeTypes_Light i32 5)
(bind-val H3DNodeTypes_Camera i32 6)
(bind-val H3DNodeTypes_Emitter i32 7)

;; SceneNodeParams
(bind-val H3DNode_NameStr i32 1)
(bind-val H3DNode_AttachmentStr i32 2)

;; SceneNodeFlags
(bind-val H3DNodeFlags_NoDraw i32 1)
(bind-val H3DNodeFlags_NoCastShadow i32 2)
(bind-val H3DNodeFlags_NoRayQuery i32 4)
(bind-val H3DNodeFlags_Inactive i32 7)

;; LightNodeParams
(bind-val H3DLight_MatResI i32 500)
(bind-val H3DLight_RadiusF i32 501)
(bind-val H3DLight_FovF i32 502)
(bind-val H3DLight_ColorF3 i32 503)
(bind-val H3DLight_ColorMultiplierF i32 504)
(bind-val H3DLight_ShadowMapCountI i32 505)
(bind-val H3DLight_ShadowSplitLambdaF i32 506)
(bind-val H3DLight_ShadowMapBiasF i32 507)
(bind-val H3DLight_LightingContextStr i32 508)
(bind-val H3DLight_LightingShadowContextStr i32 509)

;; CameraNodeParams
(bind-val H3DCamera_PipeResI i32 600)
(bind-val H3DCamera_OutTexResI i32 601)
(bind-val H3DCamera_OutBufIndexI i32 602)
(bind-val H3DCamera_LeftPlaneF i32 603)
(bind-val H3DCamera_RightPlaneF i32 604)
(bind-val H3DCamera_BottomPlaneF i32 605)
(bind-val H3DCamera_TopPlaneF i32 606)
(bind-val H3DCamera_NearPlaneF i32 607)
(bind-val H3DCamera_FarPlaneF i32 608)
(bind-val H3DCamera_ViewportXI i32 609)
(bind-val H3DCamera_ViewportYI i32 610)
(bind-val H3DCamera_ViewportWidthI i32 611)
(bind-val H3DCamera_ViewportHeightI i32 612)
(bind-val H3DCamera_OrthoI i32 613)
(bind-val H3DCamera_OccCullingI i32 614)

;; ROOT NODE
(bind-val H3DRootNode i32 1)
