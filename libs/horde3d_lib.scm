;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Horde3D Bindings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define hordelib (if (string=? "Linux" (sys:platform))
		     (sys:open-dylib "libHorde3D.so")
		     (if (string=? "Windows" (sys:platform))
			 (sys:open-dylib "Horde3D.dll")
			 (sys:open-dylib "libHorde3D.dylib"))))

(define hordeutlib (if (string=? "Linux" (sys:platform))
		       (sys:open-dylib "libHorde3DUtils.so")
		       (if (string=? "Windows" (sys:platform))
			   (sys:open-dylib "Horde3DUtils.dll")
			   (sys:open-dylib "libHorde3DUtils.dylib"))))


(bind-lib hordelib h3dInit [i1]*)
(bind-lib hordelib h3dRelease [void]*)
(bind-lib hordelib h3dSetOption [i8,i32,float]*)
(bind-lib hordelib h3dAddResource [i32,i32,i8*,i32]*)
(bind-lib hordelib h3dAddCameraNode [i32,i32,i8*,i32]*)
(bind-lib hordelib h3dAddLightNode [i32,i32,i8*,i32,i8*,i8*]*)
(bind-lib hordelib h3dAddNodes [i32,i32,i32]*)
(bind-lib hordelib h3dRemoveNode [void,i32]*)
(bind-lib hordelib h3dSetNodeTransform [void,i32,float,float,float,float,float,float,float,float,float]*)
(bind-lib hordelib h3dSetupModelAnimStage [void,i32,i32,i32,i32,i8*,i1]*)
(bind-lib hordelib h3dAddModelNode [i32,i32,i8*,i32]*)
(bind-lib hordelib h3dAddMeshNode [i32,i32,i8*,i32,i32,i32,i32,i32]*)
(bind-lib hordelib h3dFindNodes [i32,i32,i8*,i32]*)
(bind-lib hordelib h3dFindResource [i32,i32,i8*]*)
(bind-lib hordelib h3dGetNodeFindResult [i32,i32]*)
(bind-lib hordelib h3dGetNodeAABB [void,i32,float*,float*,float*,float*,float*,float*]*)
(bind-lib hordelib h3dSetNodeParamF [void,i32,i32,i32,float]*)
(bind-lib hordelib h3dSetMaterialUniform [i1,i32,i8*,float,float,float,float]*)
(bind-lib hordelib h3dSetModelAnimParams [void,i32,i32,float,float]*)
(bind-lib hordelib h3dAddEmitterNode [i32,i32,i8*,i32,i32,i32,i32]*)
(bind-lib hordelib h3dAdvanceEmitterTime [void,i32,float]*)
(bind-lib hordelib h3dGetNodeParamI [i32,i32,i32]*)
(bind-lib hordelib h3dSetNodeParamI [void,i32,i32,i32]*)
(bind-lib hordelib h3dRender [void,i32]*)
(bind-lib hordelib h3dFinalizeFrame [void]*)
(bind-lib hordelib h3dClearOverlays [void]*)
(bind-lib hordelib h3dResizePipelineBuffers [void,i32,i32,i32]*)
(bind-lib hordelib h3dSetupCameraView [void,i32,float,float,float,float]*)
(bind-lib hordelib h3dGetMessage [i8*,i32*,float*]*)
(bind-lib hordelib h3dLoadResource [i1,i32,i8*,i32]*)
(bind-lib hordelib h3dIsResLoaded [i1,i32]*)
(bind-lib hordelib h3dUnloadResource [void,i32]*)
(bind-lib hordelib h3dSetNodeTransMat [void,i32,float*]*)
(bind-lib hordelib h3dGetNodeTransMats [void,i32,float**,float**]*)
(bind-lib hordeutlib h3dutDumpMessages [i1]*)
(bind-lib hordeutlib h3dutLoadResourcesFromDisk [i1,i8*]*)
(bind-lib hordeutlib h3dutCreateGeometryRes [i32,i8*,i32,i32,float*,i32*,i32*,i32*,i32*,float*,float*]*)
(bind-lib hordeutlib h3dutScreenshot [i1,i8*]*)
					     
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

;; Model Node Params
(bind-val H3DModel_GeoResI i32 200)
(bind-val H3DModel_SWSkinningI i32 201)
(bind-val H3DModel_LodDist1F i32 202)
(bind-val H3DModel_LodDist2F i32 203)
(bind-val H3DModel_LodDist3F i32 204)
(bind-val H3DModel_LodDist4F i32 205)

;; Mesh Node Params
(bind-val H3DMesh_MatResI 300)
(bind-val H3DMesh_BatchStartI 301)
(bind-val H3DMesh_BatchCountI 302)
(bind-val H3DMesh_VertRStartI 303)
(bind-val H3DMesh_VertREndI 304)
(bind-val H3DMesh_LodLevelI 305)

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
