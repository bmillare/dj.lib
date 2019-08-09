(ns dj.simulations.cuda
  (:require [dj.dispatch.treefn :as tf]))

;;; Purpose
;; - cuda specific

(def constants
  {:BYTE 1
   :CHAR 2
   :DOUBLE 8
   :FLOAT 4
   :INT 4
   :LONG 8
   :SHORT 2
   :CU_TR_ADDRESS_MODE_CLAMP 1
   :CU_TR_ADDRESS_MODE_MIRROR 2
   :CU_TR_ADDRESS_MODE_WRAP 0
   :CU_CUBEMAP_FACE_NEGATIVE_X 1
   :CU_CUBEMAP_FACE_NEGATIVE_Y 3
   :CU_CUBEMAP_FACE_NEGATIVE_Z 5
   :CU_CUBEMAP_FACE_POSITIVE_X 0
   :CU_CUBEMAP_FACE_POSITIVE_Y 2
   :CU_CUBEMAP_FACE_POSITIVE_Z 4
   :CU_AD_FORMAT_FLOAT 32
   :CU_AD_FORMAT_HALF 16
   :CU_AD_FORMAT_SIGNED_INT16 9
   :CU_AD_FORMAT_SIGNED_INT32 10
   :CU_AD_FORMAT_SIGNED_INT8 8
   :CU_AD_FORMAT_UNSIGNED_INT16 2
   :CU_AD_FORMAT_UNSIGNED_INT32 3
   :CU_AD_FORMAT_UNSIGNED_INT8 1
   :CU_COMPUTEMODE_DEFAULT 0
   :CU_COMPUTEMODE_EXCLUSIVE 1
   :CU_COMPUTEMODE_EXCLUSIVE_PROCESS 3
   :CU_COMPUTEMODE_PROHIBITED 2
   :CU_CTX_BLOCKING_SYNC 4
   :CU_CTX_FLAGS_MASK 63
   :CU_CTX_LMEM_RESIZE_TO_MAX 16
   :CU_CTX_MAP_HOST 8
   :CU_CTX_SCHED_AUTO 0
   :CU_CTX_SCHED_BLOCKING_SYNC 4
   :CU_CTX_SCHED_MASK 7
   :CU_CTX_SCHED_SPIN 1
   :CU_CTX_SCHED_YIELD 2
   :CU_DEVICE_ATTRIBUTE_ASYNC_ENGINE_COUNT 40
   :CU_DEVICE_ATTRIBUTE_CAN_MAP_HOST_MEMORY 19
   :CU_DEVICE_ATTRIBUTE_CAN_TEX2D_GATHER 44
   :CU_DEVICE_ATTRIBUTE_CLOCK_RATE 13
   :CU_DEVICE_ATTRIBUTE_COMPUTE_MODE 20
   :CU_DEVICE_ATTRIBUTE_CONCURRENT_KERNELS 31
   :CU_DEVICE_ATTRIBUTE_ECC_ENABLED 32
   :CU_DEVICE_ATTRIBUTE_GLOBAL_MEMORY_BUS_WIDTH 37
   :CU_DEVICE_ATTRIBUTE_GPU_OVERLAP 15
   :CU_DEVICE_ATTRIBUTE_INTEGRATED 18
   :CU_DEVICE_ATTRIBUTE_KERNEL_EXEC_TIMEOUT 17
   :CU_DEVICE_ATTRIBUTE_L2_CACHE_SIZE 38
   :CU_DEVICE_ATTRIBUTE_MAX_BLOCK_DIM_X 2
   :CU_DEVICE_ATTRIBUTE_MAX_BLOCK_DIM_Y 3
   :CU_DEVICE_ATTRIBUTE_MAX_BLOCK_DIM_Z 4
   :CU_DEVICE_ATTRIBUTE_MAX_GRID_DIM_X 5
   :CU_DEVICE_ATTRIBUTE_MAX_GRID_DIM_Y 6
   :CU_DEVICE_ATTRIBUTE_MAX_GRID_DIM_Z 7
   :CU_DEVICE_ATTRIBUTE_MAX_PITCH 11
   :CU_DEVICE_ATTRIBUTE_MAX_REGISTERS_PER_BLOCK 12
   :CU_DEVICE_ATTRIBUTE_MAX_SHARED_MEMORY_PER_BLOCK 8
   :CU_DEVICE_ATTRIBUTE_MAX_THREADS_PER_BLOCK 1
   :CU_DEVICE_ATTRIBUTE_MAX_THREADS_PER_MULTIPROCESSOR 39
   :CU_DEVICE_ATTRIBUTE_MAXIMUM_SURFACE1D_LAYERED_LAYERS 62
   :CU_DEVICE_ATTRIBUTE_MAXIMUM_SURFACE1D_LAYERED_WIDTH 61
   :CU_DEVICE_ATTRIBUTE_MAXIMUM_SURFACE1D_WIDTH 55
   :CU_DEVICE_ATTRIBUTE_MAXIMUM_SURFACE2D_HEIGHT 57
   :CU_DEVICE_ATTRIBUTE_MAXIMUM_SURFACE2D_LAYERED_HEIGHT 64
   :CU_DEVICE_ATTRIBUTE_MAXIMUM_SURFACE2D_LAYERED_LAYERS 65
   :CU_DEVICE_ATTRIBUTE_MAXIMUM_SURFACE2D_LAYERED_WIDTH 63
   :CU_DEVICE_ATTRIBUTE_MAXIMUM_SURFACE2D_WIDTH 56
   :CU_DEVICE_ATTRIBUTE_MAXIMUM_SURFACE3D_DEPTH 60
   :CU_DEVICE_ATTRIBUTE_MAXIMUM_SURFACE3D_HEIGHT 59
   :CU_DEVICE_ATTRIBUTE_MAXIMUM_SURFACE3D_WIDTH 58
   :CU_DEVICE_ATTRIBUTE_MAXIMUM_SURFACECUBEMAP_LAYERED_LAYERS 68
   :CU_DEVICE_ATTRIBUTE_MAXIMUM_SURFACECUBEMAP_LAYERED_WIDTH 67
   :CU_DEVICE_ATTRIBUTE_MAXIMUM_SURFACECUBEMAP_WIDTH 66
   :CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE1D_LAYERED_LAYERS 43
   :CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE1D_LAYERED_WIDTH 42
   :CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE1D_LINEAR_WIDTH 69
   :CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE1D_WIDTH 21
   :CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE2D_ARRAY_HEIGHT 28
   :CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE2D_ARRAY_NUMSLICES 29
   :CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE2D_ARRAY_WIDTH 27
   :CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE2D_GATHER_HEIGHT 46
   :CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE2D_GATHER_WIDTH 45
   :CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE2D_HEIGHT 23
   :CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE2D_LAYERED_HEIGHT 28
   :CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE2D_LAYERED_LAYERS 29
   :CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE2D_LAYERED_WIDTH 27
   :CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE2D_LINEAR_HEIGHT 71
   :CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE2D_LINEAR_PITCH 72
   :CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE2D_LINEAR_WIDTH 70
   :CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE2D_WIDTH 22
   :CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE3D_DEPTH 26
   :CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE3D_DEPTH_ALTERNATE 49
   :CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE3D_HEIGHT 25
   :CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE3D_HEIGHT_ALTERNATE 48
   :CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE3D_WIDTH 24
   :CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE3D_WIDTH_ALTERNATE 47
   :CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURECUBEMAP_LAYERED_LAYERS 54
   :CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURECUBEMAP_LAYERED_WIDTH 53
   :CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURECUBEMAP_WIDTH 52
   :CU_DEVICE_ATTRIBUTE_MEMORY_CLOCK_RATE 36
   :CU_DEVICE_ATTRIBUTE_MULTIPROCESSOR_COUNT 16
   :CU_DEVICE_ATTRIBUTE_PCI_BUS_ID 33
   :CU_DEVICE_ATTRIBUTE_PCI_DEVICE_ID 34
   :CU_DEVICE_ATTRIBUTE_PCI_DOMAIN_ID 50
   :CU_DEVICE_ATTRIBUTE_REGISTERS_PER_BLOCK 12
   :CU_DEVICE_ATTRIBUTE_SHARED_MEMORY_PER_BLOCK 8
   :CU_DEVICE_ATTRIBUTE_SURFACE_ALIGNMENT 30
   :CU_DEVICE_ATTRIBUTE_TCC_DRIVER 35
   :CU_DEVICE_ATTRIBUTE_TEXTURE_ALIGNMENT 14
   :CU_DEVICE_ATTRIBUTE_TEXTURE_PITCH_ALIGNMENT 51
   :CU_DEVICE_ATTRIBUTE_TOTAL_CONSTANT_MEMORY 9
   :CU_DEVICE_ATTRIBUTE_UNIFIED_ADDRESSING 41
   :CU_DEVICE_ATTRIBUTE_WARP_SIZE 10
   :CU_EVENT_BLOCKING_SYNC 1
   :CU_EVENT_DEFAULT 0
   :CU_EVENT_DISABLE_TIMING 2
   :CU_EVENT_INTERPROCESS 4
   :CU_TR_FILTER_MODE_LINEAR 1
   :CU_TR_FILTER_MODE_POINT 0
   :CU_FUNC_CACHE_PREFER_EQUAL 3
   :CU_FUNC_CACHE_PREFER_L1 2
   :CU_FUNC_CACHE_PREFER_NONE 0
   :CU_FUNC_CACHE_PREFER_SHARED 1
   :CU_FUNC_ATTRIBUTE_BINARY_VERSION 6
   :CU_FUNC_ATTRIBUTE_CONST_SIZE_BYTES 2
   :CU_FUNC_ATTRIBUTE_LOCAL_SIZE_BYTES 3
   :CU_FUNC_ATTRIBUTE_MAX_THREADS_PER_BLOCK 0
   :CU_FUNC_ATTRIBUTE_NUM_REGS 4
   :CU_FUNC_ATTRIBUTE_PTX_VERSION 5
   :CU_FUNC_ATTRIBUTE_SHARED_SIZE_BYTES 1
   :CU_GL_DEVICE_LIST_ALL 1
   :CU_GL_DEVICE_LIST_CURRENT_FRAME 2
   :CU_GL_DEVICE_LIST_NEXT_FRAME 3
   :CU_GL_MAP_RESOURCE_FLAGS_NONE 0
   :CU_GL_MAP_RESOURCE_FLAGS_READ_ONLY 1
   :CU_GL_MAP_RESOURCE_FLAGS_WRITE_DISCARD 2
   :CU_GRAPHICS_MAP_RESOURCE_FLAGS_NONE 0
   :CU_GRAPHICS_MAP_RESOURCE_FLAGS_READ_ONLY 1
   :CU_GRAPHICS_MAP_RESOURCE_FLAGS_WRITE_DISCARD 2
   :CU_GRAPHICS_REGISTER_FLAGS_NONE 0
   :CU_GRAPHICS_REGISTER_FLAGS_READ_ONLY 1
   :CU_GRAPHICS_REGISTER_FLAGS_SURFACE_LDST 4
   :CU_GRAPHICS_REGISTER_FLAGS_TEXTURE_GATHER 8
   :CU_GRAPHICS_REGISTER_FLAGS_WRITE_DISCARD 2
   :CU_IPC_MEM_LAZY_ENABLE_PEER_ACCESS 1
   :CU_PREFER_BINARY 1
   :CU_PREFER_PTX 0
   :CU_JIT_ERROR_LOG_BUFFER 5
   :CU_JIT_ERROR_LOG_BUFFER_SIZE_BYTES 6
   :CU_JIT_FALLBACK_STRATEGY 10
   :CU_JIT_INFO_LOG_BUFFER 3
   :CU_JIT_INFO_LOG_BUFFER_SIZE_BYTES 4
   :CU_JIT_MAX_REGISTERS 0
   :CU_JIT_OPTIMIZATION_LEVEL 7
   :CU_JIT_TARGET 9
   :CU_JIT_TARGET_FROM_CUCONTEXT 8
   :CU_JIT_THREADS_PER_BLOCK 1
   :CU_JIT_WALL_TIME 2
   :CU_TARGET_COMPUTE_10 0
   :CU_TARGET_COMPUTE_11 1
   :CU_TARGET_COMPUTE_12 2
   :CU_TARGET_COMPUTE_13 3
   :CU_TARGET_COMPUTE_20 4
   :CU_TARGET_COMPUTE_21 5
   :CU_LIMIT_MALLOC_HEAP_SIZE 2
   :CU_LIMIT_PRINTF_FIFO_SIZE 1
   :CU_LIMIT_STACK_SIZE 0
   :CU_MEMORYTYPE_ARRAY 3
   :CU_MEMORYTYPE_DEVICE 2
   :CU_MEMORYTYPE_HOST 1
   :CU_MEMORYTYPE_UNIFIED 4
   :CU_OUT_CSV 1
   :CU_OUT_KEY_VALUE_PAIR 0
   :CU_POINTER_ATTRIBUTE_CONTEXT 1
   :CU_POINTER_ATTRIBUTE_DEVICE_POINTER 3
   :CU_POINTER_ATTRIBUTE_HOST_POINTER 4
   :CU_POINTER_ATTRIBUTE_MEMORY_TYPE 2
   :CUDA_ERROR_ALREADY_ACQUIRED 210
   :CUDA_ERROR_ALREADY_MAPPED 208
   :CUDA_ERROR_ARRAY_IS_MAPPED 207
   :CUDA_ERROR_ASSERT 710
   :CUDA_ERROR_CONTEXT_ALREADY_CURRENT 202
   :CUDA_ERROR_CONTEXT_ALREADY_IN_USE 216
   :CUDA_ERROR_CONTEXT_IS_DESTROYED 709
   :CUDA_ERROR_DEINITIALIZED 4
   :CUDA_ERROR_ECC_UNCORRECTABLE 214
   :CUDA_ERROR_FILE_NOT_FOUND 301
   :CUDA_ERROR_HOST_MEMORY_ALREADY_REGISTERED 712
   :CUDA_ERROR_HOST_MEMORY_NOT_REGISTERED 713
   :CUDA_ERROR_INVALID_CONTEXT 201
   :CUDA_ERROR_INVALID_DEVICE 101
   :CUDA_ERROR_INVALID_HANDLE 400
   :CUDA_ERROR_INVALID_IMAGE 200
   :CUDA_ERROR_INVALID_SOURCE 300
   :CUDA_ERROR_INVALID_VALUE 1
   :CUDA_ERROR_LAUNCH_FAILED 700
   :CUDA_ERROR_LAUNCH_INCOMPATIBLE_TEXTURING 703
   :CUDA_ERROR_LAUNCH_OUT_OF_RESOURCES 701
   :CUDA_ERROR_LAUNCH_TIMEOUT 702
   :CUDA_ERROR_MAP_FAILED 205
   :CUDA_ERROR_NO_BINARY_FOR_GPU 209
   :CUDA_ERROR_NO_DEVICE 100
   :CUDA_ERROR_NOT_FOUND 500
   :CUDA_ERROR_NOT_INITIALIZED 3
   :CUDA_ERROR_NOT_MAPPED 211
   :CUDA_ERROR_NOT_MAPPED_AS_ARRAY 212
   :CUDA_ERROR_NOT_MAPPED_AS_POINTER 213
   :CUDA_ERROR_NOT_READY 600
   :CUDA_ERROR_OPERATING_SYSTEM 304
   :CUDA_ERROR_OUT_OF_MEMORY 2
   :CUDA_ERROR_PEER_ACCESS_ALREADY_ENABLED 704
   :CUDA_ERROR_PEER_ACCESS_NOT_ENABLED 705
   :CUDA_ERROR_PEER_MEMORY_ALREADY_REGISTERED 706
   :CUDA_ERROR_PEER_MEMORY_NOT_REGISTERED 707
   :CUDA_ERROR_PRIMARY_CONTEXT_ACTIVE 708
   :CUDA_ERROR_PROFILER_ALREADY_STARTED 7
   :CUDA_ERROR_PROFILER_ALREADY_STOPPED 8
   :CUDA_ERROR_PROFILER_DISABLED 5
   :CUDA_ERROR_PROFILER_NOT_INITIALIZED 6
   :CUDA_ERROR_SHARED_OBJECT_INIT_FAILED 303
   :CUDA_ERROR_SHARED_OBJECT_SYMBOL_NOT_FOUND 302
   :CUDA_ERROR_TOO_MANY_PEERS 711
   :CUDA_ERROR_UNKNOWN 999
   :CUDA_ERROR_UNMAP_FAILED 206
   :CUDA_ERROR_UNSUPPORTED_LIMIT 215
   :CUDA_SUCCESS 0
   :CU_MEMHOSTALLOC_DEVICEMAP 2
   :CU_MEMHOSTALLOC_PORTABLE 1
   :CU_MEMHOSTALLOC_WRITECOMBINED 4
   :CU_MEMHOSTREGISTER_DEVICEMAP 2
   :CU_MEMHOSTREGISTER_PORTABLE 1
   :CU_MEMPEERREGISTER_DEVICEMAP 2
   :CU_PARAM_TR_DEFAULT -1
   :CU_TRSA_OVERRIDE_FORMAT 1
   :CU_TRSF_NORMALIZED_COORDINATES 2
   :CU_TRSF_READ_AS_INTEGER 1
   :CU_TRSF_SRGB 16
   :CUDA_ARRAY3D_2DARRAY 1
   :CUDA_ARRAY3D_CUBEMAP 4
   :CUDA_ARRAY3D_LAYERED 1
   :CUDA_ARRAY3D_SURFACE_LDST 2
   :CUDA_ARRAY3D_TEXTURE_GATHER 8
   :CUDA_VERSION 4010
   :cudaChannelFormatKindFloat 2
   :cudaChannelFormatKindNone 3
   :cudaChannelFormatKindSigned 0
   :cudaChannelFormatKindUnsigned 1
   :cudaComputeModeDefault 0
   :cudaComputeModeExclusive 1
   :cudaComputeModeExclusiveProcess 3
   :cudaComputeModeProhibited 2
   :cudaErrorAddressOfConstant 22
   :cudaErrorApiFailureBase 10000
   :cudaErrorAssert 59
   :cudaErrorCudartUnloading 29
   :cudaErrorDeviceAlreadyInUse 54
   :cudaErrorDevicesUnavailable 46
   :cudaErrorDuplicateSurfaceName 45
   :cudaErrorDuplicateTextureName 44
   :cudaErrorDuplicateVariableName 43
   :cudaErrorECCUncorrectable 39
   :cudaErrorHostMemoryAlreadyRegistered 61
   :cudaErrorHostMemoryNotRegistered 62
   :cudaErrorIncompatibleDriverContext 49
   :cudaErrorInitializationError 3
   :cudaErrorInsufficientDriver 35
   :cudaErrorInvalidChannelDescriptor 20
   :cudaErrorInvalidConfiguration 9
   :cudaErrorInvalidDevice 10
   :cudaErrorInvalidDeviceFunction 8
   :cudaErrorInvalidDevicePointer 17
   :cudaErrorInvalidFilterSetting 26
   :cudaErrorInvalidHostPointer 16
   :cudaErrorInvalidKernelImage 47
   :cudaErrorInvalidMemcpyDirection 21
   :cudaErrorInvalidNormSetting 27
   :cudaErrorInvalidPitchValue 12
   :cudaErrorInvalidResourceHandle 33
   :cudaErrorInvalidSurface 37
   :cudaErrorInvalidSymbol 13
   :cudaErrorInvalidTexture 18
   :cudaErrorInvalidTextureBinding 19
   :cudaErrorInvalidValue 11
   :cudaErrorLaunchFailure 4
   :cudaErrorLaunchOutOfResources 7
   :cudaErrorLaunchTimeout 6
   :cudaErrorMapBufferObjectFailed 14
   :cudaErrorMemoryAllocation 2
   :cudaErrorMemoryValueTooLarge 32
   :cudaErrorMissingConfiguration 1
   :cudaErrorMixedDeviceExecution 28
   :cudaErrorNoDevice 38
   :cudaErrorNoKernelImageForDevice 48
   :cudaErrorNotReady 34
   :cudaErrorNotYetImplemented 31
   :cudaErrorOperatingSystem 63
   :cudaErrorPeerAccessAlreadyEnabled 50
   :cudaErrorPeerAccessNotEnabled 51
   :cudaErrorPriorLaunchFailure 5
   :cudaErrorProfilerAlreadyStarted 57
   :cudaErrorProfilerAlreadyStopped 58
   :cudaErrorProfilerDisabled 55
   :cudaErrorProfilerNotInitialized 56
   :cudaErrorSetOnActiveProcess 36
   :cudaErrorSharedObjectInitFailed 41
   :cudaErrorSharedObjectSymbolNotFound 40
   :cudaErrorStartupFailure 127
   :cudaErrorSynchronizationError 25
   :cudaErrorTextureFetchFailed 23
   :cudaErrorTextureNotBound 24
   :cudaErrorTooManyPeers 60
   :cudaErrorUnknown 30
   :cudaErrorUnmapBufferObjectFailed 15
   :cudaErrorUnsupportedLimit 42
   :cudaSuccess 0
   :jcudaInternalError -2147483647
   :cudaFuncCachePreferEqual 3
   :cudaFuncCachePreferL1 2
   :cudaFuncCachePreferNone 0
   :cudaFuncCachePreferShared 1
   :cudaGLDeviceListAll 1
   :cudaGLDeviceListCurrentFrame 2
   :cudaGLDeviceListNextFrame 3
   :cudaGLMapFlagsNone 0
   :cudaGLMapFlagsReadOnly 1
   :cudaGLMapFlagsWriteDiscard 2
   :cudaGraphicsCubeFaceNegativeX 1
   :cudaGraphicsCubeFaceNegativeY 3
   :cudaGraphicsCubeFaceNegativeZ 5
   :cudaGraphicsCubeFacePositiveX 0
   :cudaGraphicsCubeFacePositiveY 2
   :cudaGraphicsCubeFacePositiveZ 4
   :cudaGraphicsMapFlagsNone 0
   :cudaGraphicsMapFlagsReadOnly 1
   :cudaGraphicsMapFlagsWriteDiscard 2
   :cudaGraphicsRegisterFlagsNone 0
   :cudaGraphicsRegisterFlagsReadOnly 1
   :cudaGraphicsRegisterFlagsSurfaceLoadStore 4
   :cudaGraphicsRegisterFlagsTextureGather 8
   :cudaGraphicsRegisterFlagsWriteDiscard 2
   :cudaLimitMallocHeapSize 2
   :cudaLimitPrintfFifoSize 1
   :cudaLimitStackSize 0
   :cudaMemcpyDefault 4
   :cudaMemcpyDeviceToDevice 3
   :cudaMemcpyDeviceToHost 2
   :cudaMemcpyHostToDevice 1
   :cudaMemcpyHostToHost 0
   :cudaMemoryTypeDevice 2
   :cudaMemoryTypeHost 1
   :cudaCSV 1
   :cudaKeyValuePair 0
   :cudaBoundaryModeClamp 1
   :cudaBoundaryModeTrap 2
   :cudaBoundaryModeZero 0
   :cudaFormatModeAuto 1
   :cudaFormatModeForced 0
   :cudaAddressModeClamp 1
   :cudaAddressModeWrap 0
   :cudaFilterModeLinear 1
   :cudaFilterModePoint 0
   :cudaReadModeElementType 0
   :cudaReadModeNormalizedFloat 1
   :cudaArrayCubemap 4
   :cudaArrayDefault 0
   :cudaArrayLayered 1
   :cudaArraySurfaceLoadStore 2
   :cudaArrayTextureGather 8
   :cudaDeviceBlockingSync 4
   :cudaDeviceLmemResizeToMax 16
   :cudaDeviceMapHost 8
   :cudaDeviceMask 31
   :cudaDeviceScheduleAuto 0
   :cudaDeviceScheduleBlockingSync 4
   :cudaDeviceScheduleMask 7
   :cudaDeviceScheduleSpin 1
   :cudaDeviceScheduleYield 2
   :cudaEventBlockingSync 1
   :cudaEventDefault 0
   :cudaEventDisableTiming 2
   :cudaEventInterprocess 4
   :cudaHostAllocDefault 0
   :cudaHostAllocMapped 2
   :cudaHostAllocPortable 1
   :cudaHostAllocWriteCombined 4
   :cudaHostRegisterDefault 0
   :cudaHostRegisterMapped 2
   :cudaHostRegisterPortable 1
   :cudaIpcMemLazyEnablePeerAccess 1
   :cudaPeerAccessDefault 0
   :CUDART_VERSION 4010
   :cudaSurfaceType1D 1
   :cudaSurfaceType1DLayered 241
   :cudaSurfaceType2D 2
   :cudaSurfaceType2DLayered 242
   :cudaSurfaceType3D 3
   :cudaSurfaceTypeCubemap 12
   :cudaSurfaceTypeCubemapLayered 252
   :cudaTextureType1D 1
   :cudaTextureType1DLayered 241
   :cudaTextureType2D 2
   :cudaTextureType2DLayered 242
   :cudaTextureType3D 3
   :cudaTextureTypeCubemap 12
   :cudaTextureTypeCubemapLayered 252})

(let [type (constants :FLOAT)]
  (defn ->float-array
    [^floats cpy-array]
    (let [size (count cpy-array)
          allocate-size (* size type)]
      (doto (jcuda.driver.CUdeviceptr.)
        (jcuda.driver.JCudaDriver/cuMemAlloc allocate-size)
        (jcuda.driver.JCudaDriver/cuMemcpyHtoD (jcuda.Pointer/to cpy-array)
                                               allocate-size)))))

(let [type (constants :INT)]
  (defn ->int-array
    [^ints cpy-array]
    (let [size (count cpy-array)
          allocate-size (* size type)]
      (doto (jcuda.driver.CUdeviceptr.)
        (jcuda.driver.JCudaDriver/cuMemAlloc allocate-size)
        (jcuda.driver.JCudaDriver/cuMemcpyHtoD (jcuda.Pointer/to cpy-array)
                                               allocate-size)))))

(extend-type jcuda.driver.CUmodule
  dj.dispatch.treefn/Idismantle
  (dismantle [this] (jcuda.driver.JCudaDriver/cuModuleUnload this)))

(extend-type jcuda.driver.CUdeviceptr
  dj.dispatch.treefn/Idismantle
  (dismantle [this] (jcuda.driver.JCudaDriver/cuMemFree this)))

(extend-type jcuda.driver.CUcontext
  dj.dispatch.treefn/Idismantle
  (dismantle [this] (jcuda.driver.JCudaDriver/cuCtxDestroy this)))

(extend-type jcuda.driver.CUstream
  dj.dispatch.treefn/Idismantle
  (dismantle [this] (jcuda.driver.JCudaDriver/cuStreamDestroy this)))

;; We don't know if it is safe to call this multiple times
(def cuda-init
  (delay (jcuda.driver.JCudaDriver/setExceptionsEnabled true)
         ;; We must run cuInit before invoking any cuda code except
         ;; exceptions. Documentation says the argument to this function must
         ;; be 0. There is no other reason.
         (jcuda.driver.JCudaDriver/cuInit 0)))

(def setup-cuda
  {:cuda/init
   (tf/fm
    []
    @cuda-init)
   :cuda/device
   (tf/fm
    [:cuda/init :app/device-id]
    (doto (jcuda.driver.CUdevice.)
      (jcuda.driver.JCudaDriver/cuDeviceGet device-id)))
   :cuda/device-name
   (tf/fm
    [:cuda/device]
    (let [string-size (int 256)
          ba-name (byte-array string-size)]
      (jcuda.driver.JCudaDriver/cuDeviceGetName ba-name
                                                string-size
                                                ^jcuda.driver.CUdevice device)
      (apply str (map char ba-name))))
   :cuda/device-count
   (tf/fm
    []
    (let [v (int-array 1)]
      (jcuda.driver.JCudaDriver/cuDeviceGetCount v)
      (first v)))
   :cuda/mem-info
   (tf/fm
    [:cuda/context ;implicit dependency
     ]
    (let [free (long-array 1)
          total (long-array 1)]
      (jcuda.driver.JCudaDriver/cuMemGetInfo free total)
      
      {:memory-free (first free)
       :memory-total (first total)}))
   :cuda/device-attributes
   (tf/fm
    [:cuda/device]
    (reduce (fn [ret attribute]
              (assoc ret
                     attribute
                     (let [v (int-array 1)]
                       (jcuda.driver.JCudaDriver/cuDeviceGetAttribute v
                                                                      (int (constants attribute))
                                                                      ^jcuda.driver.CUdevice device)
                       (first v))))
            {}
            [:CU_DEVICE_ATTRIBUTE_MAX_THREADS_PER_BLOCK
             :CU_DEVICE_ATTRIBUTE_MAX_BLOCK_DIM_X
             :CU_DEVICE_ATTRIBUTE_MAX_BLOCK_DIM_Y
             :CU_DEVICE_ATTRIBUTE_MAX_BLOCK_DIM_Z
             :CU_DEVICE_ATTRIBUTE_MAX_GRID_DIM_X
             :CU_DEVICE_ATTRIBUTE_MAX_GRID_DIM_Y
             :CU_DEVICE_ATTRIBUTE_MAX_GRID_DIM_Z
             :CU_DEVICE_ATTRIBUTE_MAX_SHARED_MEMORY_PER_BLOCK
             :CU_DEVICE_ATTRIBUTE_TOTAL_CONSTANT_MEMORY
             :CU_DEVICE_ATTRIBUTE_WARP_SIZE
             :CU_DEVICE_ATTRIBUTE_MAX_PITCH
             :CU_DEVICE_ATTRIBUTE_MAX_REGISTERS_PER_BLOCK
             :CU_DEVICE_ATTRIBUTE_CLOCK_RATE
             :CU_DEVICE_ATTRIBUTE_GPU_OVERLAP
             :CU_DEVICE_ATTRIBUTE_MULTIPROCESSOR_COUNT
             :CU_DEVICE_ATTRIBUTE_KERNEL_EXEC_TIMEOUT
             :CU_DEVICE_ATTRIBUTE_INTEGRATED
             :CU_DEVICE_ATTRIBUTE_CAN_MAP_HOST_MEMORY
             :CU_DEVICE_ATTRIBUTE_COMPUTE_MODE
             :CU_DEVICE_ATTRIBUTE_CONCURRENT_KERNELS
             :CU_DEVICE_ATTRIBUTE_ECC_ENABLED
             :CU_DEVICE_ATTRIBUTE_PCI_BUS_ID
             :CU_DEVICE_ATTRIBUTE_PCI_DEVICE_ID
             :CU_DEVICE_ATTRIBUTE_TCC_DRIVER
             :CU_DEVICE_ATTRIBUTE_MEMORY_CLOCK_RATE
             :CU_DEVICE_ATTRIBUTE_GLOBAL_MEMORY_BUS_WIDTH
             :CU_DEVICE_ATTRIBUTE_L2_CACHE_SIZE
             :CU_DEVICE_ATTRIBUTE_MAX_THREADS_PER_MULTIPROCESSOR
             :CU_DEVICE_ATTRIBUTE_UNIFIED_ADDRESSING]))
   :cuda/api-version
   (tf/fm
    [:cuda/context]
    (let [v (int-array 1)]
      (jcuda.driver.JCudaDriver/cuCtxGetApiVersion ^jcuda.driver.CUcontext context v)
      (first v)))
   :cuda/driver-version
   (tf/fm
    [:cuda/context]
    (let [v (int-array 1)]
      (jcuda.driver.JCudaDriver/cuDriverGetVersion v)
      (first v)))
   :cuda/cache-config
   (tf/fm
    []
    (let [v (int-array 1)]
      (jcuda.driver.JCudaDriver/cuCtxGetCacheConfig v)
      (first v)))
   ;; a context represents all the state for a GPU process, ie memory
   ;; map, allocations, and kernel definitions
   :cuda/context
   (tf/fm
    [:cuda/device]
    (doto (jcuda.driver.CUcontext.)
      ;; 0 is default scheduling mode, see documentation for full
      ;; explanation
      (jcuda.driver.JCudaDriver/cuCtxCreate 0 ^jcuda.driver.CUdevice device)))
   ;; I think :cuda/module and :cuda/fn have an implicit :cuda/context dependency
   :cuda/module
   (tf/fm
    [:cuda.compile/cubin :cuda/context]
    (doto (jcuda.driver.CUmodule.)
      (jcuda.driver.JCudaDriver/cuModuleLoadData ^bytes cubin)))

   ;; make sure we grab the meta data for debugging purposes
   :cuda/meta
   (tf/fm
    [:cuda/cache-config
     :cuda/api-version
     :cuda/driver-version
     :cuda/mem-info
     :cuda/device-name
     :cuda/device-count
     :cuda/device-attributes]
    :stub)

   :cuda/fn
   (tf/fm
    [:simulations/kernel-name :cuda/module :cuda/meta]
    (doto (jcuda.driver.CUfunction.)
      (jcuda.driver.JCudaDriver/cuModuleGetFunction module kernel-name)))

   :cuda/stream
   (tf/fm
    [:cuda/context]
    (doto (jcuda.driver.CUstream.)
      (jcuda.driver.JCudaDriver/cuStreamCreate jcuda.driver.CUstream_flags/CU_STREAM_DEFAULT)))})

;; We will need to make a few more abstractions before we write the
;; setup code for calling kernels. The jcuda pointer constructors are
;; weird in that they use java variadics. Having a pointer to multiple
;; pointers is actually a pointer to an array of pointers. I make it
;; explicit whether you are pointing to an array or a pointer to a
;; pointer.
(defn ptr->ptr [ptr]
  (jcuda.Pointer/to (into-array (list ptr))))

(defn ptr->arr [arr]
  (jcuda.Pointer/to arr))

(defn kernel-arguments
  "convert arrays into a data structure that is acceptable to
  cuLaunchKernel"
  [arrays]
  (ptr->arr (into-array (map ptr->ptr arrays))))

;; input: kernel invoke parameters
;; output boundary: kernel invocation
(def invoke-kernel-fns
  {:cuda/invoke-kernel
   (tf/fm
    [:cuda/fn

     ;; grid-dim is the number of blocks (simulations) we will be executing for this job
     :cuda/grid-dim-x 
     :cuda/grid-dim-y
     :cuda/grid-dim-z

     ;; block-dim is the size of a simulation
     :cuda/block-dim-x
     :cuda/block-dim-y
     :cuda/block-dim-z
     :cuda/shared-memory-size
     :cuda/stream
     :cuda/kernel-parameters
     :cuda/extra]
    (jcuda.driver.JCudaDriver/cuLaunchKernel
     ^jcuda.driver.CUfunction fn
     (int grid-dim-x)
     (int grid-dim-y)
     (int grid-dim-z)
     (int block-dim-x)
     (int block-dim-y)
     (int block-dim-z)
     (int shared-memory-size)
     ^jcuda.driver.CUstream stream
     ^jcuda.Pointer (when kernel-parameters
                      (kernel-arguments kernel-parameters))
     ^jcuda.Pointer extra))})

;; calling kernel thus requires at minimum
;; :cuda/fn
;; :cuda/grid-dim-x
;; :cuda/block-dim-x

;; unused
(defn reset-cuda []
  (jcuda.runtime.JCuda/cudaDeviceReset))

;;; Possible keys
;; num-nodes (number of nodes in the model)

;; Note that we have two time steps to seperate numerical and analyzing issues
;; dt (the solve dt, care must be taken to for using up too much precision)
;; record-dt (the resolution we output the model at)
;; end-time (in ms)
;; kernel-name (the name of the kernel function)
;; model-file (the name of the .model file to parse)

;; enhance-precision (currently only :all is supported, this tells the
;; compiler whether the precision trick should be used. This has
;; performance implications since the method uses more memory.

;; cuda-device-id (which cuda device to use)

;; REST-ptx-compiler (if enabled it will use the host's service to
;; compile the ptx file)

;; nvcc-path (path to nvidia compiler, windows usually has it in path)
;; ptxas-path (path to nvidia ptx only compiler, windows usually has it in path)

;; example configs (out of date right now)
#_ (def example-profile
     {:num-nodes 1
      :dt 0.01
      :record-dt 10
      :end-time 1000
      :kernel-name "mitomodel"
      :model-file (dj.io/file "dj/cuda/models/mitomodel.c")
      :enhance-precision :all
      :cuda-device-id 0
      :ptxas-path "/usr/local/cuda/bin/ptxas"
      :nvcc-path "/usr/local/cuda/bin/nvcc"})
