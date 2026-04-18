#ifndef XTMWEBGPU_H
#define XTMWEBGPU_H

#include <webgpu/webgpu.h>

#ifdef _WIN32
#define XTMWGPU_EXPORT __declspec(dllexport)
#else
#define XTMWGPU_EXPORT __attribute__((visibility("default")))
#endif

#ifdef __cplusplus
extern "C" {
#endif

struct GLFWwindow;

XTMWGPU_EXPORT WGPUSurface xtm_wgpu_create_surface(WGPUInstance instance,
                                                     struct GLFWwindow *window);

XTMWGPU_EXPORT WGPUAdapter xtm_wgpu_request_adapter(WGPUInstance instance,
                                                      WGPUSurface surface);

XTMWGPU_EXPORT WGPUDevice xtm_wgpu_request_device(WGPUInstance instance,
                                                    WGPUAdapter adapter);

XTMWGPU_EXPORT void xtm_wgpu_configure_surface(WGPUSurface surface,
                                                 WGPUDevice device,
                                                 uint32_t format,
                                                 struct GLFWwindow *window);

XTMWGPU_EXPORT uint32_t xtm_wgpu_surface_format(WGPUSurface surface,
                                                  WGPUAdapter adapter);

XTMWGPU_EXPORT WGPUShaderModule xtm_wgpu_create_shader(WGPUDevice device,
                                                         const char *wgsl);

XTMWGPU_EXPORT WGPURenderPipeline xtm_wgpu_create_pipeline(
    WGPUDevice device, WGPUShaderModule shader, uint32_t format,
    const char *vs_entry, const char *fs_entry);

XTMWGPU_EXPORT void xtm_wgpu_begin_frame(WGPUSurface surface,
                                           WGPUDevice device, double r,
                                           double g, double b, double a,
                                           WGPUCommandEncoder *out_encoder,
                                           WGPURenderPassEncoder *out_pass,
                                           WGPUTextureView *out_view);

XTMWGPU_EXPORT void xtm_wgpu_end_frame(WGPUSurface surface, WGPUQueue queue,
                                         WGPUCommandEncoder encoder,
                                         WGPURenderPassEncoder pass,
                                         WGPUTextureView view);

XTMWGPU_EXPORT WGPUBuffer xtm_wgpu_create_buffer(WGPUDevice device,
                                                    uint64_t size,
                                                    uint32_t usage);

XTMWGPU_EXPORT WGPUBindGroupLayout
xtm_wgpu_create_uniform_layout(WGPUDevice device);

XTMWGPU_EXPORT WGPUBindGroup
xtm_wgpu_create_uniform_bind_group(WGPUDevice device,
                                    WGPUBindGroupLayout layout,
                                    WGPUBuffer buffer, uint64_t size);

XTMWGPU_EXPORT WGPURenderPipeline xtm_wgpu_create_pipeline_with_layout(
    WGPUDevice device, WGPUShaderModule shader, uint32_t format,
    const char *vs_entry, const char *fs_entry,
    WGPUBindGroupLayout bind_group_layout);

#ifdef __cplusplus
}
#endif

#endif
