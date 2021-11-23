with Interfaces.C; use Interfaces.C;
with stdint_h;
with System;
with crtdefs_h;
with Interfaces.C.Strings;
with Interfaces.C.Extensions;

package Vulkan.Core.Binding is
    pragma Preelaborate;

   --  unsupported macro: VULKAN_CORE_H_ 1
   --  unsupported macro: VK_VERSION_1_0 1
   --  unsupported macro: VK_DEFINE_HANDLE(object) typedef struct object ##_T* object;
   --  unsupported macro: VK_DEFINE_NON_DISPATCHABLE_HANDLE(object) typedef struct object ##_T *object;
   --  arg-macro: function VK_MAKE_VERSION ((((uint32_t)(major)) << 22) or (((uint32_t)(minor)) << 12) or ((uint32_t)(patch))
   --    return (((uint32_t)(major)) << 22) or (((uint32_t)(minor)) << 12) or ((uint32_t)(patch));
   --  unsupported macro: VK_API_VERSION_1_0 VK_MAKE_VERSION(1, 0, 0)
   --  unsupported macro: VK_HEADER_VERSION 170
   --  unsupported macro: VK_HEADER_VERSION_COMPLETE VK_MAKE_VERSION(1, 2, VK_HEADER_VERSION)
   --  arg-macro: function VK_VERSION_MAJOR ((uint32_t)(version) >> 22
   --    return (uint32_t)(version) >> 22;
   --  arg-macro: function VK_VERSION_MINOR (((uint32_t)(version) >> 12) and 0x3ff
   --    return ((uint32_t)(version) >> 12) and 0x3ff;
   --  arg-macro: function VK_VERSION_PATCH ((uint32_t)(version) and 0xfff
   --    return (uint32_t)(version) and 0xfff;
   --  unsupported macro: VK_NULL_HANDLE 0
   --  unsupported macro: VK_ATTACHMENT_UNUSED (~0U)
   --  unsupported macro: VK_FALSE 0
   --  unsupported macro: VK_LOD_CLAMP_NONE 1000.0f
   --  unsupported macro: VK_QUEUE_FAMILY_IGNORED (~0U)
   --  unsupported macro: VK_REMAINING_ARRAY_LAYERS (~0U)
   --  unsupported macro: VK_REMAINING_MIP_LEVELS (~0U)
   --  unsupported macro: VK_SUBPASS_EXTERNAL (~0U)
   --  unsupported macro: VK_TRUE 1
   --  unsupported macro: VK_WHOLE_SIZE (~0ULL)
   --  unsupported macro: VK_MAX_MEMORY_TYPES 32
   --  unsupported macro: VK_MAX_MEMORY_HEAPS 16
   --  unsupported macro: VK_MAX_PHYSICAL_DEVICE_NAME_SIZE 256
   --  unsupported macro: VK_UUID_SIZE 16
   --  unsupported macro: VK_MAX_EXTENSION_NAME_SIZE 256
   --  unsupported macro: VK_MAX_DESCRIPTION_SIZE 256
   --  unsupported macro: VK_VERSION_1_1 1
   --  unsupported macro: VK_API_VERSION_1_1 VK_MAKE_VERSION(1, 1, 0)
   --  unsupported macro: VK_MAX_DEVICE_GROUP_SIZE 32
   --  unsupported macro: VK_LUID_SIZE 8
   --  unsupported macro: VK_QUEUE_FAMILY_EXTERNAL (~0U-1)
   --  unsupported macro: VK_VERSION_1_2 1
   --  unsupported macro: VK_API_VERSION_1_2 VK_MAKE_VERSION(1, 2, 0)
   --  unsupported macro: VK_MAX_DRIVER_NAME_SIZE 256
   --  unsupported macro: VK_MAX_DRIVER_INFO_SIZE 256
   --  unsupported macro: VK_KHR_surface 1
   --  unsupported macro: VK_KHR_SURFACE_SPEC_VERSION 25
   --  unsupported macro: VK_KHR_SURFACE_EXTENSION_NAME "VK_KHR_surface"
   --  unsupported macro: VK_KHR_swapchain 1
   --  unsupported macro: VK_KHR_SWAPCHAIN_SPEC_VERSION 70
   --  unsupported macro: VK_KHR_SWAPCHAIN_EXTENSION_NAME "VK_KHR_swapchain"
   --  unsupported macro: VK_KHR_display 1
   --  unsupported macro: VK_KHR_DISPLAY_SPEC_VERSION 23
   --  unsupported macro: VK_KHR_DISPLAY_EXTENSION_NAME "VK_KHR_display"
   --  unsupported macro: VK_KHR_display_swapchain 1
   --  unsupported macro: VK_KHR_DISPLAY_SWAPCHAIN_SPEC_VERSION 10
   --  unsupported macro: VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME "VK_KHR_display_swapchain"
   --  unsupported macro: VK_KHR_sampler_mirror_clamp_to_edge 1
   --  unsupported macro: VK_KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_SPEC_VERSION 3
   --  unsupported macro: VK_KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_EXTENSION_NAME "VK_KHR_sampler_mirror_clamp_to_edge"
   --  unsupported macro: VK_KHR_multiview 1
   --  unsupported macro: VK_KHR_MULTIVIEW_SPEC_VERSION 1
   --  unsupported macro: VK_KHR_MULTIVIEW_EXTENSION_NAME "VK_KHR_multiview"
   --  unsupported macro: VK_KHR_get_physical_device_properties2 1
   --  unsupported macro: VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_SPEC_VERSION 2
   --  unsupported macro: VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME "VK_KHR_get_physical_device_properties2"
   --  unsupported macro: VK_KHR_device_group 1
   --  unsupported macro: VK_KHR_DEVICE_GROUP_SPEC_VERSION 4
   --  unsupported macro: VK_KHR_DEVICE_GROUP_EXTENSION_NAME "VK_KHR_device_group"
   --  unsupported macro: VK_KHR_shader_draw_parameters 1
   --  unsupported macro: VK_KHR_SHADER_DRAW_PARAMETERS_SPEC_VERSION 1
   --  unsupported macro: VK_KHR_SHADER_DRAW_PARAMETERS_EXTENSION_NAME "VK_KHR_shader_draw_parameters"
   --  unsupported macro: VK_KHR_maintenance1 1
   --  unsupported macro: VK_KHR_MAINTENANCE1_SPEC_VERSION 2
   --  unsupported macro: VK_KHR_MAINTENANCE1_EXTENSION_NAME "VK_KHR_maintenance1"
   --  unsupported macro: VK_KHR_device_group_creation 1
   --  unsupported macro: VK_KHR_DEVICE_GROUP_CREATION_SPEC_VERSION 1
   --  unsupported macro: VK_KHR_DEVICE_GROUP_CREATION_EXTENSION_NAME "VK_KHR_device_group_creation"
   --  unsupported macro: VK_MAX_DEVICE_GROUP_SIZE_KHR VK_MAX_DEVICE_GROUP_SIZE
   --  unsupported macro: VK_KHR_external_memory_capabilities 1
   --  unsupported macro: VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION 1
   --  unsupported macro: VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME "VK_KHR_external_memory_capabilities"
   --  unsupported macro: VK_LUID_SIZE_KHR VK_LUID_SIZE
   --  unsupported macro: VK_KHR_external_memory 1
   --  unsupported macro: VK_KHR_EXTERNAL_MEMORY_SPEC_VERSION 1
   --  unsupported macro: VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME "VK_KHR_external_memory"
   --  unsupported macro: VK_QUEUE_FAMILY_EXTERNAL_KHR VK_QUEUE_FAMILY_EXTERNAL
   --  unsupported macro: VK_KHR_external_memory_fd 1
   --  unsupported macro: VK_KHR_EXTERNAL_MEMORY_FD_SPEC_VERSION 1
   --  unsupported macro: VK_KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME "VK_KHR_external_memory_fd"
   --  unsupported macro: VK_KHR_external_semaphore_capabilities 1
   --  unsupported macro: VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_SPEC_VERSION 1
   --  unsupported macro: VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_EXTENSION_NAME "VK_KHR_external_semaphore_capabilities"
   --  unsupported macro: VK_KHR_external_semaphore 1
   --  unsupported macro: VK_KHR_EXTERNAL_SEMAPHORE_SPEC_VERSION 1
   --  unsupported macro: VK_KHR_EXTERNAL_SEMAPHORE_EXTENSION_NAME "VK_KHR_external_semaphore"
   --  unsupported macro: VK_KHR_external_semaphore_fd 1
   --  unsupported macro: VK_KHR_EXTERNAL_SEMAPHORE_FD_SPEC_VERSION 1
   --  unsupported macro: VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME "VK_KHR_external_semaphore_fd"
   --  unsupported macro: VK_KHR_push_descriptor 1
   --  unsupported macro: VK_KHR_PUSH_DESCRIPTOR_SPEC_VERSION 2
   --  unsupported macro: VK_KHR_PUSH_DESCRIPTOR_EXTENSION_NAME "VK_KHR_push_descriptor"
   --  unsupported macro: VK_KHR_shader_float16_int8 1
   --  unsupported macro: VK_KHR_SHADER_FLOAT16_INT8_SPEC_VERSION 1
   --  unsupported macro: VK_KHR_SHADER_FLOAT16_INT8_EXTENSION_NAME "VK_KHR_shader_float16_int8"
   --  unsupported macro: VK_KHR_16bit_storage 1
   --  unsupported macro: VK_KHR_16BIT_STORAGE_SPEC_VERSION 1
   --  unsupported macro: VK_KHR_16BIT_STORAGE_EXTENSION_NAME "VK_KHR_16bit_storage"
   --  unsupported macro: VK_KHR_incremental_present 1
   --  unsupported macro: VK_KHR_INCREMENTAL_PRESENT_SPEC_VERSION 1
   --  unsupported macro: VK_KHR_INCREMENTAL_PRESENT_EXTENSION_NAME "VK_KHR_incremental_present"
   --  unsupported macro: VK_KHR_descriptor_update_template 1
   --  unsupported macro: VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_SPEC_VERSION 1
   --  unsupported macro: VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME "VK_KHR_descriptor_update_template"
   --  unsupported macro: VK_KHR_imageless_framebuffer 1
   --  unsupported macro: VK_KHR_IMAGELESS_FRAMEBUFFER_SPEC_VERSION 1
   --  unsupported macro: VK_KHR_IMAGELESS_FRAMEBUFFER_EXTENSION_NAME "VK_KHR_imageless_framebuffer"
   --  unsupported macro: VK_KHR_create_renderpass2 1
   --  unsupported macro: VK_KHR_CREATE_RENDERPASS_2_SPEC_VERSION 1
   --  unsupported macro: VK_KHR_CREATE_RENDERPASS_2_EXTENSION_NAME "VK_KHR_create_renderpass2"
   --  unsupported macro: VK_KHR_shared_presentable_image 1
   --  unsupported macro: VK_KHR_SHARED_PRESENTABLE_IMAGE_SPEC_VERSION 1
   --  unsupported macro: VK_KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME "VK_KHR_shared_presentable_image"
   --  unsupported macro: VK_KHR_external_fence_capabilities 1
   --  unsupported macro: VK_KHR_EXTERNAL_FENCE_CAPABILITIES_SPEC_VERSION 1
   --  unsupported macro: VK_KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME "VK_KHR_external_fence_capabilities"
   --  unsupported macro: VK_KHR_external_fence 1
   --  unsupported macro: VK_KHR_EXTERNAL_FENCE_SPEC_VERSION 1
   --  unsupported macro: VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME "VK_KHR_external_fence"
   --  unsupported macro: VK_KHR_external_fence_fd 1
   --  unsupported macro: VK_KHR_EXTERNAL_FENCE_FD_SPEC_VERSION 1
   --  unsupported macro: VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME "VK_KHR_external_fence_fd"
   --  unsupported macro: VK_KHR_performance_query 1
   --  unsupported macro: VK_KHR_PERFORMANCE_QUERY_SPEC_VERSION 1
   --  unsupported macro: VK_KHR_PERFORMANCE_QUERY_EXTENSION_NAME "VK_KHR_performance_query"
   --  unsupported macro: VK_KHR_maintenance2 1
   --  unsupported macro: VK_KHR_MAINTENANCE2_SPEC_VERSION 1
   --  unsupported macro: VK_KHR_MAINTENANCE2_EXTENSION_NAME "VK_KHR_maintenance2"
   --  unsupported macro: VK_KHR_get_surface_capabilities2 1
   --  unsupported macro: VK_KHR_GET_SURFACE_CAPABILITIES_2_SPEC_VERSION 1
   --  unsupported macro: VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME "VK_KHR_get_surface_capabilities2"
   --  unsupported macro: VK_KHR_variable_pointers 1
   --  unsupported macro: VK_KHR_VARIABLE_POINTERS_SPEC_VERSION 1
   --  unsupported macro: VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME "VK_KHR_variable_pointers"
   --  unsupported macro: VK_KHR_get_display_properties2 1
   --  unsupported macro: VK_KHR_GET_DISPLAY_PROPERTIES_2_SPEC_VERSION 1
   --  unsupported macro: VK_KHR_GET_DISPLAY_PROPERTIES_2_EXTENSION_NAME "VK_KHR_get_display_properties2"
   --  unsupported macro: VK_KHR_dedicated_allocation 1
   --  unsupported macro: VK_KHR_DEDICATED_ALLOCATION_SPEC_VERSION 3
   --  unsupported macro: VK_KHR_DEDICATED_ALLOCATION_EXTENSION_NAME "VK_KHR_dedicated_allocation"
   --  unsupported macro: VK_KHR_storage_buffer_storage_class 1
   --  unsupported macro: VK_KHR_STORAGE_BUFFER_STORAGE_CLASS_SPEC_VERSION 1
   --  unsupported macro: VK_KHR_STORAGE_BUFFER_STORAGE_CLASS_EXTENSION_NAME "VK_KHR_storage_buffer_storage_class"
   --  unsupported macro: VK_KHR_relaxed_block_layout 1
   --  unsupported macro: VK_KHR_RELAXED_BLOCK_LAYOUT_SPEC_VERSION 1
   --  unsupported macro: VK_KHR_RELAXED_BLOCK_LAYOUT_EXTENSION_NAME "VK_KHR_relaxed_block_layout"
   --  unsupported macro: VK_KHR_get_memory_requirements2 1
   --  unsupported macro: VK_KHR_GET_MEMORY_REQUIREMENTS_2_SPEC_VERSION 1
   --  unsupported macro: VK_KHR_GET_MEMORY_REQUIREMENTS_2_EXTENSION_NAME "VK_KHR_get_memory_requirements2"
   --  unsupported macro: VK_KHR_image_format_list 1
   --  unsupported macro: VK_KHR_IMAGE_FORMAT_LIST_SPEC_VERSION 1
   --  unsupported macro: VK_KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME "VK_KHR_image_format_list"
   --  unsupported macro: VK_KHR_sampler_ycbcr_conversion 1
   --  unsupported macro: VK_KHR_SAMPLER_YCBCR_CONVERSION_SPEC_VERSION 14
   --  unsupported macro: VK_KHR_SAMPLER_YCBCR_CONVERSION_EXTENSION_NAME "VK_KHR_sampler_ycbcr_conversion"
   --  unsupported macro: VK_KHR_bind_memory2 1
   --  unsupported macro: VK_KHR_BIND_MEMORY_2_SPEC_VERSION 1
   --  unsupported macro: VK_KHR_BIND_MEMORY_2_EXTENSION_NAME "VK_KHR_bind_memory2"
   --  unsupported macro: VK_KHR_maintenance3 1
   --  unsupported macro: VK_KHR_MAINTENANCE3_SPEC_VERSION 1
   --  unsupported macro: VK_KHR_MAINTENANCE3_EXTENSION_NAME "VK_KHR_maintenance3"
   --  unsupported macro: VK_KHR_draw_indirect_count 1
   --  unsupported macro: VK_KHR_DRAW_INDIRECT_COUNT_SPEC_VERSION 1
   --  unsupported macro: VK_KHR_DRAW_INDIRECT_COUNT_EXTENSION_NAME "VK_KHR_draw_indirect_count"
   --  unsupported macro: VK_KHR_shader_subgroup_extended_types 1
   --  unsupported macro: VK_KHR_SHADER_SUBGROUP_EXTENDED_TYPES_SPEC_VERSION 1
   --  unsupported macro: VK_KHR_SHADER_SUBGROUP_EXTENDED_TYPES_EXTENSION_NAME "VK_KHR_shader_subgroup_extended_types"
   --  unsupported macro: VK_KHR_8bit_storage 1
   --  unsupported macro: VK_KHR_8BIT_STORAGE_SPEC_VERSION 1
   --  unsupported macro: VK_KHR_8BIT_STORAGE_EXTENSION_NAME "VK_KHR_8bit_storage"
   --  unsupported macro: VK_KHR_shader_atomic_int64 1
   --  unsupported macro: VK_KHR_SHADER_ATOMIC_INT64_SPEC_VERSION 1
   --  unsupported macro: VK_KHR_SHADER_ATOMIC_INT64_EXTENSION_NAME "VK_KHR_shader_atomic_int64"
   --  unsupported macro: VK_KHR_shader_clock 1
   --  unsupported macro: VK_KHR_SHADER_CLOCK_SPEC_VERSION 1
   --  unsupported macro: VK_KHR_SHADER_CLOCK_EXTENSION_NAME "VK_KHR_shader_clock"
   --  unsupported macro: VK_KHR_driver_properties 1
   --  unsupported macro: VK_KHR_DRIVER_PROPERTIES_SPEC_VERSION 1
   --  unsupported macro: VK_KHR_DRIVER_PROPERTIES_EXTENSION_NAME "VK_KHR_driver_properties"
   --  unsupported macro: VK_MAX_DRIVER_NAME_SIZE_KHR VK_MAX_DRIVER_NAME_SIZE
   --  unsupported macro: VK_MAX_DRIVER_INFO_SIZE_KHR VK_MAX_DRIVER_INFO_SIZE
   --  unsupported macro: VK_KHR_shader_float_controls 1
   --  unsupported macro: VK_KHR_SHADER_FLOAT_CONTROLS_SPEC_VERSION 4
   --  unsupported macro: VK_KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME "VK_KHR_shader_float_controls"
   --  unsupported macro: VK_KHR_depth_stencil_resolve 1
   --  unsupported macro: VK_KHR_DEPTH_STENCIL_RESOLVE_SPEC_VERSION 1
   --  unsupported macro: VK_KHR_DEPTH_STENCIL_RESOLVE_EXTENSION_NAME "VK_KHR_depth_stencil_resolve"
   --  unsupported macro: VK_KHR_swapchain_mutable_format 1
   --  unsupported macro: VK_KHR_SWAPCHAIN_MUTABLE_FORMAT_SPEC_VERSION 1
   --  unsupported macro: VK_KHR_SWAPCHAIN_MUTABLE_FORMAT_EXTENSION_NAME "VK_KHR_swapchain_mutable_format"
   --  unsupported macro: VK_KHR_timeline_semaphore 1
   --  unsupported macro: VK_KHR_TIMELINE_SEMAPHORE_SPEC_VERSION 2
   --  unsupported macro: VK_KHR_TIMELINE_SEMAPHORE_EXTENSION_NAME "VK_KHR_timeline_semaphore"
   --  unsupported macro: VK_KHR_vulkan_memory_model 1
   --  unsupported macro: VK_KHR_VULKAN_MEMORY_MODEL_SPEC_VERSION 3
   --  unsupported macro: VK_KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME "VK_KHR_vulkan_memory_model"
   --  unsupported macro: VK_KHR_shader_terminate_invocation 1
   --  unsupported macro: VK_KHR_SHADER_TERMINATE_INVOCATION_SPEC_VERSION 1
   --  unsupported macro: VK_KHR_SHADER_TERMINATE_INVOCATION_EXTENSION_NAME "VK_KHR_shader_terminate_invocation"
   --  unsupported macro: VK_KHR_fragment_shading_rate 1
   --  unsupported macro: VK_KHR_FRAGMENT_SHADING_RATE_SPEC_VERSION 1
   --  unsupported macro: VK_KHR_FRAGMENT_SHADING_RATE_EXTENSION_NAME "VK_KHR_fragment_shading_rate"
   --  unsupported macro: VK_KHR_spirv_1_4 1
   --  unsupported macro: VK_KHR_SPIRV_1_4_SPEC_VERSION 1
   --  unsupported macro: VK_KHR_SPIRV_1_4_EXTENSION_NAME "VK_KHR_spirv_1_4"
   --  unsupported macro: VK_KHR_surface_protected_capabilities 1
   --  unsupported macro: VK_KHR_SURFACE_PROTECTED_CAPABILITIES_SPEC_VERSION 1
   --  unsupported macro: VK_KHR_SURFACE_PROTECTED_CAPABILITIES_EXTENSION_NAME "VK_KHR_surface_protected_capabilities"
   --  unsupported macro: VK_KHR_separate_depth_stencil_layouts 1
   --  unsupported macro: VK_KHR_SEPARATE_DEPTH_STENCIL_LAYOUTS_SPEC_VERSION 1
   --  unsupported macro: VK_KHR_SEPARATE_DEPTH_STENCIL_LAYOUTS_EXTENSION_NAME "VK_KHR_separate_depth_stencil_layouts"
   --  unsupported macro: VK_KHR_uniform_buffer_standard_layout 1
   --  unsupported macro: VK_KHR_UNIFORM_BUFFER_STANDARD_LAYOUT_SPEC_VERSION 1
   --  unsupported macro: VK_KHR_UNIFORM_BUFFER_STANDARD_LAYOUT_EXTENSION_NAME "VK_KHR_uniform_buffer_standard_layout"
   --  unsupported macro: VK_KHR_buffer_device_address 1
   --  unsupported macro: VK_KHR_BUFFER_DEVICE_ADDRESS_SPEC_VERSION 1
   --  unsupported macro: VK_KHR_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME "VK_KHR_buffer_device_address"
   --  unsupported macro: VK_KHR_deferred_host_operations 1
   --  unsupported macro: VK_KHR_DEFERRED_HOST_OPERATIONS_SPEC_VERSION 4
   --  unsupported macro: VK_KHR_DEFERRED_HOST_OPERATIONS_EXTENSION_NAME "VK_KHR_deferred_host_operations"
   --  unsupported macro: VK_KHR_pipeline_executable_properties 1
   --  unsupported macro: VK_KHR_PIPELINE_EXECUTABLE_PROPERTIES_SPEC_VERSION 1
   --  unsupported macro: VK_KHR_PIPELINE_EXECUTABLE_PROPERTIES_EXTENSION_NAME "VK_KHR_pipeline_executable_properties"
   --  unsupported macro: VK_KHR_pipeline_library 1
   --  unsupported macro: VK_KHR_PIPELINE_LIBRARY_SPEC_VERSION 1
   --  unsupported macro: VK_KHR_PIPELINE_LIBRARY_EXTENSION_NAME "VK_KHR_pipeline_library"
   --  unsupported macro: VK_KHR_shader_non_semantic_info 1
   --  unsupported macro: VK_KHR_SHADER_NON_SEMANTIC_INFO_SPEC_VERSION 1
   --  unsupported macro: VK_KHR_SHADER_NON_SEMANTIC_INFO_EXTENSION_NAME "VK_KHR_shader_non_semantic_info"
   --  unsupported macro: VK_KHR_synchronization2 1
   --  unsupported macro: VK_KHR_SYNCHRONIZATION_2_SPEC_VERSION 1
   --  unsupported macro: VK_KHR_SYNCHRONIZATION_2_EXTENSION_NAME "VK_KHR_synchronization2"
   --  unsupported macro: VK_KHR_zero_initialize_workgroup_memory 1
   --  unsupported macro: VK_KHR_ZERO_INITIALIZE_WORKGROUP_MEMORY_SPEC_VERSION 1
   --  unsupported macro: VK_KHR_ZERO_INITIALIZE_WORKGROUP_MEMORY_EXTENSION_NAME "VK_KHR_zero_initialize_workgroup_memory"
   --  unsupported macro: VK_KHR_workgroup_memory_explicit_layout 1
   --  unsupported macro: VK_KHR_WORKGROUP_MEMORY_EXPLICIT_LAYOUT_SPEC_VERSION 1
   --  unsupported macro: VK_KHR_WORKGROUP_MEMORY_EXPLICIT_LAYOUT_EXTENSION_NAME "VK_KHR_workgroup_memory_explicit_layout"
   --  unsupported macro: VK_KHR_copy_commands2 1
   --  unsupported macro: VK_KHR_COPY_COMMANDS_2_SPEC_VERSION 1
   --  unsupported macro: VK_KHR_COPY_COMMANDS_2_EXTENSION_NAME "VK_KHR_copy_commands2"
   --  unsupported macro: VK_EXT_debug_report 1
   --  unsupported macro: VK_EXT_DEBUG_REPORT_SPEC_VERSION 9
   --  unsupported macro: VK_EXT_DEBUG_REPORT_EXTENSION_NAME "VK_EXT_debug_report"
   --  unsupported macro: VK_NV_glsl_shader 1
   --  unsupported macro: VK_NV_GLSL_SHADER_SPEC_VERSION 1
   --  unsupported macro: VK_NV_GLSL_SHADER_EXTENSION_NAME "VK_NV_glsl_shader"
   --  unsupported macro: VK_EXT_depth_range_unrestricted 1
   --  unsupported macro: VK_EXT_DEPTH_RANGE_UNRESTRICTED_SPEC_VERSION 1
   --  unsupported macro: VK_EXT_DEPTH_RANGE_UNRESTRICTED_EXTENSION_NAME "VK_EXT_depth_range_unrestricted"
   --  unsupported macro: VK_IMG_filter_cubic 1
   --  unsupported macro: VK_IMG_FILTER_CUBIC_SPEC_VERSION 1
   --  unsupported macro: VK_IMG_FILTER_CUBIC_EXTENSION_NAME "VK_IMG_filter_cubic"
   --  unsupported macro: VK_AMD_rasterization_order 1
   --  unsupported macro: VK_AMD_RASTERIZATION_ORDER_SPEC_VERSION 1
   --  unsupported macro: VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME "VK_AMD_rasterization_order"
   --  unsupported macro: VK_AMD_shader_trinary_minmax 1
   --  unsupported macro: VK_AMD_SHADER_TRINARY_MINMAX_SPEC_VERSION 1
   --  unsupported macro: VK_AMD_SHADER_TRINARY_MINMAX_EXTENSION_NAME "VK_AMD_shader_trinary_minmax"
   --  unsupported macro: VK_AMD_shader_explicit_vertex_parameter 1
   --  unsupported macro: VK_AMD_SHADER_EXPLICIT_VERTEX_PARAMETER_SPEC_VERSION 1
   --  unsupported macro: VK_AMD_SHADER_EXPLICIT_VERTEX_PARAMETER_EXTENSION_NAME "VK_AMD_shader_explicit_vertex_parameter"
   --  unsupported macro: VK_EXT_debug_marker 1
   --  unsupported macro: VK_EXT_DEBUG_MARKER_SPEC_VERSION 4
   --  unsupported macro: VK_EXT_DEBUG_MARKER_EXTENSION_NAME "VK_EXT_debug_marker"
   --  unsupported macro: VK_AMD_gcn_shader 1
   --  unsupported macro: VK_AMD_GCN_SHADER_SPEC_VERSION 1
   --  unsupported macro: VK_AMD_GCN_SHADER_EXTENSION_NAME "VK_AMD_gcn_shader"
   --  unsupported macro: VK_NV_dedicated_allocation 1
   --  unsupported macro: VK_NV_DEDICATED_ALLOCATION_SPEC_VERSION 1
   --  unsupported macro: VK_NV_DEDICATED_ALLOCATION_EXTENSION_NAME "VK_NV_dedicated_allocation"
   --  unsupported macro: VK_EXT_transform_feedback 1
   --  unsupported macro: VK_EXT_TRANSFORM_FEEDBACK_SPEC_VERSION 1
   --  unsupported macro: VK_EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME "VK_EXT_transform_feedback"
   --  unsupported macro: VK_NVX_image_view_handle 1
   --  unsupported macro: VK_NVX_IMAGE_VIEW_HANDLE_SPEC_VERSION 2
   --  unsupported macro: VK_NVX_IMAGE_VIEW_HANDLE_EXTENSION_NAME "VK_NVX_image_view_handle"
   --  unsupported macro: VK_AMD_draw_indirect_count 1
   --  unsupported macro: VK_AMD_DRAW_INDIRECT_COUNT_SPEC_VERSION 2
   --  unsupported macro: VK_AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME "VK_AMD_draw_indirect_count"
   --  unsupported macro: VK_AMD_negative_viewport_height 1
   --  unsupported macro: VK_AMD_NEGATIVE_VIEWPORT_HEIGHT_SPEC_VERSION 1
   --  unsupported macro: VK_AMD_NEGATIVE_VIEWPORT_HEIGHT_EXTENSION_NAME "VK_AMD_negative_viewport_height"
   --  unsupported macro: VK_AMD_gpu_shader_half_float 1
   --  unsupported macro: VK_AMD_GPU_SHADER_HALF_FLOAT_SPEC_VERSION 2
   --  unsupported macro: VK_AMD_GPU_SHADER_HALF_FLOAT_EXTENSION_NAME "VK_AMD_gpu_shader_half_float"
   --  unsupported macro: VK_AMD_shader_ballot 1
   --  unsupported macro: VK_AMD_SHADER_BALLOT_SPEC_VERSION 1
   --  unsupported macro: VK_AMD_SHADER_BALLOT_EXTENSION_NAME "VK_AMD_shader_ballot"
   --  unsupported macro: VK_AMD_texture_gather_bias_lod 1
   --  unsupported macro: VK_AMD_TEXTURE_GATHER_BIAS_LOD_SPEC_VERSION 1
   --  unsupported macro: VK_AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME "VK_AMD_texture_gather_bias_lod"
   --  unsupported macro: VK_AMD_shader_info 1
   --  unsupported macro: VK_AMD_SHADER_INFO_SPEC_VERSION 1
   --  unsupported macro: VK_AMD_SHADER_INFO_EXTENSION_NAME "VK_AMD_shader_info"
   --  unsupported macro: VK_AMD_shader_image_load_store_lod 1
   --  unsupported macro: VK_AMD_SHADER_IMAGE_LOAD_STORE_LOD_SPEC_VERSION 1
   --  unsupported macro: VK_AMD_SHADER_IMAGE_LOAD_STORE_LOD_EXTENSION_NAME "VK_AMD_shader_image_load_store_lod"
   --  unsupported macro: VK_NV_corner_sampled_image 1
   --  unsupported macro: VK_NV_CORNER_SAMPLED_IMAGE_SPEC_VERSION 2
   --  unsupported macro: VK_NV_CORNER_SAMPLED_IMAGE_EXTENSION_NAME "VK_NV_corner_sampled_image"
   --  unsupported macro: VK_IMG_format_pvrtc 1
   --  unsupported macro: VK_IMG_FORMAT_PVRTC_SPEC_VERSION 1
   --  unsupported macro: VK_IMG_FORMAT_PVRTC_EXTENSION_NAME "VK_IMG_format_pvrtc"
   --  unsupported macro: VK_NV_external_memory_capabilities 1
   --  unsupported macro: VK_NV_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION 1
   --  unsupported macro: VK_NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME "VK_NV_external_memory_capabilities"
   --  unsupported macro: VK_NV_external_memory 1
   --  unsupported macro: VK_NV_EXTERNAL_MEMORY_SPEC_VERSION 1
   --  unsupported macro: VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME "VK_NV_external_memory"
   --  unsupported macro: VK_EXT_validation_flags 1
   --  unsupported macro: VK_EXT_VALIDATION_FLAGS_SPEC_VERSION 2
   --  unsupported macro: VK_EXT_VALIDATION_FLAGS_EXTENSION_NAME "VK_EXT_validation_flags"
   --  unsupported macro: VK_EXT_shader_subgroup_ballot 1
   --  unsupported macro: VK_EXT_SHADER_SUBGROUP_BALLOT_SPEC_VERSION 1
   --  unsupported macro: VK_EXT_SHADER_SUBGROUP_BALLOT_EXTENSION_NAME "VK_EXT_shader_subgroup_ballot"
   --  unsupported macro: VK_EXT_shader_subgroup_vote 1
   --  unsupported macro: VK_EXT_SHADER_SUBGROUP_VOTE_SPEC_VERSION 1
   --  unsupported macro: VK_EXT_SHADER_SUBGROUP_VOTE_EXTENSION_NAME "VK_EXT_shader_subgroup_vote"
   --  unsupported macro: VK_EXT_texture_compression_astc_hdr 1
   --  unsupported macro: VK_EXT_TEXTURE_COMPRESSION_ASTC_HDR_SPEC_VERSION 1
   --  unsupported macro: VK_EXT_TEXTURE_COMPRESSION_ASTC_HDR_EXTENSION_NAME "VK_EXT_texture_compression_astc_hdr"
   --  unsupported macro: VK_EXT_astc_decode_mode 1
   --  unsupported macro: VK_EXT_ASTC_DECODE_MODE_SPEC_VERSION 1
   --  unsupported macro: VK_EXT_ASTC_DECODE_MODE_EXTENSION_NAME "VK_EXT_astc_decode_mode"
   --  unsupported macro: VK_EXT_conditional_rendering 1
   --  unsupported macro: VK_EXT_CONDITIONAL_RENDERING_SPEC_VERSION 2
   --  unsupported macro: VK_EXT_CONDITIONAL_RENDERING_EXTENSION_NAME "VK_EXT_conditional_rendering"
   --  unsupported macro: VK_NV_clip_space_w_scaling 1
   --  unsupported macro: VK_NV_CLIP_SPACE_W_SCALING_SPEC_VERSION 1
   --  unsupported macro: VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME "VK_NV_clip_space_w_scaling"
   --  unsupported macro: VK_EXT_direct_mode_display 1
   --  unsupported macro: VK_EXT_DIRECT_MODE_DISPLAY_SPEC_VERSION 1
   --  unsupported macro: VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME "VK_EXT_direct_mode_display"
   --  unsupported macro: VK_EXT_display_surface_counter 1
   --  unsupported macro: VK_EXT_DISPLAY_SURFACE_COUNTER_SPEC_VERSION 1
   --  unsupported macro: VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME "VK_EXT_display_surface_counter"
   --  unsupported macro: VK_EXT_display_control 1
   --  unsupported macro: VK_EXT_DISPLAY_CONTROL_SPEC_VERSION 1
   --  unsupported macro: VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME "VK_EXT_display_control"
   --  unsupported macro: VK_GOOGLE_display_timing 1
   --  unsupported macro: VK_GOOGLE_DISPLAY_TIMING_SPEC_VERSION 1
   --  unsupported macro: VK_GOOGLE_DISPLAY_TIMING_EXTENSION_NAME "VK_GOOGLE_display_timing"
   --  unsupported macro: VK_NV_sample_mask_override_coverage 1
   --  unsupported macro: VK_NV_SAMPLE_MASK_OVERRIDE_COVERAGE_SPEC_VERSION 1
   --  unsupported macro: VK_NV_SAMPLE_MASK_OVERRIDE_COVERAGE_EXTENSION_NAME "VK_NV_sample_mask_override_coverage"
   --  unsupported macro: VK_NV_geometry_shader_passthrough 1
   --  unsupported macro: VK_NV_GEOMETRY_SHADER_PASSTHROUGH_SPEC_VERSION 1
   --  unsupported macro: VK_NV_GEOMETRY_SHADER_PASSTHROUGH_EXTENSION_NAME "VK_NV_geometry_shader_passthrough"
   --  unsupported macro: VK_NV_viewport_array2 1
   --  unsupported macro: VK_NV_VIEWPORT_ARRAY2_SPEC_VERSION 1
   --  unsupported macro: VK_NV_VIEWPORT_ARRAY2_EXTENSION_NAME "VK_NV_viewport_array2"
   --  unsupported macro: VK_NVX_multiview_per_view_attributes 1
   --  unsupported macro: VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_SPEC_VERSION 1
   --  unsupported macro: VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME "VK_NVX_multiview_per_view_attributes"
   --  unsupported macro: VK_NV_viewport_swizzle 1
   --  unsupported macro: VK_NV_VIEWPORT_SWIZZLE_SPEC_VERSION 1
   --  unsupported macro: VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME "VK_NV_viewport_swizzle"
   --  unsupported macro: VK_EXT_discard_rectangles 1
   --  unsupported macro: VK_EXT_DISCARD_RECTANGLES_SPEC_VERSION 1
   --  unsupported macro: VK_EXT_DISCARD_RECTANGLES_EXTENSION_NAME "VK_EXT_discard_rectangles"
   --  unsupported macro: VK_EXT_conservative_rasterization 1
   --  unsupported macro: VK_EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION 1
   --  unsupported macro: VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME "VK_EXT_conservative_rasterization"
   --  unsupported macro: VK_EXT_depth_clip_enable 1
   --  unsupported macro: VK_EXT_DEPTH_CLIP_ENABLE_SPEC_VERSION 1
   --  unsupported macro: VK_EXT_DEPTH_CLIP_ENABLE_EXTENSION_NAME "VK_EXT_depth_clip_enable"
   --  unsupported macro: VK_EXT_swapchain_colorspace 1
   --  unsupported macro: VK_EXT_SWAPCHAIN_COLOR_SPACE_SPEC_VERSION 4
   --  unsupported macro: VK_EXT_SWAPCHAIN_COLOR_SPACE_EXTENSION_NAME "VK_EXT_swapchain_colorspace"
   --  unsupported macro: VK_EXT_hdr_metadata 1
   --  unsupported macro: VK_EXT_HDR_METADATA_SPEC_VERSION 2
   --  unsupported macro: VK_EXT_HDR_METADATA_EXTENSION_NAME "VK_EXT_hdr_metadata"
   --  unsupported macro: VK_EXT_external_memory_dma_buf 1
   --  unsupported macro: VK_EXT_EXTERNAL_MEMORY_DMA_BUF_SPEC_VERSION 1
   --  unsupported macro: VK_EXT_EXTERNAL_MEMORY_DMA_BUF_EXTENSION_NAME "VK_EXT_external_memory_dma_buf"
   --  unsupported macro: VK_EXT_queue_family_foreign 1
   --  unsupported macro: VK_EXT_QUEUE_FAMILY_FOREIGN_SPEC_VERSION 1
   --  unsupported macro: VK_EXT_QUEUE_FAMILY_FOREIGN_EXTENSION_NAME "VK_EXT_queue_family_foreign"
   --  unsupported macro: VK_QUEUE_FAMILY_FOREIGN_EXT (~0U-2)
   --  unsupported macro: VK_EXT_debug_utils 1
   --  unsupported macro: VK_EXT_DEBUG_UTILS_SPEC_VERSION 2
   --  unsupported macro: VK_EXT_DEBUG_UTILS_EXTENSION_NAME "VK_EXT_debug_utils"
   --  unsupported macro: VK_EXT_sampler_filter_minmax 1
   --  unsupported macro: VK_EXT_SAMPLER_FILTER_MINMAX_SPEC_VERSION 2
   --  unsupported macro: VK_EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME "VK_EXT_sampler_filter_minmax"
   --  unsupported macro: VK_AMD_gpu_shader_int16 1
   --  unsupported macro: VK_AMD_GPU_SHADER_INT16_SPEC_VERSION 2
   --  unsupported macro: VK_AMD_GPU_SHADER_INT16_EXTENSION_NAME "VK_AMD_gpu_shader_int16"
   --  unsupported macro: VK_AMD_mixed_attachment_samples 1
   --  unsupported macro: VK_AMD_MIXED_ATTACHMENT_SAMPLES_SPEC_VERSION 1
   --  unsupported macro: VK_AMD_MIXED_ATTACHMENT_SAMPLES_EXTENSION_NAME "VK_AMD_mixed_attachment_samples"
   --  unsupported macro: VK_AMD_shader_fragment_mask 1
   --  unsupported macro: VK_AMD_SHADER_FRAGMENT_MASK_SPEC_VERSION 1
   --  unsupported macro: VK_AMD_SHADER_FRAGMENT_MASK_EXTENSION_NAME "VK_AMD_shader_fragment_mask"
   --  unsupported macro: VK_EXT_inline_uniform_block 1
   --  unsupported macro: VK_EXT_INLINE_UNIFORM_BLOCK_SPEC_VERSION 1
   --  unsupported macro: VK_EXT_INLINE_UNIFORM_BLOCK_EXTENSION_NAME "VK_EXT_inline_uniform_block"
   --  unsupported macro: VK_EXT_shader_stencil_export 1
   --  unsupported macro: VK_EXT_SHADER_STENCIL_EXPORT_SPEC_VERSION 1
   --  unsupported macro: VK_EXT_SHADER_STENCIL_EXPORT_EXTENSION_NAME "VK_EXT_shader_stencil_export"
   --  unsupported macro: VK_EXT_sample_locations 1
   --  unsupported macro: VK_EXT_SAMPLE_LOCATIONS_SPEC_VERSION 1
   --  unsupported macro: VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME "VK_EXT_sample_locations"
   --  unsupported macro: VK_EXT_blend_operation_advanced 1
   --  unsupported macro: VK_EXT_BLEND_OPERATION_ADVANCED_SPEC_VERSION 2
   --  unsupported macro: VK_EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME "VK_EXT_blend_operation_advanced"
   --  unsupported macro: VK_NV_fragment_coverage_to_color 1
   --  unsupported macro: VK_NV_FRAGMENT_COVERAGE_TO_COLOR_SPEC_VERSION 1
   --  unsupported macro: VK_NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME "VK_NV_fragment_coverage_to_color"
   --  unsupported macro: VK_NV_framebuffer_mixed_samples 1
   --  unsupported macro: VK_NV_FRAMEBUFFER_MIXED_SAMPLES_SPEC_VERSION 1
   --  unsupported macro: VK_NV_FRAMEBUFFER_MIXED_SAMPLES_EXTENSION_NAME "VK_NV_framebuffer_mixed_samples"
   --  unsupported macro: VK_NV_fill_rectangle 1
   --  unsupported macro: VK_NV_FILL_RECTANGLE_SPEC_VERSION 1
   --  unsupported macro: VK_NV_FILL_RECTANGLE_EXTENSION_NAME "VK_NV_fill_rectangle"
   --  unsupported macro: VK_NV_shader_sm_builtins 1
   --  unsupported macro: VK_NV_SHADER_SM_BUILTINS_SPEC_VERSION 1
   --  unsupported macro: VK_NV_SHADER_SM_BUILTINS_EXTENSION_NAME "VK_NV_shader_sm_builtins"
   --  unsupported macro: VK_EXT_post_depth_coverage 1
   --  unsupported macro: VK_EXT_POST_DEPTH_COVERAGE_SPEC_VERSION 1
   --  unsupported macro: VK_EXT_POST_DEPTH_COVERAGE_EXTENSION_NAME "VK_EXT_post_depth_coverage"
   --  unsupported macro: VK_EXT_image_drm_format_modifier 1
   --  unsupported macro: VK_EXT_IMAGE_DRM_FORMAT_MODIFIER_SPEC_VERSION 1
   --  unsupported macro: VK_EXT_IMAGE_DRM_FORMAT_MODIFIER_EXTENSION_NAME "VK_EXT_image_drm_format_modifier"
   --  unsupported macro: VK_EXT_validation_cache 1
   --  unsupported macro: VK_EXT_VALIDATION_CACHE_SPEC_VERSION 1
   --  unsupported macro: VK_EXT_VALIDATION_CACHE_EXTENSION_NAME "VK_EXT_validation_cache"
   --  unsupported macro: VK_EXT_descriptor_indexing 1
   --  unsupported macro: VK_EXT_DESCRIPTOR_INDEXING_SPEC_VERSION 2
   --  unsupported macro: VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME "VK_EXT_descriptor_indexing"
   --  unsupported macro: VK_EXT_shader_viewport_index_layer 1
   --  unsupported macro: VK_EXT_SHADER_VIEWPORT_INDEX_LAYER_SPEC_VERSION 1
   --  unsupported macro: VK_EXT_SHADER_VIEWPORT_INDEX_LAYER_EXTENSION_NAME "VK_EXT_shader_viewport_index_layer"
   --  unsupported macro: VK_NV_shading_rate_image 1
   --  unsupported macro: VK_NV_SHADING_RATE_IMAGE_SPEC_VERSION 3
   --  unsupported macro: VK_NV_SHADING_RATE_IMAGE_EXTENSION_NAME "VK_NV_shading_rate_image"
   --  unsupported macro: VK_NV_ray_tracing 1
   --  unsupported macro: VK_NV_RAY_TRACING_SPEC_VERSION 3
   --  unsupported macro: VK_NV_RAY_TRACING_EXTENSION_NAME "VK_NV_ray_tracing"
   --  unsupported macro: VK_SHADER_UNUSED_KHR (~0U)
   --  unsupported macro: VK_SHADER_UNUSED_NV VK_SHADER_UNUSED_KHR
   --  unsupported macro: VK_NV_representative_fragment_test 1
   --  unsupported macro: VK_NV_REPRESENTATIVE_FRAGMENT_TEST_SPEC_VERSION 2
   --  unsupported macro: VK_NV_REPRESENTATIVE_FRAGMENT_TEST_EXTENSION_NAME "VK_NV_representative_fragment_test"
   --  unsupported macro: VK_EXT_filter_cubic 1
   --  unsupported macro: VK_EXT_FILTER_CUBIC_SPEC_VERSION 3
   --  unsupported macro: VK_EXT_FILTER_CUBIC_EXTENSION_NAME "VK_EXT_filter_cubic"
   --  unsupported macro: VK_QCOM_render_pass_shader_resolve 1
   --  unsupported macro: VK_QCOM_RENDER_PASS_SHADER_RESOLVE_SPEC_VERSION 4
   --  unsupported macro: VK_QCOM_RENDER_PASS_SHADER_RESOLVE_EXTENSION_NAME "VK_QCOM_render_pass_shader_resolve"
   --  unsupported macro: VK_EXT_global_priority 1
   --  unsupported macro: VK_EXT_GLOBAL_PRIORITY_SPEC_VERSION 2
   --  unsupported macro: VK_EXT_GLOBAL_PRIORITY_EXTENSION_NAME "VK_EXT_global_priority"
   --  unsupported macro: VK_EXT_external_memory_host 1
   --  unsupported macro: VK_EXT_EXTERNAL_MEMORY_HOST_SPEC_VERSION 1
   --  unsupported macro: VK_EXT_EXTERNAL_MEMORY_HOST_EXTENSION_NAME "VK_EXT_external_memory_host"
   --  unsupported macro: VK_AMD_buffer_marker 1
   --  unsupported macro: VK_AMD_BUFFER_MARKER_SPEC_VERSION 1
   --  unsupported macro: VK_AMD_BUFFER_MARKER_EXTENSION_NAME "VK_AMD_buffer_marker"
   --  unsupported macro: VK_AMD_pipeline_compiler_control 1
   --  unsupported macro: VK_AMD_PIPELINE_COMPILER_CONTROL_SPEC_VERSION 1
   --  unsupported macro: VK_AMD_PIPELINE_COMPILER_CONTROL_EXTENSION_NAME "VK_AMD_pipeline_compiler_control"
   --  unsupported macro: VK_EXT_calibrated_timestamps 1
   --  unsupported macro: VK_EXT_CALIBRATED_TIMESTAMPS_SPEC_VERSION 1
   --  unsupported macro: VK_EXT_CALIBRATED_TIMESTAMPS_EXTENSION_NAME "VK_EXT_calibrated_timestamps"
   --  unsupported macro: VK_AMD_shader_core_properties 1
   --  unsupported macro: VK_AMD_SHADER_CORE_PROPERTIES_SPEC_VERSION 2
   --  unsupported macro: VK_AMD_SHADER_CORE_PROPERTIES_EXTENSION_NAME "VK_AMD_shader_core_properties"
   --  unsupported macro: VK_AMD_memory_overallocation_behavior 1
   --  unsupported macro: VK_AMD_MEMORY_OVERALLOCATION_BEHAVIOR_SPEC_VERSION 1
   --  unsupported macro: VK_AMD_MEMORY_OVERALLOCATION_BEHAVIOR_EXTENSION_NAME "VK_AMD_memory_overallocation_behavior"
   --  unsupported macro: VK_EXT_vertex_attribute_divisor 1
   --  unsupported macro: VK_EXT_VERTEX_ATTRIBUTE_DIVISOR_SPEC_VERSION 3
   --  unsupported macro: VK_EXT_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME "VK_EXT_vertex_attribute_divisor"
   --  unsupported macro: VK_EXT_pipeline_creation_feedback 1
   --  unsupported macro: VK_EXT_PIPELINE_CREATION_FEEDBACK_SPEC_VERSION 1
   --  unsupported macro: VK_EXT_PIPELINE_CREATION_FEEDBACK_EXTENSION_NAME "VK_EXT_pipeline_creation_feedback"
   --  unsupported macro: VK_NV_shader_subgroup_partitioned 1
   --  unsupported macro: VK_NV_SHADER_SUBGROUP_PARTITIONED_SPEC_VERSION 1
   --  unsupported macro: VK_NV_SHADER_SUBGROUP_PARTITIONED_EXTENSION_NAME "VK_NV_shader_subgroup_partitioned"
   --  unsupported macro: VK_NV_compute_shader_derivatives 1
   --  unsupported macro: VK_NV_COMPUTE_SHADER_DERIVATIVES_SPEC_VERSION 1
   --  unsupported macro: VK_NV_COMPUTE_SHADER_DERIVATIVES_EXTENSION_NAME "VK_NV_compute_shader_derivatives"
   --  unsupported macro: VK_NV_mesh_shader 1
   --  unsupported macro: VK_NV_MESH_SHADER_SPEC_VERSION 1
   --  unsupported macro: VK_NV_MESH_SHADER_EXTENSION_NAME "VK_NV_mesh_shader"
   --  unsupported macro: VK_NV_fragment_shader_barycentric 1
   --  unsupported macro: VK_NV_FRAGMENT_SHADER_BARYCENTRIC_SPEC_VERSION 1
   --  unsupported macro: VK_NV_FRAGMENT_SHADER_BARYCENTRIC_EXTENSION_NAME "VK_NV_fragment_shader_barycentric"
   --  unsupported macro: VK_NV_shader_image_footprint 1
   --  unsupported macro: VK_NV_SHADER_IMAGE_FOOTPRINT_SPEC_VERSION 2
   --  unsupported macro: VK_NV_SHADER_IMAGE_FOOTPRINT_EXTENSION_NAME "VK_NV_shader_image_footprint"
   --  unsupported macro: VK_NV_scissor_exclusive 1
   --  unsupported macro: VK_NV_SCISSOR_EXCLUSIVE_SPEC_VERSION 1
   --  unsupported macro: VK_NV_SCISSOR_EXCLUSIVE_EXTENSION_NAME "VK_NV_scissor_exclusive"
   --  unsupported macro: VK_NV_device_diagnostic_checkpoints 1
   --  unsupported macro: VK_NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_SPEC_VERSION 2
   --  unsupported macro: VK_NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_EXTENSION_NAME "VK_NV_device_diagnostic_checkpoints"
   --  unsupported macro: VK_INTEL_shader_integer_functions2 1
   --  unsupported macro: VK_INTEL_SHADER_INTEGER_FUNCTIONS_2_SPEC_VERSION 1
   --  unsupported macro: VK_INTEL_SHADER_INTEGER_FUNCTIONS_2_EXTENSION_NAME "VK_INTEL_shader_integer_functions2"
   --  unsupported macro: VK_INTEL_performance_query 1
   --  unsupported macro: VK_INTEL_PERFORMANCE_QUERY_SPEC_VERSION 2
   --  unsupported macro: VK_INTEL_PERFORMANCE_QUERY_EXTENSION_NAME "VK_INTEL_performance_query"
   --  unsupported macro: VK_EXT_pci_bus_info 1
   --  unsupported macro: VK_EXT_PCI_BUS_INFO_SPEC_VERSION 2
   --  unsupported macro: VK_EXT_PCI_BUS_INFO_EXTENSION_NAME "VK_EXT_pci_bus_info"
   --  unsupported macro: VK_AMD_display_native_hdr 1
   --  unsupported macro: VK_AMD_DISPLAY_NATIVE_HDR_SPEC_VERSION 1
   --  unsupported macro: VK_AMD_DISPLAY_NATIVE_HDR_EXTENSION_NAME "VK_AMD_display_native_hdr"
   --  unsupported macro: VK_EXT_fragment_density_map 1
   --  unsupported macro: VK_EXT_FRAGMENT_DENSITY_MAP_SPEC_VERSION 1
   --  unsupported macro: VK_EXT_FRAGMENT_DENSITY_MAP_EXTENSION_NAME "VK_EXT_fragment_density_map"
   --  unsupported macro: VK_EXT_scalar_block_layout 1
   --  unsupported macro: VK_EXT_SCALAR_BLOCK_LAYOUT_SPEC_VERSION 1
   --  unsupported macro: VK_EXT_SCALAR_BLOCK_LAYOUT_EXTENSION_NAME "VK_EXT_scalar_block_layout"
   --  unsupported macro: VK_GOOGLE_hlsl_functionality1 1
   --  unsupported macro: VK_GOOGLE_HLSL_FUNCTIONALITY1_SPEC_VERSION 1
   --  unsupported macro: VK_GOOGLE_HLSL_FUNCTIONALITY1_EXTENSION_NAME "VK_GOOGLE_hlsl_functionality1"
   --  unsupported macro: VK_GOOGLE_decorate_string 1
   --  unsupported macro: VK_GOOGLE_DECORATE_STRING_SPEC_VERSION 1
   --  unsupported macro: VK_GOOGLE_DECORATE_STRING_EXTENSION_NAME "VK_GOOGLE_decorate_string"
   --  unsupported macro: VK_EXT_subgroup_size_control 1
   --  unsupported macro: VK_EXT_SUBGROUP_SIZE_CONTROL_SPEC_VERSION 2
   --  unsupported macro: VK_EXT_SUBGROUP_SIZE_CONTROL_EXTENSION_NAME "VK_EXT_subgroup_size_control"
   --  unsupported macro: VK_AMD_shader_core_properties2 1
   --  unsupported macro: VK_AMD_SHADER_CORE_PROPERTIES_2_SPEC_VERSION 1
   --  unsupported macro: VK_AMD_SHADER_CORE_PROPERTIES_2_EXTENSION_NAME "VK_AMD_shader_core_properties2"
   --  unsupported macro: VK_AMD_device_coherent_memory 1
   --  unsupported macro: VK_AMD_DEVICE_COHERENT_MEMORY_SPEC_VERSION 1
   --  unsupported macro: VK_AMD_DEVICE_COHERENT_MEMORY_EXTENSION_NAME "VK_AMD_device_coherent_memory"
   --  unsupported macro: VK_EXT_shader_image_atomic_int64 1
   --  unsupported macro: VK_EXT_SHADER_IMAGE_ATOMIC_INT64_SPEC_VERSION 1
   --  unsupported macro: VK_EXT_SHADER_IMAGE_ATOMIC_INT64_EXTENSION_NAME "VK_EXT_shader_image_atomic_int64"
   --  unsupported macro: VK_EXT_memory_budget 1
   --  unsupported macro: VK_EXT_MEMORY_BUDGET_SPEC_VERSION 1
   --  unsupported macro: VK_EXT_MEMORY_BUDGET_EXTENSION_NAME "VK_EXT_memory_budget"
   --  unsupported macro: VK_EXT_memory_priority 1
   --  unsupported macro: VK_EXT_MEMORY_PRIORITY_SPEC_VERSION 1
   --  unsupported macro: VK_EXT_MEMORY_PRIORITY_EXTENSION_NAME "VK_EXT_memory_priority"
   --  unsupported macro: VK_NV_dedicated_allocation_image_aliasing 1
   --  unsupported macro: VK_NV_DEDICATED_ALLOCATION_IMAGE_ALIASING_SPEC_VERSION 1
   --  unsupported macro: VK_NV_DEDICATED_ALLOCATION_IMAGE_ALIASING_EXTENSION_NAME "VK_NV_dedicated_allocation_image_aliasing"
   --  unsupported macro: VK_EXT_buffer_device_address 1
   --  unsupported macro: VK_EXT_BUFFER_DEVICE_ADDRESS_SPEC_VERSION 2
   --  unsupported macro: VK_EXT_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME "VK_EXT_buffer_device_address"
   --  unsupported macro: VK_EXT_tooling_info 1
   --  unsupported macro: VK_EXT_TOOLING_INFO_SPEC_VERSION 1
   --  unsupported macro: VK_EXT_TOOLING_INFO_EXTENSION_NAME "VK_EXT_tooling_info"
   --  unsupported macro: VK_EXT_separate_stencil_usage 1
   --  unsupported macro: VK_EXT_SEPARATE_STENCIL_USAGE_SPEC_VERSION 1
   --  unsupported macro: VK_EXT_SEPARATE_STENCIL_USAGE_EXTENSION_NAME "VK_EXT_separate_stencil_usage"
   --  unsupported macro: VK_EXT_validation_features 1
   --  unsupported macro: VK_EXT_VALIDATION_FEATURES_SPEC_VERSION 4
   --  unsupported macro: VK_EXT_VALIDATION_FEATURES_EXTENSION_NAME "VK_EXT_validation_features"
   --  unsupported macro: VK_NV_cooperative_matrix 1
   --  unsupported macro: VK_NV_COOPERATIVE_MATRIX_SPEC_VERSION 1
   --  unsupported macro: VK_NV_COOPERATIVE_MATRIX_EXTENSION_NAME "VK_NV_cooperative_matrix"
   --  unsupported macro: VK_NV_coverage_reduction_mode 1
   --  unsupported macro: VK_NV_COVERAGE_REDUCTION_MODE_SPEC_VERSION 1
   --  unsupported macro: VK_NV_COVERAGE_REDUCTION_MODE_EXTENSION_NAME "VK_NV_coverage_reduction_mode"
   --  unsupported macro: VK_EXT_fragment_shader_interlock 1
   --  unsupported macro: VK_EXT_FRAGMENT_SHADER_INTERLOCK_SPEC_VERSION 1
   --  unsupported macro: VK_EXT_FRAGMENT_SHADER_INTERLOCK_EXTENSION_NAME "VK_EXT_fragment_shader_interlock"
   --  unsupported macro: VK_EXT_ycbcr_image_arrays 1
   --  unsupported macro: VK_EXT_YCBCR_IMAGE_ARRAYS_SPEC_VERSION 1
   --  unsupported macro: VK_EXT_YCBCR_IMAGE_ARRAYS_EXTENSION_NAME "VK_EXT_ycbcr_image_arrays"
   --  unsupported macro: VK_EXT_headless_surface 1
   --  unsupported macro: VK_EXT_HEADLESS_SURFACE_SPEC_VERSION 1
   --  unsupported macro: VK_EXT_HEADLESS_SURFACE_EXTENSION_NAME "VK_EXT_headless_surface"
   --  unsupported macro: VK_EXT_line_rasterization 1
   --  unsupported macro: VK_EXT_LINE_RASTERIZATION_SPEC_VERSION 1
   --  unsupported macro: VK_EXT_LINE_RASTERIZATION_EXTENSION_NAME "VK_EXT_line_rasterization"
   --  unsupported macro: VK_EXT_shader_atomic_float 1
   --  unsupported macro: VK_EXT_SHADER_ATOMIC_FLOAT_SPEC_VERSION 1
   --  unsupported macro: VK_EXT_SHADER_ATOMIC_FLOAT_EXTENSION_NAME "VK_EXT_shader_atomic_float"
   --  unsupported macro: VK_EXT_host_query_reset 1
   --  unsupported macro: VK_EXT_HOST_QUERY_RESET_SPEC_VERSION 1
   --  unsupported macro: VK_EXT_HOST_QUERY_RESET_EXTENSION_NAME "VK_EXT_host_query_reset"
   --  unsupported macro: VK_EXT_index_type_uint8 1
   --  unsupported macro: VK_EXT_INDEX_TYPE_UINT8_SPEC_VERSION 1
   --  unsupported macro: VK_EXT_INDEX_TYPE_UINT8_EXTENSION_NAME "VK_EXT_index_type_uint8"
   --  unsupported macro: VK_EXT_extended_dynamic_state 1
   --  unsupported macro: VK_EXT_EXTENDED_DYNAMIC_STATE_SPEC_VERSION 1
   --  unsupported macro: VK_EXT_EXTENDED_DYNAMIC_STATE_EXTENSION_NAME "VK_EXT_extended_dynamic_state"
   --  unsupported macro: VK_EXT_shader_demote_to_helper_invocation 1
   --  unsupported macro: VK_EXT_SHADER_DEMOTE_TO_HELPER_INVOCATION_SPEC_VERSION 1
   --  unsupported macro: VK_EXT_SHADER_DEMOTE_TO_HELPER_INVOCATION_EXTENSION_NAME "VK_EXT_shader_demote_to_helper_invocation"
   --  unsupported macro: VK_NV_device_generated_commands 1
   --  unsupported macro: VK_NV_DEVICE_GENERATED_COMMANDS_SPEC_VERSION 3
   --  unsupported macro: VK_NV_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME "VK_NV_device_generated_commands"
   --  unsupported macro: VK_EXT_texel_buffer_alignment 1
   --  unsupported macro: VK_EXT_TEXEL_BUFFER_ALIGNMENT_SPEC_VERSION 1
   --  unsupported macro: VK_EXT_TEXEL_BUFFER_ALIGNMENT_EXTENSION_NAME "VK_EXT_texel_buffer_alignment"
   --  unsupported macro: VK_QCOM_render_pass_transform 1
   --  unsupported macro: VK_QCOM_RENDER_PASS_TRANSFORM_SPEC_VERSION 1
   --  unsupported macro: VK_QCOM_RENDER_PASS_TRANSFORM_EXTENSION_NAME "VK_QCOM_render_pass_transform"
   --  unsupported macro: VK_EXT_device_memory_report 1
   --  unsupported macro: VK_EXT_DEVICE_MEMORY_REPORT_SPEC_VERSION 2
   --  unsupported macro: VK_EXT_DEVICE_MEMORY_REPORT_EXTENSION_NAME "VK_EXT_device_memory_report"
   --  unsupported macro: VK_EXT_robustness2 1
   --  unsupported macro: VK_EXT_ROBUSTNESS_2_SPEC_VERSION 1
   --  unsupported macro: VK_EXT_ROBUSTNESS_2_EXTENSION_NAME "VK_EXT_robustness2"
   --  unsupported macro: VK_EXT_custom_border_color 1
   --  unsupported macro: VK_EXT_CUSTOM_BORDER_COLOR_SPEC_VERSION 12
   --  unsupported macro: VK_EXT_CUSTOM_BORDER_COLOR_EXTENSION_NAME "VK_EXT_custom_border_color"
   --  unsupported macro: VK_GOOGLE_user_type 1
   --  unsupported macro: VK_GOOGLE_USER_TYPE_SPEC_VERSION 1
   --  unsupported macro: VK_GOOGLE_USER_TYPE_EXTENSION_NAME "VK_GOOGLE_user_type"
   --  unsupported macro: VK_EXT_private_data 1
   --  unsupported macro: VK_EXT_PRIVATE_DATA_SPEC_VERSION 1
   --  unsupported macro: VK_EXT_PRIVATE_DATA_EXTENSION_NAME "VK_EXT_private_data"
   --  unsupported macro: VK_EXT_pipeline_creation_cache_control 1
   --  unsupported macro: VK_EXT_PIPELINE_CREATION_CACHE_CONTROL_SPEC_VERSION 3
   --  unsupported macro: VK_EXT_PIPELINE_CREATION_CACHE_CONTROL_EXTENSION_NAME "VK_EXT_pipeline_creation_cache_control"
   --  unsupported macro: VK_NV_device_diagnostics_config 1
   --  unsupported macro: VK_NV_DEVICE_DIAGNOSTICS_CONFIG_SPEC_VERSION 1
   --  unsupported macro: VK_NV_DEVICE_DIAGNOSTICS_CONFIG_EXTENSION_NAME "VK_NV_device_diagnostics_config"
   --  unsupported macro: VK_QCOM_render_pass_store_ops 1
   --  unsupported macro: VK_QCOM_render_pass_store_ops_SPEC_VERSION 2
   --  unsupported macro: VK_QCOM_render_pass_store_ops_EXTENSION_NAME "VK_QCOM_render_pass_store_ops"
   --  unsupported macro: VK_NV_fragment_shading_rate_enums 1
   --  unsupported macro: VK_NV_FRAGMENT_SHADING_RATE_ENUMS_SPEC_VERSION 1
   --  unsupported macro: VK_NV_FRAGMENT_SHADING_RATE_ENUMS_EXTENSION_NAME "VK_NV_fragment_shading_rate_enums"
   --  unsupported macro: VK_EXT_fragment_density_map2 1
   --  unsupported macro: VK_EXT_FRAGMENT_DENSITY_MAP_2_SPEC_VERSION 1
   --  unsupported macro: VK_EXT_FRAGMENT_DENSITY_MAP_2_EXTENSION_NAME "VK_EXT_fragment_density_map2"
   --  unsupported macro: VK_QCOM_rotated_copy_commands 1
   --  unsupported macro: VK_QCOM_ROTATED_COPY_COMMANDS_SPEC_VERSION 0
   --  unsupported macro: VK_QCOM_ROTATED_COPY_COMMANDS_EXTENSION_NAME "VK_QCOM_rotated_copy_commands"
   --  unsupported macro: VK_EXT_image_robustness 1
   --  unsupported macro: VK_EXT_IMAGE_ROBUSTNESS_SPEC_VERSION 1
   --  unsupported macro: VK_EXT_IMAGE_ROBUSTNESS_EXTENSION_NAME "VK_EXT_image_robustness"
   --  unsupported macro: VK_EXT_4444_formats 1
   --  unsupported macro: VK_EXT_4444_FORMATS_SPEC_VERSION 1
   --  unsupported macro: VK_EXT_4444_FORMATS_EXTENSION_NAME "VK_EXT_4444_formats"
   --  unsupported macro: VK_NV_acquire_winrt_display 1
   --  unsupported macro: VK_NV_ACQUIRE_WINRT_DISPLAY_SPEC_VERSION 1
   --  unsupported macro: VK_NV_ACQUIRE_WINRT_DISPLAY_EXTENSION_NAME "VK_NV_acquire_winrt_display"
   --  unsupported macro: VK_VALVE_mutable_descriptor_type 1
   --  unsupported macro: VK_VALVE_MUTABLE_DESCRIPTOR_TYPE_SPEC_VERSION 1
   --  unsupported macro: VK_VALVE_MUTABLE_DESCRIPTOR_TYPE_EXTENSION_NAME "VK_VALVE_mutable_descriptor_type"
   --  unsupported macro: VK_KHR_acceleration_structure 1
   --  unsupported macro: VK_KHR_ACCELERATION_STRUCTURE_SPEC_VERSION 11
   --  unsupported macro: VK_KHR_ACCELERATION_STRUCTURE_EXTENSION_NAME "VK_KHR_acceleration_structure"
   --  unsupported macro: VK_KHR_ray_tracing_pipeline 1
   --  unsupported macro: VK_KHR_RAY_TRACING_PIPELINE_SPEC_VERSION 1
   --  unsupported macro: VK_KHR_RAY_TRACING_PIPELINE_EXTENSION_NAME "VK_KHR_ray_tracing_pipeline"
   --  unsupported macro: VK_KHR_ray_query 1
   --  unsupported macro: VK_KHR_RAY_QUERY_SPEC_VERSION 1
   --  unsupported macro: VK_KHR_RAY_QUERY_EXTENSION_NAME "VK_KHR_ray_query"
  --** Copyright 2015-2021 The Khronos Group Inc.
  --**
  --** SPDX-License-Identifier: Apache-2.0
  --

  --** This header is generated from the Khronos Vulkan XML API Registry.
  --**
  --

  -- DEPRECATED: This define has been removed. Specific version defines (e.g. VK_API_VERSION_1_0), or the VK_MAKE_VERSION macro, should be used instead.
  --#define VK_API_VERSION VK_MAKE_VERSION(1, 0, 0) // Patch version should always be set to 0
  -- Vulkan 1.0 version number
  -- Version of this file
  -- Complete version of this file
   subtype VkBool32 is stdint_h.uint32_t;  -- vulkan_core.h:57

   subtype VkDeviceAddress is stdint_h.uint64_t;  -- vulkan_core.h:58

   subtype VkDeviceSize is stdint_h.uint64_t;  -- vulkan_core.h:59

   subtype VkFlags is stdint_h.uint32_t;  -- vulkan_core.h:60

   subtype VkSampleMask is stdint_h.uint32_t;  -- vulkan_core.h:61

   type VkBuffer is new System.Address;  -- vulkan_core.h:62

   --  skipped empty struct VkBuffer_T

   type VkImage is new System.Address;  -- vulkan_core.h:63

   --  skipped empty struct VkImage_T

   --  skipped empty struct VkInstance_T

   type VkInstance is new System.Address;  -- vulkan_core.h:64

   type VkPhysicalDevice is new System.Address;  -- vulkan_core.h:65

   --  skipped empty struct VkPhysicalDevice_T

   type VkDevice is new System.Address;  -- vulkan_core.h:66

   --  skipped empty struct VkDevice_T

   type VkQueue is new System.Address;  -- vulkan_core.h:67

   --  skipped empty struct VkQueue_T

   type VkSemaphore is new System.Address;  -- vulkan_core.h:68

   --  skipped empty struct VkSemaphore_T

   --  skipped empty struct VkCommandBuffer_T

   type VkCommandBuffer is new System.Address;  -- vulkan_core.h:69

   type VkFence is new System.Address;  -- vulkan_core.h:70

   --  skipped empty struct VkFence_T

   type VkDeviceMemory is new System.Address;  -- vulkan_core.h:71

   --  skipped empty struct VkDeviceMemory_T

   type VkEvent is new System.Address;  -- vulkan_core.h:72

   --  skipped empty struct VkEvent_T

   type VkQueryPool is new System.Address;  -- vulkan_core.h:73

   --  skipped empty struct VkQueryPool_T

   --  skipped empty struct VkBufferView_T

   type VkBufferView is new System.Address;  -- vulkan_core.h:74

   type VkImageView is new System.Address;  -- vulkan_core.h:75

   --  skipped empty struct VkImageView_T

   type VkShaderModule is new System.Address;  -- vulkan_core.h:76

   --  skipped empty struct VkShaderModule_T

   type VkPipelineCache is new System.Address;  -- vulkan_core.h:77

   --  skipped empty struct VkPipelineCache_T

   type VkPipelineLayout is new System.Address;  -- vulkan_core.h:78

   --  skipped empty struct VkPipelineLayout_T

   --  skipped empty struct VkPipeline_T

   type VkPipeline is new System.Address;  -- vulkan_core.h:79

   type VkRenderPass is new System.Address;  -- vulkan_core.h:80

   --  skipped empty struct VkRenderPass_T

   type VkDescriptorSetLayout is new System.Address;  -- vulkan_core.h:81

   --  skipped empty struct VkDescriptorSetLayout_T

   type VkSampler is new System.Address;  -- vulkan_core.h:82

   --  skipped empty struct VkSampler_T

   type VkDescriptorSet is new System.Address;  -- vulkan_core.h:83

   --  skipped empty struct VkDescriptorSet_T

   --  skipped empty struct VkDescriptorPool_T

   type VkDescriptorPool is new System.Address;  -- vulkan_core.h:84

   type VkFramebuffer is new System.Address;  -- vulkan_core.h:85

   --  skipped empty struct VkFramebuffer_T

   type VkCommandPool is new System.Address;  -- vulkan_core.h:86

   --  skipped empty struct VkCommandPool_T

   subtype VkResult is unsigned;
   VK_SUCCESS : constant VkResult := 0;
   VK_NOT_READY : constant VkResult := 1;
   VK_TIMEOUT : constant VkResult := 2;
   VK_EVENT_SET : constant VkResult := 3;
   VK_EVENT_RESET : constant VkResult := 4;
   VK_INCOMPLETE : constant VkResult := 5;
   VK_ERROR_OUT_OF_HOST_MEMORY : constant VkResult := -1;
   VK_ERROR_OUT_OF_DEVICE_MEMORY : constant VkResult := -2;
   VK_ERROR_INITIALIZATION_FAILED : constant VkResult := -3;
   VK_ERROR_DEVICE_LOST : constant VkResult := -4;
   VK_ERROR_MEMORY_MAP_FAILED : constant VkResult := -5;
   VK_ERROR_LAYER_NOT_PRESENT : constant VkResult := -6;
   VK_ERROR_EXTENSION_NOT_PRESENT : constant VkResult := -7;
   VK_ERROR_FEATURE_NOT_PRESENT : constant VkResult := -8;
   VK_ERROR_INCOMPATIBLE_DRIVER : constant VkResult := -9;
   VK_ERROR_TOO_MANY_OBJECTS : constant VkResult := -10;
   VK_ERROR_FORMAT_NOT_SUPPORTED : constant VkResult := -11;
   VK_ERROR_FRAGMENTED_POOL : constant VkResult := -12;
   VK_ERROR_UNKNOWN : constant VkResult := -13;
   VK_ERROR_OUT_OF_POOL_MEMORY : constant VkResult := -1000069000;
   VK_ERROR_INVALID_EXTERNAL_HANDLE : constant VkResult := -1000072003;
   VK_ERROR_FRAGMENTATION : constant VkResult := -1000161000;
   VK_ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS : constant VkResult := -1000257000;
   VK_ERROR_SURFACE_LOST_KHR : constant VkResult := -1000000000;
   VK_ERROR_NATIVE_WINDOW_IN_USE_KHR : constant VkResult := -1000000001;
   VK_SUBOPTIMAL_KHR : constant VkResult := 1000001003;
   VK_ERROR_OUT_OF_DATE_KHR : constant VkResult := -1000001004;
   VK_ERROR_INCOMPATIBLE_DISPLAY_KHR : constant VkResult := -1000003001;
   VK_ERROR_VALIDATION_FAILED_EXT : constant VkResult := -1000011001;
   VK_ERROR_INVALID_SHADER_NV : constant VkResult := -1000012000;
   VK_ERROR_INVALID_DRM_FORMAT_MODIFIER_PLANE_LAYOUT_EXT : constant VkResult := -1000158000;
   VK_ERROR_NOT_PERMITTED_EXT : constant VkResult := -1000174001;
   VK_ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT : constant VkResult := -1000255000;
   VK_THREAD_IDLE_KHR : constant VkResult := 1000268000;
   VK_THREAD_DONE_KHR : constant VkResult := 1000268001;
   VK_OPERATION_DEFERRED_KHR : constant VkResult := 1000268002;
   VK_OPERATION_NOT_DEFERRED_KHR : constant VkResult := 1000268003;
   VK_PIPELINE_COMPILE_REQUIRED_EXT : constant VkResult := 1000297000;
   VK_ERROR_OUT_OF_POOL_MEMORY_KHR : constant VkResult := -1000069000;
   VK_ERROR_INVALID_EXTERNAL_HANDLE_KHR : constant VkResult := -1000072003;
   VK_ERROR_FRAGMENTATION_EXT : constant VkResult := -1000161000;
   VK_ERROR_INVALID_DEVICE_ADDRESS_EXT : constant VkResult := -1000257000;
   VK_ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS_KHR : constant VkResult := -1000257000;
   VK_ERROR_PIPELINE_COMPILE_REQUIRED_EXT : constant VkResult := 1000297000;
   VK_RESULT_MAX_ENUM : constant VkResult := 2147483647;  -- vulkan_core.h:103

   subtype VkStructureType is unsigned;
   VK_STRUCTURE_TYPE_APPLICATION_INFO : constant VkStructureType := 0;
   VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO : constant VkStructureType := 1;
   VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO : constant VkStructureType := 2;
   VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO : constant VkStructureType := 3;
   VK_STRUCTURE_TYPE_SUBMIT_INFO : constant VkStructureType := 4;
   VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO : constant VkStructureType := 5;
   VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE : constant VkStructureType := 6;
   VK_STRUCTURE_TYPE_BIND_SPARSE_INFO : constant VkStructureType := 7;
   VK_STRUCTURE_TYPE_FENCE_CREATE_INFO : constant VkStructureType := 8;
   VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO : constant VkStructureType := 9;
   VK_STRUCTURE_TYPE_EVENT_CREATE_INFO : constant VkStructureType := 10;
   VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO : constant VkStructureType := 11;
   VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO : constant VkStructureType := 12;
   VK_STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO : constant VkStructureType := 13;
   VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO : constant VkStructureType := 14;
   VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO : constant VkStructureType := 15;
   VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO : constant VkStructureType := 16;
   VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO : constant VkStructureType := 17;
   VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO : constant VkStructureType := 18;
   VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO : constant VkStructureType := 19;
   VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO : constant VkStructureType := 20;
   VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO : constant VkStructureType := 21;
   VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO : constant VkStructureType := 22;
   VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO : constant VkStructureType := 23;
   VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO : constant VkStructureType := 24;
   VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO : constant VkStructureType := 25;
   VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO : constant VkStructureType := 26;
   VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO : constant VkStructureType := 27;
   VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO : constant VkStructureType := 28;
   VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO : constant VkStructureType := 29;
   VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO : constant VkStructureType := 30;
   VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO : constant VkStructureType := 31;
   VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO : constant VkStructureType := 32;
   VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO : constant VkStructureType := 33;
   VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO : constant VkStructureType := 34;
   VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET : constant VkStructureType := 35;
   VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET : constant VkStructureType := 36;
   VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO : constant VkStructureType := 37;
   VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO : constant VkStructureType := 38;
   VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO : constant VkStructureType := 39;
   VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO : constant VkStructureType := 40;
   VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO : constant VkStructureType := 41;
   VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO : constant VkStructureType := 42;
   VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO : constant VkStructureType := 43;
   VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER : constant VkStructureType := 44;
   VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER : constant VkStructureType := 45;
   VK_STRUCTURE_TYPE_MEMORY_BARRIER : constant VkStructureType := 46;
   VK_STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO : constant VkStructureType := 47;
   VK_STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO : constant VkStructureType := 48;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_PROPERTIES : constant VkStructureType := 1000094000;
   VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO : constant VkStructureType := 1000157000;
   VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO : constant VkStructureType := 1000157001;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES : constant VkStructureType := 1000083000;
   VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS : constant VkStructureType := 1000127000;
   VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO : constant VkStructureType := 1000127001;
   VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO : constant VkStructureType := 1000060000;
   VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO : constant VkStructureType := 1000060003;
   VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO : constant VkStructureType := 1000060004;
   VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO : constant VkStructureType := 1000060005;
   VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO : constant VkStructureType := 1000060006;
   VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO : constant VkStructureType := 1000060013;
   VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO : constant VkStructureType := 1000060014;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES : constant VkStructureType := 1000070000;
   VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO : constant VkStructureType := 1000070001;
   VK_STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2 : constant VkStructureType := 1000146000;
   VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2 : constant VkStructureType := 1000146001;
   VK_STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2 : constant VkStructureType := 1000146002;
   VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2 : constant VkStructureType := 1000146003;
   VK_STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2 : constant VkStructureType := 1000146004;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2 : constant VkStructureType := 1000059000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2 : constant VkStructureType := 1000059001;
   VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2 : constant VkStructureType := 1000059002;
   VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2 : constant VkStructureType := 1000059003;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2 : constant VkStructureType := 1000059004;
   VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2 : constant VkStructureType := 1000059005;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2 : constant VkStructureType := 1000059006;
   VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2 : constant VkStructureType := 1000059007;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2 : constant VkStructureType := 1000059008;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES : constant VkStructureType := 1000117000;
   VK_STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO : constant VkStructureType := 1000117001;
   VK_STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO : constant VkStructureType := 1000117002;
   VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO : constant VkStructureType := 1000117003;
   VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO : constant VkStructureType := 1000053000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES : constant VkStructureType := 1000053001;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES : constant VkStructureType := 1000053002;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES : constant VkStructureType := 1000120000;
   VK_STRUCTURE_TYPE_PROTECTED_SUBMIT_INFO : constant VkStructureType := 1000145000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_FEATURES : constant VkStructureType := 1000145001;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_PROPERTIES : constant VkStructureType := 1000145002;
   VK_STRUCTURE_TYPE_DEVICE_QUEUE_INFO_2 : constant VkStructureType := 1000145003;
   VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO : constant VkStructureType := 1000156000;
   VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO : constant VkStructureType := 1000156001;
   VK_STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO : constant VkStructureType := 1000156002;
   VK_STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO : constant VkStructureType := 1000156003;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES : constant VkStructureType := 1000156004;
   VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES : constant VkStructureType := 1000156005;
   VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO : constant VkStructureType := 1000085000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO : constant VkStructureType := 1000071000;
   VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES : constant VkStructureType := 1000071001;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO : constant VkStructureType := 1000071002;
   VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES : constant VkStructureType := 1000071003;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES : constant VkStructureType := 1000071004;
   VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO : constant VkStructureType := 1000072000;
   VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO : constant VkStructureType := 1000072001;
   VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO : constant VkStructureType := 1000072002;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO : constant VkStructureType := 1000112000;
   VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES : constant VkStructureType := 1000112001;
   VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO : constant VkStructureType := 1000113000;
   VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO : constant VkStructureType := 1000077000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO : constant VkStructureType := 1000076000;
   VK_STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES : constant VkStructureType := 1000076001;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES : constant VkStructureType := 1000168000;
   VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT : constant VkStructureType := 1000168001;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETERS_FEATURES : constant VkStructureType := 1000063000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_1_FEATURES : constant VkStructureType := 49;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_1_PROPERTIES : constant VkStructureType := 50;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_FEATURES : constant VkStructureType := 51;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_PROPERTIES : constant VkStructureType := 52;
   VK_STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO : constant VkStructureType := 1000147000;
   VK_STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_2 : constant VkStructureType := 1000109000;
   VK_STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2 : constant VkStructureType := 1000109001;
   VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_2 : constant VkStructureType := 1000109002;
   VK_STRUCTURE_TYPE_SUBPASS_DEPENDENCY_2 : constant VkStructureType := 1000109003;
   VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO_2 : constant VkStructureType := 1000109004;
   VK_STRUCTURE_TYPE_SUBPASS_BEGIN_INFO : constant VkStructureType := 1000109005;
   VK_STRUCTURE_TYPE_SUBPASS_END_INFO : constant VkStructureType := 1000109006;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_8BIT_STORAGE_FEATURES : constant VkStructureType := 1000177000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DRIVER_PROPERTIES : constant VkStructureType := 1000196000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_INT64_FEATURES : constant VkStructureType := 1000180000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_FLOAT16_INT8_FEATURES : constant VkStructureType := 1000082000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES : constant VkStructureType := 1000197000;
   VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO : constant VkStructureType := 1000161000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES : constant VkStructureType := 1000161001;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES : constant VkStructureType := 1000161002;
   VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO : constant VkStructureType := 1000161003;
   VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT : constant VkStructureType := 1000161004;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_STENCIL_RESOLVE_PROPERTIES : constant VkStructureType := 1000199000;
   VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_DEPTH_STENCIL_RESOLVE : constant VkStructureType := 1000199001;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES : constant VkStructureType := 1000221000;
   VK_STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO : constant VkStructureType := 1000246000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES : constant VkStructureType := 1000130000;
   VK_STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO : constant VkStructureType := 1000130001;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_MEMORY_MODEL_FEATURES : constant VkStructureType := 1000211000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGELESS_FRAMEBUFFER_FEATURES : constant VkStructureType := 1000108000;
   VK_STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENTS_CREATE_INFO : constant VkStructureType := 1000108001;
   VK_STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENT_IMAGE_INFO : constant VkStructureType := 1000108002;
   VK_STRUCTURE_TYPE_RENDER_PASS_ATTACHMENT_BEGIN_INFO : constant VkStructureType := 1000108003;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_UNIFORM_BUFFER_STANDARD_LAYOUT_FEATURES : constant VkStructureType := 1000253000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SUBGROUP_EXTENDED_TYPES_FEATURES : constant VkStructureType := 1000175000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SEPARATE_DEPTH_STENCIL_LAYOUTS_FEATURES : constant VkStructureType := 1000241000;
   VK_STRUCTURE_TYPE_ATTACHMENT_REFERENCE_STENCIL_LAYOUT : constant VkStructureType := 1000241001;
   VK_STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_STENCIL_LAYOUT : constant VkStructureType := 1000241002;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES : constant VkStructureType := 1000261000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_FEATURES : constant VkStructureType := 1000207000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_PROPERTIES : constant VkStructureType := 1000207001;
   VK_STRUCTURE_TYPE_SEMAPHORE_TYPE_CREATE_INFO : constant VkStructureType := 1000207002;
   VK_STRUCTURE_TYPE_TIMELINE_SEMAPHORE_SUBMIT_INFO : constant VkStructureType := 1000207003;
   VK_STRUCTURE_TYPE_SEMAPHORE_WAIT_INFO : constant VkStructureType := 1000207004;
   VK_STRUCTURE_TYPE_SEMAPHORE_SIGNAL_INFO : constant VkStructureType := 1000207005;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES : constant VkStructureType := 1000257000;
   VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO : constant VkStructureType := 1000244001;
   VK_STRUCTURE_TYPE_BUFFER_OPAQUE_CAPTURE_ADDRESS_CREATE_INFO : constant VkStructureType := 1000257002;
   VK_STRUCTURE_TYPE_MEMORY_OPAQUE_CAPTURE_ADDRESS_ALLOCATE_INFO : constant VkStructureType := 1000257003;
   VK_STRUCTURE_TYPE_DEVICE_MEMORY_OPAQUE_CAPTURE_ADDRESS_INFO : constant VkStructureType := 1000257004;
   VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR : constant VkStructureType := 1000001000;
   VK_STRUCTURE_TYPE_PRESENT_INFO_KHR : constant VkStructureType := 1000001001;
   VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHR : constant VkStructureType := 1000060007;
   VK_STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR : constant VkStructureType := 1000060008;
   VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR : constant VkStructureType := 1000060009;
   VK_STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHR : constant VkStructureType := 1000060010;
   VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR : constant VkStructureType := 1000060011;
   VK_STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHR : constant VkStructureType := 1000060012;
   VK_STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR : constant VkStructureType := 1000002000;
   VK_STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR : constant VkStructureType := 1000002001;
   VK_STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR : constant VkStructureType := 1000003000;
   VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR : constant VkStructureType := 1000004000;
   VK_STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR : constant VkStructureType := 1000005000;
   VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR : constant VkStructureType := 1000006000;
   VK_STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR : constant VkStructureType := 1000008000;
   VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR : constant VkStructureType := 1000009000;
   VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT : constant VkStructureType := 1000011000;
   VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD : constant VkStructureType := 1000018000;
   VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT : constant VkStructureType := 1000022000;
   VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT : constant VkStructureType := 1000022001;
   VK_STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT : constant VkStructureType := 1000022002;
   VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV : constant VkStructureType := 1000026000;
   VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV : constant VkStructureType := 1000026001;
   VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV : constant VkStructureType := 1000026002;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_FEATURES_EXT : constant VkStructureType := 1000028000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_PROPERTIES_EXT : constant VkStructureType := 1000028001;
   VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_STREAM_CREATE_INFO_EXT : constant VkStructureType := 1000028002;
   VK_STRUCTURE_TYPE_IMAGE_VIEW_HANDLE_INFO_NVX : constant VkStructureType := 1000030000;
   VK_STRUCTURE_TYPE_IMAGE_VIEW_ADDRESS_PROPERTIES_NVX : constant VkStructureType := 1000030001;
   VK_STRUCTURE_TYPE_TEXTURE_LOD_GATHER_FORMAT_PROPERTIES_AMD : constant VkStructureType := 1000041000;
   VK_STRUCTURE_TYPE_STREAM_DESCRIPTOR_SURFACE_CREATE_INFO_GGP : constant VkStructureType := 1000049000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CORNER_SAMPLED_IMAGE_FEATURES_NV : constant VkStructureType := 1000050000;
   VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV : constant VkStructureType := 1000056000;
   VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV : constant VkStructureType := 1000056001;
   VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV : constant VkStructureType := 1000057000;
   VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV : constant VkStructureType := 1000057001;
   VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_NV : constant VkStructureType := 1000058000;
   VK_STRUCTURE_TYPE_VALIDATION_FLAGS_EXT : constant VkStructureType := 1000061000;
   VK_STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN : constant VkStructureType := 1000062000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXTURE_COMPRESSION_ASTC_HDR_FEATURES_EXT : constant VkStructureType := 1000066000;
   VK_STRUCTURE_TYPE_IMAGE_VIEW_ASTC_DECODE_MODE_EXT : constant VkStructureType := 1000067000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ASTC_DECODE_FEATURES_EXT : constant VkStructureType := 1000067001;
   VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR : constant VkStructureType := 1000073000;
   VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR : constant VkStructureType := 1000073001;
   VK_STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHR : constant VkStructureType := 1000073002;
   VK_STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR : constant VkStructureType := 1000073003;
   VK_STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHR : constant VkStructureType := 1000074000;
   VK_STRUCTURE_TYPE_MEMORY_FD_PROPERTIES_KHR : constant VkStructureType := 1000074001;
   VK_STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR : constant VkStructureType := 1000074002;
   VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_KHR : constant VkStructureType := 1000075000;
   VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR : constant VkStructureType := 1000078000;
   VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR : constant VkStructureType := 1000078001;
   VK_STRUCTURE_TYPE_D3D12_FENCE_SUBMIT_INFO_KHR : constant VkStructureType := 1000078002;
   VK_STRUCTURE_TYPE_SEMAPHORE_GET_WIN32_HANDLE_INFO_KHR : constant VkStructureType := 1000078003;
   VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR : constant VkStructureType := 1000079000;
   VK_STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR : constant VkStructureType := 1000079001;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR : constant VkStructureType := 1000080000;
   VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_CONDITIONAL_RENDERING_INFO_EXT : constant VkStructureType := 1000081000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONDITIONAL_RENDERING_FEATURES_EXT : constant VkStructureType := 1000081001;
   VK_STRUCTURE_TYPE_CONDITIONAL_RENDERING_BEGIN_INFO_EXT : constant VkStructureType := 1000081002;
   VK_STRUCTURE_TYPE_PRESENT_REGIONS_KHR : constant VkStructureType := 1000084000;
   VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV : constant VkStructureType := 1000087000;
   VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT : constant VkStructureType := 1000090000;
   VK_STRUCTURE_TYPE_DISPLAY_POWER_INFO_EXT : constant VkStructureType := 1000091000;
   VK_STRUCTURE_TYPE_DEVICE_EVENT_INFO_EXT : constant VkStructureType := 1000091001;
   VK_STRUCTURE_TYPE_DISPLAY_EVENT_INFO_EXT : constant VkStructureType := 1000091002;
   VK_STRUCTURE_TYPE_SWAPCHAIN_COUNTER_CREATE_INFO_EXT : constant VkStructureType := 1000091003;
   VK_STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE : constant VkStructureType := 1000092000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX : constant VkStructureType := 1000097000;
   VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SWIZZLE_STATE_CREATE_INFO_NV : constant VkStructureType := 1000098000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DISCARD_RECTANGLE_PROPERTIES_EXT : constant VkStructureType := 1000099000;
   VK_STRUCTURE_TYPE_PIPELINE_DISCARD_RECTANGLE_STATE_CREATE_INFO_EXT : constant VkStructureType := 1000099001;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT : constant VkStructureType := 1000101000;
   VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT : constant VkStructureType := 1000101001;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLIP_ENABLE_FEATURES_EXT : constant VkStructureType := 1000102000;
   VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_DEPTH_CLIP_STATE_CREATE_INFO_EXT : constant VkStructureType := 1000102001;
   VK_STRUCTURE_TYPE_HDR_METADATA_EXT : constant VkStructureType := 1000105000;
   VK_STRUCTURE_TYPE_SHARED_PRESENT_SURFACE_CAPABILITIES_KHR : constant VkStructureType := 1000111000;
   VK_STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR : constant VkStructureType := 1000114000;
   VK_STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR : constant VkStructureType := 1000114001;
   VK_STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR : constant VkStructureType := 1000114002;
   VK_STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR : constant VkStructureType := 1000115000;
   VK_STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR : constant VkStructureType := 1000115001;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PERFORMANCE_QUERY_FEATURES_KHR : constant VkStructureType := 1000116000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PERFORMANCE_QUERY_PROPERTIES_KHR : constant VkStructureType := 1000116001;
   VK_STRUCTURE_TYPE_QUERY_POOL_PERFORMANCE_CREATE_INFO_KHR : constant VkStructureType := 1000116002;
   VK_STRUCTURE_TYPE_PERFORMANCE_QUERY_SUBMIT_INFO_KHR : constant VkStructureType := 1000116003;
   VK_STRUCTURE_TYPE_ACQUIRE_PROFILING_LOCK_INFO_KHR : constant VkStructureType := 1000116004;
   VK_STRUCTURE_TYPE_PERFORMANCE_COUNTER_KHR : constant VkStructureType := 1000116005;
   VK_STRUCTURE_TYPE_PERFORMANCE_COUNTER_DESCRIPTION_KHR : constant VkStructureType := 1000116006;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SURFACE_INFO_2_KHR : constant VkStructureType := 1000119000;
   VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_KHR : constant VkStructureType := 1000119001;
   VK_STRUCTURE_TYPE_SURFACE_FORMAT_2_KHR : constant VkStructureType := 1000119002;
   VK_STRUCTURE_TYPE_DISPLAY_PROPERTIES_2_KHR : constant VkStructureType := 1000121000;
   VK_STRUCTURE_TYPE_DISPLAY_PLANE_PROPERTIES_2_KHR : constant VkStructureType := 1000121001;
   VK_STRUCTURE_TYPE_DISPLAY_MODE_PROPERTIES_2_KHR : constant VkStructureType := 1000121002;
   VK_STRUCTURE_TYPE_DISPLAY_PLANE_INFO_2_KHR : constant VkStructureType := 1000121003;
   VK_STRUCTURE_TYPE_DISPLAY_PLANE_CAPABILITIES_2_KHR : constant VkStructureType := 1000121004;
   VK_STRUCTURE_TYPE_IOS_SURFACE_CREATE_INFO_MVK : constant VkStructureType := 1000122000;
   VK_STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK : constant VkStructureType := 1000123000;
   VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT : constant VkStructureType := 1000128000;
   VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_TAG_INFO_EXT : constant VkStructureType := 1000128001;
   VK_STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT : constant VkStructureType := 1000128002;
   VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT : constant VkStructureType := 1000128003;
   VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT : constant VkStructureType := 1000128004;
   VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_USAGE_ANDROID : constant VkStructureType := 1000129000;
   VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_PROPERTIES_ANDROID : constant VkStructureType := 1000129001;
   VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_PROPERTIES_ANDROID : constant VkStructureType := 1000129002;
   VK_STRUCTURE_TYPE_IMPORT_ANDROID_HARDWARE_BUFFER_INFO_ANDROID : constant VkStructureType := 1000129003;
   VK_STRUCTURE_TYPE_MEMORY_GET_ANDROID_HARDWARE_BUFFER_INFO_ANDROID : constant VkStructureType := 1000129004;
   VK_STRUCTURE_TYPE_EXTERNAL_FORMAT_ANDROID : constant VkStructureType := 1000129005;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_FEATURES_EXT : constant VkStructureType := 1000138000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_PROPERTIES_EXT : constant VkStructureType := 1000138001;
   VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_INLINE_UNIFORM_BLOCK_EXT : constant VkStructureType := 1000138002;
   VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_INLINE_UNIFORM_BLOCK_CREATE_INFO_EXT : constant VkStructureType := 1000138003;
   VK_STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT : constant VkStructureType := 1000143000;
   VK_STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT : constant VkStructureType := 1000143001;
   VK_STRUCTURE_TYPE_PIPELINE_SAMPLE_LOCATIONS_STATE_CREATE_INFO_EXT : constant VkStructureType := 1000143002;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT : constant VkStructureType := 1000143003;
   VK_STRUCTURE_TYPE_MULTISAMPLE_PROPERTIES_EXT : constant VkStructureType := 1000143004;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_FEATURES_EXT : constant VkStructureType := 1000148000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_PROPERTIES_EXT : constant VkStructureType := 1000148001;
   VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_ADVANCED_STATE_CREATE_INFO_EXT : constant VkStructureType := 1000148002;
   VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_TO_COLOR_STATE_CREATE_INFO_NV : constant VkStructureType := 1000149000;
   VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_KHR : constant VkStructureType := 1000150007;
   VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_GEOMETRY_INFO_KHR : constant VkStructureType := 1000150000;
   VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_DEVICE_ADDRESS_INFO_KHR : constant VkStructureType := 1000150002;
   VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_AABBS_DATA_KHR : constant VkStructureType := 1000150003;
   VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_INSTANCES_DATA_KHR : constant VkStructureType := 1000150004;
   VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_TRIANGLES_DATA_KHR : constant VkStructureType := 1000150005;
   VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_KHR : constant VkStructureType := 1000150006;
   VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_VERSION_INFO_KHR : constant VkStructureType := 1000150009;
   VK_STRUCTURE_TYPE_COPY_ACCELERATION_STRUCTURE_INFO_KHR : constant VkStructureType := 1000150010;
   VK_STRUCTURE_TYPE_COPY_ACCELERATION_STRUCTURE_TO_MEMORY_INFO_KHR : constant VkStructureType := 1000150011;
   VK_STRUCTURE_TYPE_COPY_MEMORY_TO_ACCELERATION_STRUCTURE_INFO_KHR : constant VkStructureType := 1000150012;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ACCELERATION_STRUCTURE_FEATURES_KHR : constant VkStructureType := 1000150013;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ACCELERATION_STRUCTURE_PROPERTIES_KHR : constant VkStructureType := 1000150014;
   VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_KHR : constant VkStructureType := 1000150017;
   VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_SIZES_INFO_KHR : constant VkStructureType := 1000150020;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PIPELINE_FEATURES_KHR : constant VkStructureType := 1000347000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PIPELINE_PROPERTIES_KHR : constant VkStructureType := 1000347001;
   VK_STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_KHR : constant VkStructureType := 1000150015;
   VK_STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_KHR : constant VkStructureType := 1000150016;
   VK_STRUCTURE_TYPE_RAY_TRACING_PIPELINE_INTERFACE_CREATE_INFO_KHR : constant VkStructureType := 1000150018;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_QUERY_FEATURES_KHR : constant VkStructureType := 1000348013;
   VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_MODULATION_STATE_CREATE_INFO_NV : constant VkStructureType := 1000152000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SM_BUILTINS_FEATURES_NV : constant VkStructureType := 1000154000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SM_BUILTINS_PROPERTIES_NV : constant VkStructureType := 1000154001;
   VK_STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_LIST_EXT : constant VkStructureType := 1000158000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_DRM_FORMAT_MODIFIER_INFO_EXT : constant VkStructureType := 1000158002;
   VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_LIST_CREATE_INFO_EXT : constant VkStructureType := 1000158003;
   VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_EXPLICIT_CREATE_INFO_EXT : constant VkStructureType := 1000158004;
   VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT : constant VkStructureType := 1000158005;
   VK_STRUCTURE_TYPE_VALIDATION_CACHE_CREATE_INFO_EXT : constant VkStructureType := 1000160000;
   VK_STRUCTURE_TYPE_SHADER_MODULE_VALIDATION_CACHE_CREATE_INFO_EXT : constant VkStructureType := 1000160001;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PORTABILITY_SUBSET_FEATURES_KHR : constant VkStructureType := 1000163000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PORTABILITY_SUBSET_PROPERTIES_KHR : constant VkStructureType := 1000163001;
   VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SHADING_RATE_IMAGE_STATE_CREATE_INFO_NV : constant VkStructureType := 1000164000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_FEATURES_NV : constant VkStructureType := 1000164001;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_PROPERTIES_NV : constant VkStructureType := 1000164002;
   VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_COARSE_SAMPLE_ORDER_STATE_CREATE_INFO_NV : constant VkStructureType := 1000164005;
   VK_STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_NV : constant VkStructureType := 1000165000;
   VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_NV : constant VkStructureType := 1000165001;
   VK_STRUCTURE_TYPE_GEOMETRY_NV : constant VkStructureType := 1000165003;
   VK_STRUCTURE_TYPE_GEOMETRY_TRIANGLES_NV : constant VkStructureType := 1000165004;
   VK_STRUCTURE_TYPE_GEOMETRY_AABB_NV : constant VkStructureType := 1000165005;
   VK_STRUCTURE_TYPE_BIND_ACCELERATION_STRUCTURE_MEMORY_INFO_NV : constant VkStructureType := 1000165006;
   VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_NV : constant VkStructureType := 1000165007;
   VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_INFO_NV : constant VkStructureType := 1000165008;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_NV : constant VkStructureType := 1000165009;
   VK_STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_NV : constant VkStructureType := 1000165011;
   VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_INFO_NV : constant VkStructureType := 1000165012;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_REPRESENTATIVE_FRAGMENT_TEST_FEATURES_NV : constant VkStructureType := 1000166000;
   VK_STRUCTURE_TYPE_PIPELINE_REPRESENTATIVE_FRAGMENT_TEST_STATE_CREATE_INFO_NV : constant VkStructureType := 1000166001;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_VIEW_IMAGE_FORMAT_INFO_EXT : constant VkStructureType := 1000170000;
   VK_STRUCTURE_TYPE_FILTER_CUBIC_IMAGE_VIEW_IMAGE_FORMAT_PROPERTIES_EXT : constant VkStructureType := 1000170001;
   VK_STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_EXT : constant VkStructureType := 1000174000;
   VK_STRUCTURE_TYPE_IMPORT_MEMORY_HOST_POINTER_INFO_EXT : constant VkStructureType := 1000178000;
   VK_STRUCTURE_TYPE_MEMORY_HOST_POINTER_PROPERTIES_EXT : constant VkStructureType := 1000178001;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_HOST_PROPERTIES_EXT : constant VkStructureType := 1000178002;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CLOCK_FEATURES_KHR : constant VkStructureType := 1000181000;
   VK_STRUCTURE_TYPE_PIPELINE_COMPILER_CONTROL_CREATE_INFO_AMD : constant VkStructureType := 1000183000;
   VK_STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_EXT : constant VkStructureType := 1000184000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_AMD : constant VkStructureType := 1000185000;
   VK_STRUCTURE_TYPE_DEVICE_MEMORY_OVERALLOCATION_CREATE_INFO_AMD : constant VkStructureType := 1000189000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_EXT : constant VkStructureType := 1000190000;
   VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_EXT : constant VkStructureType := 1000190001;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_FEATURES_EXT : constant VkStructureType := 1000190002;
   VK_STRUCTURE_TYPE_PRESENT_FRAME_TOKEN_GGP : constant VkStructureType := 1000191000;
   VK_STRUCTURE_TYPE_PIPELINE_CREATION_FEEDBACK_CREATE_INFO_EXT : constant VkStructureType := 1000192000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_SHADER_DERIVATIVES_FEATURES_NV : constant VkStructureType := 1000201000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_FEATURES_NV : constant VkStructureType := 1000202000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_PROPERTIES_NV : constant VkStructureType := 1000202001;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_BARYCENTRIC_FEATURES_NV : constant VkStructureType := 1000203000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_IMAGE_FOOTPRINT_FEATURES_NV : constant VkStructureType := 1000204000;
   VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_EXCLUSIVE_SCISSOR_STATE_CREATE_INFO_NV : constant VkStructureType := 1000205000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXCLUSIVE_SCISSOR_FEATURES_NV : constant VkStructureType := 1000205002;
   VK_STRUCTURE_TYPE_CHECKPOINT_DATA_NV : constant VkStructureType := 1000206000;
   VK_STRUCTURE_TYPE_QUEUE_FAMILY_CHECKPOINT_PROPERTIES_NV : constant VkStructureType := 1000206001;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_INTEGER_FUNCTIONS_2_FEATURES_INTEL : constant VkStructureType := 1000209000;
   VK_STRUCTURE_TYPE_QUERY_POOL_PERFORMANCE_QUERY_CREATE_INFO_INTEL : constant VkStructureType := 1000210000;
   VK_STRUCTURE_TYPE_INITIALIZE_PERFORMANCE_API_INFO_INTEL : constant VkStructureType := 1000210001;
   VK_STRUCTURE_TYPE_PERFORMANCE_MARKER_INFO_INTEL : constant VkStructureType := 1000210002;
   VK_STRUCTURE_TYPE_PERFORMANCE_STREAM_MARKER_INFO_INTEL : constant VkStructureType := 1000210003;
   VK_STRUCTURE_TYPE_PERFORMANCE_OVERRIDE_INFO_INTEL : constant VkStructureType := 1000210004;
   VK_STRUCTURE_TYPE_PERFORMANCE_CONFIGURATION_ACQUIRE_INFO_INTEL : constant VkStructureType := 1000210005;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PCI_BUS_INFO_PROPERTIES_EXT : constant VkStructureType := 1000212000;
   VK_STRUCTURE_TYPE_DISPLAY_NATIVE_HDR_SURFACE_CAPABILITIES_AMD : constant VkStructureType := 1000213000;
   VK_STRUCTURE_TYPE_SWAPCHAIN_DISPLAY_NATIVE_HDR_CREATE_INFO_AMD : constant VkStructureType := 1000213001;
   VK_STRUCTURE_TYPE_IMAGEPIPE_SURFACE_CREATE_INFO_FUCHSIA : constant VkStructureType := 1000214000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_TERMINATE_INVOCATION_FEATURES_KHR : constant VkStructureType := 1000215000;
   VK_STRUCTURE_TYPE_METAL_SURFACE_CREATE_INFO_EXT : constant VkStructureType := 1000217000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_FEATURES_EXT : constant VkStructureType := 1000218000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_PROPERTIES_EXT : constant VkStructureType := 1000218001;
   VK_STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_CREATE_INFO_EXT : constant VkStructureType := 1000218002;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_SIZE_CONTROL_PROPERTIES_EXT : constant VkStructureType := 1000225000;
   VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_REQUIRED_SUBGROUP_SIZE_CREATE_INFO_EXT : constant VkStructureType := 1000225001;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_SIZE_CONTROL_FEATURES_EXT : constant VkStructureType := 1000225002;
   VK_STRUCTURE_TYPE_FRAGMENT_SHADING_RATE_ATTACHMENT_INFO_KHR : constant VkStructureType := 1000226000;
   VK_STRUCTURE_TYPE_PIPELINE_FRAGMENT_SHADING_RATE_STATE_CREATE_INFO_KHR : constant VkStructureType := 1000226001;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_PROPERTIES_KHR : constant VkStructureType := 1000226002;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_FEATURES_KHR : constant VkStructureType := 1000226003;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_KHR : constant VkStructureType := 1000226004;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_2_AMD : constant VkStructureType := 1000227000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COHERENT_MEMORY_FEATURES_AMD : constant VkStructureType := 1000229000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_IMAGE_ATOMIC_INT64_FEATURES_EXT : constant VkStructureType := 1000234000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT : constant VkStructureType := 1000237000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PRIORITY_FEATURES_EXT : constant VkStructureType := 1000238000;
   VK_STRUCTURE_TYPE_MEMORY_PRIORITY_ALLOCATE_INFO_EXT : constant VkStructureType := 1000238001;
   VK_STRUCTURE_TYPE_SURFACE_PROTECTED_CAPABILITIES_KHR : constant VkStructureType := 1000239000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEDICATED_ALLOCATION_IMAGE_ALIASING_FEATURES_NV : constant VkStructureType := 1000240000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_EXT : constant VkStructureType := 1000244000;
   VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_CREATE_INFO_EXT : constant VkStructureType := 1000244002;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TOOL_PROPERTIES_EXT : constant VkStructureType := 1000245000;
   VK_STRUCTURE_TYPE_VALIDATION_FEATURES_EXT : constant VkStructureType := 1000247000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_FEATURES_NV : constant VkStructureType := 1000249000;
   VK_STRUCTURE_TYPE_COOPERATIVE_MATRIX_PROPERTIES_NV : constant VkStructureType := 1000249001;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_PROPERTIES_NV : constant VkStructureType := 1000249002;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COVERAGE_REDUCTION_MODE_FEATURES_NV : constant VkStructureType := 1000250000;
   VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_REDUCTION_STATE_CREATE_INFO_NV : constant VkStructureType := 1000250001;
   VK_STRUCTURE_TYPE_FRAMEBUFFER_MIXED_SAMPLES_COMBINATION_NV : constant VkStructureType := 1000250002;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_INTERLOCK_FEATURES_EXT : constant VkStructureType := 1000251000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_IMAGE_ARRAYS_FEATURES_EXT : constant VkStructureType := 1000252000;
   VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_INFO_EXT : constant VkStructureType := 1000255000;
   VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_FULL_SCREEN_EXCLUSIVE_EXT : constant VkStructureType := 1000255002;
   VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_WIN32_INFO_EXT : constant VkStructureType := 1000255001;
   VK_STRUCTURE_TYPE_HEADLESS_SURFACE_CREATE_INFO_EXT : constant VkStructureType := 1000256000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_FEATURES_EXT : constant VkStructureType := 1000259000;
   VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_LINE_STATE_CREATE_INFO_EXT : constant VkStructureType := 1000259001;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_PROPERTIES_EXT : constant VkStructureType := 1000259002;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_FLOAT_FEATURES_EXT : constant VkStructureType := 1000260000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_INDEX_TYPE_UINT8_FEATURES_EXT : constant VkStructureType := 1000265000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_DYNAMIC_STATE_FEATURES_EXT : constant VkStructureType := 1000267000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_EXECUTABLE_PROPERTIES_FEATURES_KHR : constant VkStructureType := 1000269000;
   VK_STRUCTURE_TYPE_PIPELINE_INFO_KHR : constant VkStructureType := 1000269001;
   VK_STRUCTURE_TYPE_PIPELINE_EXECUTABLE_PROPERTIES_KHR : constant VkStructureType := 1000269002;
   VK_STRUCTURE_TYPE_PIPELINE_EXECUTABLE_INFO_KHR : constant VkStructureType := 1000269003;
   VK_STRUCTURE_TYPE_PIPELINE_EXECUTABLE_STATISTIC_KHR : constant VkStructureType := 1000269004;
   VK_STRUCTURE_TYPE_PIPELINE_EXECUTABLE_INTERNAL_REPRESENTATION_KHR : constant VkStructureType := 1000269005;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DEMOTE_TO_HELPER_INVOCATION_FEATURES_EXT : constant VkStructureType := 1000276000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_PROPERTIES_NV : constant VkStructureType := 1000277000;
   VK_STRUCTURE_TYPE_GRAPHICS_SHADER_GROUP_CREATE_INFO_NV : constant VkStructureType := 1000277001;
   VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_SHADER_GROUPS_CREATE_INFO_NV : constant VkStructureType := 1000277002;
   VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_TOKEN_NV : constant VkStructureType := 1000277003;
   VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NV : constant VkStructureType := 1000277004;
   VK_STRUCTURE_TYPE_GENERATED_COMMANDS_INFO_NV : constant VkStructureType := 1000277005;
   VK_STRUCTURE_TYPE_GENERATED_COMMANDS_MEMORY_REQUIREMENTS_INFO_NV : constant VkStructureType := 1000277006;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_FEATURES_NV : constant VkStructureType := 1000277007;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXEL_BUFFER_ALIGNMENT_FEATURES_EXT : constant VkStructureType := 1000281000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXEL_BUFFER_ALIGNMENT_PROPERTIES_EXT : constant VkStructureType := 1000281001;
   VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_RENDER_PASS_TRANSFORM_INFO_QCOM : constant VkStructureType := 1000282000;
   VK_STRUCTURE_TYPE_RENDER_PASS_TRANSFORM_BEGIN_INFO_QCOM : constant VkStructureType := 1000282001;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_MEMORY_REPORT_FEATURES_EXT : constant VkStructureType := 1000284000;
   VK_STRUCTURE_TYPE_DEVICE_DEVICE_MEMORY_REPORT_CREATE_INFO_EXT : constant VkStructureType := 1000284001;
   VK_STRUCTURE_TYPE_DEVICE_MEMORY_REPORT_CALLBACK_DATA_EXT : constant VkStructureType := 1000284002;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ROBUSTNESS_2_FEATURES_EXT : constant VkStructureType := 1000286000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ROBUSTNESS_2_PROPERTIES_EXT : constant VkStructureType := 1000286001;
   VK_STRUCTURE_TYPE_SAMPLER_CUSTOM_BORDER_COLOR_CREATE_INFO_EXT : constant VkStructureType := 1000287000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CUSTOM_BORDER_COLOR_PROPERTIES_EXT : constant VkStructureType := 1000287001;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CUSTOM_BORDER_COLOR_FEATURES_EXT : constant VkStructureType := 1000287002;
   VK_STRUCTURE_TYPE_PIPELINE_LIBRARY_CREATE_INFO_KHR : constant VkStructureType := 1000290000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PRIVATE_DATA_FEATURES_EXT : constant VkStructureType := 1000295000;
   VK_STRUCTURE_TYPE_DEVICE_PRIVATE_DATA_CREATE_INFO_EXT : constant VkStructureType := 1000295001;
   VK_STRUCTURE_TYPE_PRIVATE_DATA_SLOT_CREATE_INFO_EXT : constant VkStructureType := 1000295002;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_CREATION_CACHE_CONTROL_FEATURES_EXT : constant VkStructureType := 1000297000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DIAGNOSTICS_CONFIG_FEATURES_NV : constant VkStructureType := 1000300000;
   VK_STRUCTURE_TYPE_DEVICE_DIAGNOSTICS_CONFIG_CREATE_INFO_NV : constant VkStructureType := 1000300001;
   VK_STRUCTURE_TYPE_MEMORY_BARRIER_2_KHR : constant VkStructureType := 1000314000;
   VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER_2_KHR : constant VkStructureType := 1000314001;
   VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER_2_KHR : constant VkStructureType := 1000314002;
   VK_STRUCTURE_TYPE_DEPENDENCY_INFO_KHR : constant VkStructureType := 1000314003;
   VK_STRUCTURE_TYPE_SUBMIT_INFO_2_KHR : constant VkStructureType := 1000314004;
   VK_STRUCTURE_TYPE_SEMAPHORE_SUBMIT_INFO_KHR : constant VkStructureType := 1000314005;
   VK_STRUCTURE_TYPE_COMMAND_BUFFER_SUBMIT_INFO_KHR : constant VkStructureType := 1000314006;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SYNCHRONIZATION_2_FEATURES_KHR : constant VkStructureType := 1000314007;
   VK_STRUCTURE_TYPE_QUEUE_FAMILY_CHECKPOINT_PROPERTIES_2_NV : constant VkStructureType := 1000314008;
   VK_STRUCTURE_TYPE_CHECKPOINT_DATA_2_NV : constant VkStructureType := 1000314009;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ZERO_INITIALIZE_WORKGROUP_MEMORY_FEATURES_KHR : constant VkStructureType := 1000325000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_ENUMS_PROPERTIES_NV : constant VkStructureType := 1000326000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_ENUMS_FEATURES_NV : constant VkStructureType := 1000326001;
   VK_STRUCTURE_TYPE_PIPELINE_FRAGMENT_SHADING_RATE_ENUM_STATE_CREATE_INFO_NV : constant VkStructureType := 1000326002;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_2_FEATURES_EXT : constant VkStructureType := 1000332000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_2_PROPERTIES_EXT : constant VkStructureType := 1000332001;
   VK_STRUCTURE_TYPE_COPY_COMMAND_TRANSFORM_INFO_QCOM : constant VkStructureType := 1000333000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_ROBUSTNESS_FEATURES_EXT : constant VkStructureType := 1000335000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_WORKGROUP_MEMORY_EXPLICIT_LAYOUT_FEATURES_KHR : constant VkStructureType := 1000336000;
   VK_STRUCTURE_TYPE_COPY_BUFFER_INFO_2_KHR : constant VkStructureType := 1000337000;
   VK_STRUCTURE_TYPE_COPY_IMAGE_INFO_2_KHR : constant VkStructureType := 1000337001;
   VK_STRUCTURE_TYPE_COPY_BUFFER_TO_IMAGE_INFO_2_KHR : constant VkStructureType := 1000337002;
   VK_STRUCTURE_TYPE_COPY_IMAGE_TO_BUFFER_INFO_2_KHR : constant VkStructureType := 1000337003;
   VK_STRUCTURE_TYPE_BLIT_IMAGE_INFO_2_KHR : constant VkStructureType := 1000337004;
   VK_STRUCTURE_TYPE_RESOLVE_IMAGE_INFO_2_KHR : constant VkStructureType := 1000337005;
   VK_STRUCTURE_TYPE_BUFFER_COPY_2_KHR : constant VkStructureType := 1000337006;
   VK_STRUCTURE_TYPE_IMAGE_COPY_2_KHR : constant VkStructureType := 1000337007;
   VK_STRUCTURE_TYPE_IMAGE_BLIT_2_KHR : constant VkStructureType := 1000337008;
   VK_STRUCTURE_TYPE_BUFFER_IMAGE_COPY_2_KHR : constant VkStructureType := 1000337009;
   VK_STRUCTURE_TYPE_IMAGE_RESOLVE_2_KHR : constant VkStructureType := 1000337010;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_4444_FORMATS_FEATURES_EXT : constant VkStructureType := 1000340000;
   VK_STRUCTURE_TYPE_DIRECTFB_SURFACE_CREATE_INFO_EXT : constant VkStructureType := 1000346000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MUTABLE_DESCRIPTOR_TYPE_FEATURES_VALVE : constant VkStructureType := 1000351000;
   VK_STRUCTURE_TYPE_MUTABLE_DESCRIPTOR_TYPE_CREATE_INFO_VALVE : constant VkStructureType := 1000351002;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES : constant VkStructureType := 1000120000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETER_FEATURES : constant VkStructureType := 1000063000;
   VK_STRUCTURE_TYPE_DEBUG_REPORT_CREATE_INFO_EXT : constant VkStructureType := 1000011000;
   VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO_KHR : constant VkStructureType := 1000053000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES_KHR : constant VkStructureType := 1000053001;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES_KHR : constant VkStructureType := 1000053002;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2_KHR : constant VkStructureType := 1000059000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2_KHR : constant VkStructureType := 1000059001;
   VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2_KHR : constant VkStructureType := 1000059002;
   VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2_KHR : constant VkStructureType := 1000059003;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2_KHR : constant VkStructureType := 1000059004;
   VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2_KHR : constant VkStructureType := 1000059005;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2_KHR : constant VkStructureType := 1000059006;
   VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2_KHR : constant VkStructureType := 1000059007;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2_KHR : constant VkStructureType := 1000059008;
   VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO_KHR : constant VkStructureType := 1000060000;
   VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO_KHR : constant VkStructureType := 1000060003;
   VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO_KHR : constant VkStructureType := 1000060004;
   VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO_KHR : constant VkStructureType := 1000060005;
   VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO_KHR : constant VkStructureType := 1000060006;
   VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO_KHR : constant VkStructureType := 1000060013;
   VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO_KHR : constant VkStructureType := 1000060014;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES_KHR : constant VkStructureType := 1000070000;
   VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO_KHR : constant VkStructureType := 1000070001;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO_KHR : constant VkStructureType := 1000071000;
   VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES_KHR : constant VkStructureType := 1000071001;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO_KHR : constant VkStructureType := 1000071002;
   VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES_KHR : constant VkStructureType := 1000071003;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES_KHR : constant VkStructureType := 1000071004;
   VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO_KHR : constant VkStructureType := 1000072000;
   VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_KHR : constant VkStructureType := 1000072001;
   VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_KHR : constant VkStructureType := 1000072002;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO_KHR : constant VkStructureType := 1000076000;
   VK_STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES_KHR : constant VkStructureType := 1000076001;
   VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO_KHR : constant VkStructureType := 1000077000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_FLOAT16_INT8_FEATURES_KHR : constant VkStructureType := 1000082000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT16_INT8_FEATURES_KHR : constant VkStructureType := 1000082000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES_KHR : constant VkStructureType := 1000083000;
   VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO_KHR : constant VkStructureType := 1000085000;
   VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES2_EXT : constant VkStructureType := 1000090000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGELESS_FRAMEBUFFER_FEATURES_KHR : constant VkStructureType := 1000108000;
   VK_STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENTS_CREATE_INFO_KHR : constant VkStructureType := 1000108001;
   VK_STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENT_IMAGE_INFO_KHR : constant VkStructureType := 1000108002;
   VK_STRUCTURE_TYPE_RENDER_PASS_ATTACHMENT_BEGIN_INFO_KHR : constant VkStructureType := 1000108003;
   VK_STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_2_KHR : constant VkStructureType := 1000109000;
   VK_STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2_KHR : constant VkStructureType := 1000109001;
   VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_2_KHR : constant VkStructureType := 1000109002;
   VK_STRUCTURE_TYPE_SUBPASS_DEPENDENCY_2_KHR : constant VkStructureType := 1000109003;
   VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO_2_KHR : constant VkStructureType := 1000109004;
   VK_STRUCTURE_TYPE_SUBPASS_BEGIN_INFO_KHR : constant VkStructureType := 1000109005;
   VK_STRUCTURE_TYPE_SUBPASS_END_INFO_KHR : constant VkStructureType := 1000109006;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO_KHR : constant VkStructureType := 1000112000;
   VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES_KHR : constant VkStructureType := 1000112001;
   VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO_KHR : constant VkStructureType := 1000113000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES_KHR : constant VkStructureType := 1000117000;
   VK_STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO_KHR : constant VkStructureType := 1000117001;
   VK_STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO_KHR : constant VkStructureType := 1000117002;
   VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO_KHR : constant VkStructureType := 1000117003;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES_KHR : constant VkStructureType := 1000120000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES_KHR : constant VkStructureType := 1000120000;
   VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS_KHR : constant VkStructureType := 1000127000;
   VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO_KHR : constant VkStructureType := 1000127001;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES_EXT : constant VkStructureType := 1000130000;
   VK_STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO_EXT : constant VkStructureType := 1000130001;
   VK_STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2_KHR : constant VkStructureType := 1000146000;
   VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2_KHR : constant VkStructureType := 1000146001;
   VK_STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2_KHR : constant VkStructureType := 1000146002;
   VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2_KHR : constant VkStructureType := 1000146003;
   VK_STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2_KHR : constant VkStructureType := 1000146004;
   VK_STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO_KHR : constant VkStructureType := 1000147000;
   VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO_KHR : constant VkStructureType := 1000156000;
   VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO_KHR : constant VkStructureType := 1000156001;
   VK_STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO_KHR : constant VkStructureType := 1000156002;
   VK_STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO_KHR : constant VkStructureType := 1000156003;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES_KHR : constant VkStructureType := 1000156004;
   VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES_KHR : constant VkStructureType := 1000156005;
   VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO_KHR : constant VkStructureType := 1000157000;
   VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO_KHR : constant VkStructureType := 1000157001;
   VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO_EXT : constant VkStructureType := 1000161000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES_EXT : constant VkStructureType := 1000161001;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES_EXT : constant VkStructureType := 1000161002;
   VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO_EXT : constant VkStructureType := 1000161003;
   VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT_EXT : constant VkStructureType := 1000161004;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES_KHR : constant VkStructureType := 1000168000;
   VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT_KHR : constant VkStructureType := 1000168001;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SUBGROUP_EXTENDED_TYPES_FEATURES_KHR : constant VkStructureType := 1000175000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_8BIT_STORAGE_FEATURES_KHR : constant VkStructureType := 1000177000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_INT64_FEATURES_KHR : constant VkStructureType := 1000180000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DRIVER_PROPERTIES_KHR : constant VkStructureType := 1000196000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES_KHR : constant VkStructureType := 1000197000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_STENCIL_RESOLVE_PROPERTIES_KHR : constant VkStructureType := 1000199000;
   VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_DEPTH_STENCIL_RESOLVE_KHR : constant VkStructureType := 1000199001;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_FEATURES_KHR : constant VkStructureType := 1000207000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_PROPERTIES_KHR : constant VkStructureType := 1000207001;
   VK_STRUCTURE_TYPE_SEMAPHORE_TYPE_CREATE_INFO_KHR : constant VkStructureType := 1000207002;
   VK_STRUCTURE_TYPE_TIMELINE_SEMAPHORE_SUBMIT_INFO_KHR : constant VkStructureType := 1000207003;
   VK_STRUCTURE_TYPE_SEMAPHORE_WAIT_INFO_KHR : constant VkStructureType := 1000207004;
   VK_STRUCTURE_TYPE_SEMAPHORE_SIGNAL_INFO_KHR : constant VkStructureType := 1000207005;
   VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO_INTEL : constant VkStructureType := 1000210000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_MEMORY_MODEL_FEATURES_KHR : constant VkStructureType := 1000211000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES_EXT : constant VkStructureType := 1000221000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SEPARATE_DEPTH_STENCIL_LAYOUTS_FEATURES_KHR : constant VkStructureType := 1000241000;
   VK_STRUCTURE_TYPE_ATTACHMENT_REFERENCE_STENCIL_LAYOUT_KHR : constant VkStructureType := 1000241001;
   VK_STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_STENCIL_LAYOUT_KHR : constant VkStructureType := 1000241002;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_ADDRESS_FEATURES_EXT : constant VkStructureType := 1000244000;
   VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO_EXT : constant VkStructureType := 1000244001;
   VK_STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO_EXT : constant VkStructureType := 1000246000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_UNIFORM_BUFFER_STANDARD_LAYOUT_FEATURES_KHR : constant VkStructureType := 1000253000;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_KHR : constant VkStructureType := 1000257000;
   VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO_KHR : constant VkStructureType := 1000244001;
   VK_STRUCTURE_TYPE_BUFFER_OPAQUE_CAPTURE_ADDRESS_CREATE_INFO_KHR : constant VkStructureType := 1000257002;
   VK_STRUCTURE_TYPE_MEMORY_OPAQUE_CAPTURE_ADDRESS_ALLOCATE_INFO_KHR : constant VkStructureType := 1000257003;
   VK_STRUCTURE_TYPE_DEVICE_MEMORY_OPAQUE_CAPTURE_ADDRESS_INFO_KHR : constant VkStructureType := 1000257004;
   VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES_EXT : constant VkStructureType := 1000261000;
   VK_STRUCTURE_TYPE_MAX_ENUM : constant VkStructureType := 2147483647;  -- vulkan_core.h:151

   subtype VkImageLayout is unsigned;
   VK_IMAGE_LAYOUT_UNDEFINED : constant VkImageLayout := 0;
   VK_IMAGE_LAYOUT_GENERAL : constant VkImageLayout := 1;
   VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL : constant VkImageLayout := 2;
   VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL : constant VkImageLayout := 3;
   VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL : constant VkImageLayout := 4;
   VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL : constant VkImageLayout := 5;
   VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL : constant VkImageLayout := 6;
   VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL : constant VkImageLayout := 7;
   VK_IMAGE_LAYOUT_PREINITIALIZED : constant VkImageLayout := 8;
   VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL : constant VkImageLayout := 1000117000;
   VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL : constant VkImageLayout := 1000117001;
   VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL : constant VkImageLayout := 1000241000;
   VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL : constant VkImageLayout := 1000241001;
   VK_IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL : constant VkImageLayout := 1000241002;
   VK_IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL : constant VkImageLayout := 1000241003;
   VK_IMAGE_LAYOUT_PRESENT_SRC_KHR : constant VkImageLayout := 1000001002;
   VK_IMAGE_LAYOUT_SHARED_PRESENT_KHR : constant VkImageLayout := 1000111000;
   VK_IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV : constant VkImageLayout := 1000164003;
   VK_IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT : constant VkImageLayout := 1000218000;
   VK_IMAGE_LAYOUT_READ_ONLY_OPTIMAL_KHR : constant VkImageLayout := 1000314000;
   VK_IMAGE_LAYOUT_ATTACHMENT_OPTIMAL_KHR : constant VkImageLayout := 1000314001;
   VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL_KHR : constant VkImageLayout := 1000117000;
   VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL_KHR : constant VkImageLayout := 1000117001;
   VK_IMAGE_LAYOUT_FRAGMENT_SHADING_RATE_ATTACHMENT_OPTIMAL_KHR : constant VkImageLayout := 1000164003;
   VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL_KHR : constant VkImageLayout := 1000241000;
   VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL_KHR : constant VkImageLayout := 1000241001;
   VK_IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL_KHR : constant VkImageLayout := 1000241002;
   VK_IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL_KHR : constant VkImageLayout := 1000241003;
   VK_IMAGE_LAYOUT_MAX_ENUM : constant VkImageLayout := 2147483647;  -- vulkan_core.h:754

   subtype VkObjectType is unsigned;
   VK_OBJECT_TYPE_UNKNOWN : constant VkObjectType := 0;
   VK_OBJECT_TYPE_INSTANCE : constant VkObjectType := 1;
   VK_OBJECT_TYPE_PHYSICAL_DEVICE : constant VkObjectType := 2;
   VK_OBJECT_TYPE_DEVICE : constant VkObjectType := 3;
   VK_OBJECT_TYPE_QUEUE : constant VkObjectType := 4;
   VK_OBJECT_TYPE_SEMAPHORE : constant VkObjectType := 5;
   VK_OBJECT_TYPE_COMMAND_BUFFER : constant VkObjectType := 6;
   VK_OBJECT_TYPE_FENCE : constant VkObjectType := 7;
   VK_OBJECT_TYPE_DEVICE_MEMORY : constant VkObjectType := 8;
   VK_OBJECT_TYPE_BUFFER : constant VkObjectType := 9;
   VK_OBJECT_TYPE_IMAGE : constant VkObjectType := 10;
   VK_OBJECT_TYPE_EVENT : constant VkObjectType := 11;
   VK_OBJECT_TYPE_QUERY_POOL : constant VkObjectType := 12;
   VK_OBJECT_TYPE_BUFFER_VIEW : constant VkObjectType := 13;
   VK_OBJECT_TYPE_IMAGE_VIEW : constant VkObjectType := 14;
   VK_OBJECT_TYPE_SHADER_MODULE : constant VkObjectType := 15;
   VK_OBJECT_TYPE_PIPELINE_CACHE : constant VkObjectType := 16;
   VK_OBJECT_TYPE_PIPELINE_LAYOUT : constant VkObjectType := 17;
   VK_OBJECT_TYPE_RENDER_PASS : constant VkObjectType := 18;
   VK_OBJECT_TYPE_PIPELINE : constant VkObjectType := 19;
   VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT : constant VkObjectType := 20;
   VK_OBJECT_TYPE_SAMPLER : constant VkObjectType := 21;
   VK_OBJECT_TYPE_DESCRIPTOR_POOL : constant VkObjectType := 22;
   VK_OBJECT_TYPE_DESCRIPTOR_SET : constant VkObjectType := 23;
   VK_OBJECT_TYPE_FRAMEBUFFER : constant VkObjectType := 24;
   VK_OBJECT_TYPE_COMMAND_POOL : constant VkObjectType := 25;
   VK_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION : constant VkObjectType := 1000156000;
   VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE : constant VkObjectType := 1000085000;
   VK_OBJECT_TYPE_SURFACE_KHR : constant VkObjectType := 1000000000;
   VK_OBJECT_TYPE_SWAPCHAIN_KHR : constant VkObjectType := 1000001000;
   VK_OBJECT_TYPE_DISPLAY_KHR : constant VkObjectType := 1000002000;
   VK_OBJECT_TYPE_DISPLAY_MODE_KHR : constant VkObjectType := 1000002001;
   VK_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT : constant VkObjectType := 1000011000;
   VK_OBJECT_TYPE_DEBUG_UTILS_MESSENGER_EXT : constant VkObjectType := 1000128000;
   VK_OBJECT_TYPE_ACCELERATION_STRUCTURE_KHR : constant VkObjectType := 1000150000;
   VK_OBJECT_TYPE_VALIDATION_CACHE_EXT : constant VkObjectType := 1000160000;
   VK_OBJECT_TYPE_ACCELERATION_STRUCTURE_NV : constant VkObjectType := 1000165000;
   VK_OBJECT_TYPE_PERFORMANCE_CONFIGURATION_INTEL : constant VkObjectType := 1000210000;
   VK_OBJECT_TYPE_DEFERRED_OPERATION_KHR : constant VkObjectType := 1000268000;
   VK_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NV : constant VkObjectType := 1000277000;
   VK_OBJECT_TYPE_PRIVATE_DATA_SLOT_EXT : constant VkObjectType := 1000295000;
   VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR : constant VkObjectType := 1000085000;
   VK_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION_KHR : constant VkObjectType := 1000156000;
   VK_OBJECT_TYPE_MAX_ENUM : constant VkObjectType := 2147483647;  -- vulkan_core.h:786

   subtype VkVendorId is unsigned;
   VK_VENDOR_ID_VIV : constant VkVendorId := 65537;
   VK_VENDOR_ID_VSI : constant VkVendorId := 65538;
   VK_VENDOR_ID_KAZAN : constant VkVendorId := 65539;
   VK_VENDOR_ID_CODEPLAY : constant VkVendorId := 65540;
   VK_VENDOR_ID_MESA : constant VkVendorId := 65541;
   VK_VENDOR_ID_POCL : constant VkVendorId := 65542;
   VK_VENDOR_ID_MAX_ENUM : constant VkVendorId := 2147483647;  -- vulkan_core.h:833

   subtype VkPipelineCacheHeaderVersion is unsigned;
   VK_PIPELINE_CACHE_HEADER_VERSION_ONE : constant VkPipelineCacheHeaderVersion := 1;
   VK_PIPELINE_CACHE_HEADER_VERSION_MAX_ENUM : constant VkPipelineCacheHeaderVersion := 2147483647;  -- vulkan_core.h:843

   subtype VkSystemAllocationScope is unsigned;
   VK_SYSTEM_ALLOCATION_SCOPE_COMMAND : constant VkSystemAllocationScope := 0;
   VK_SYSTEM_ALLOCATION_SCOPE_OBJECT : constant VkSystemAllocationScope := 1;
   VK_SYSTEM_ALLOCATION_SCOPE_CACHE : constant VkSystemAllocationScope := 2;
   VK_SYSTEM_ALLOCATION_SCOPE_DEVICE : constant VkSystemAllocationScope := 3;
   VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE : constant VkSystemAllocationScope := 4;
   VK_SYSTEM_ALLOCATION_SCOPE_MAX_ENUM : constant VkSystemAllocationScope := 2147483647;  -- vulkan_core.h:848

   subtype VkInternalAllocationType is unsigned;
   VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE : constant VkInternalAllocationType := 0;
   VK_INTERNAL_ALLOCATION_TYPE_MAX_ENUM : constant VkInternalAllocationType := 2147483647;  -- vulkan_core.h:857

   subtype VkFormat is unsigned;
   VK_FORMAT_UNDEFINED : constant VkFormat := 0;
   VK_FORMAT_R4G4_UNORM_PACK8 : constant VkFormat := 1;
   VK_FORMAT_R4G4B4A4_UNORM_PACK16 : constant VkFormat := 2;
   VK_FORMAT_B4G4R4A4_UNORM_PACK16 : constant VkFormat := 3;
   VK_FORMAT_R5G6B5_UNORM_PACK16 : constant VkFormat := 4;
   VK_FORMAT_B5G6R5_UNORM_PACK16 : constant VkFormat := 5;
   VK_FORMAT_R5G5B5A1_UNORM_PACK16 : constant VkFormat := 6;
   VK_FORMAT_B5G5R5A1_UNORM_PACK16 : constant VkFormat := 7;
   VK_FORMAT_A1R5G5B5_UNORM_PACK16 : constant VkFormat := 8;
   VK_FORMAT_R8_UNORM : constant VkFormat := 9;
   VK_FORMAT_R8_SNORM : constant VkFormat := 10;
   VK_FORMAT_R8_USCALED : constant VkFormat := 11;
   VK_FORMAT_R8_SSCALED : constant VkFormat := 12;
   VK_FORMAT_R8_UINT : constant VkFormat := 13;
   VK_FORMAT_R8_SINT : constant VkFormat := 14;
   VK_FORMAT_R8_SRGB : constant VkFormat := 15;
   VK_FORMAT_R8G8_UNORM : constant VkFormat := 16;
   VK_FORMAT_R8G8_SNORM : constant VkFormat := 17;
   VK_FORMAT_R8G8_USCALED : constant VkFormat := 18;
   VK_FORMAT_R8G8_SSCALED : constant VkFormat := 19;
   VK_FORMAT_R8G8_UINT : constant VkFormat := 20;
   VK_FORMAT_R8G8_SINT : constant VkFormat := 21;
   VK_FORMAT_R8G8_SRGB : constant VkFormat := 22;
   VK_FORMAT_R8G8B8_UNORM : constant VkFormat := 23;
   VK_FORMAT_R8G8B8_SNORM : constant VkFormat := 24;
   VK_FORMAT_R8G8B8_USCALED : constant VkFormat := 25;
   VK_FORMAT_R8G8B8_SSCALED : constant VkFormat := 26;
   VK_FORMAT_R8G8B8_UINT : constant VkFormat := 27;
   VK_FORMAT_R8G8B8_SINT : constant VkFormat := 28;
   VK_FORMAT_R8G8B8_SRGB : constant VkFormat := 29;
   VK_FORMAT_B8G8R8_UNORM : constant VkFormat := 30;
   VK_FORMAT_B8G8R8_SNORM : constant VkFormat := 31;
   VK_FORMAT_B8G8R8_USCALED : constant VkFormat := 32;
   VK_FORMAT_B8G8R8_SSCALED : constant VkFormat := 33;
   VK_FORMAT_B8G8R8_UINT : constant VkFormat := 34;
   VK_FORMAT_B8G8R8_SINT : constant VkFormat := 35;
   VK_FORMAT_B8G8R8_SRGB : constant VkFormat := 36;
   VK_FORMAT_R8G8B8A8_UNORM : constant VkFormat := 37;
   VK_FORMAT_R8G8B8A8_SNORM : constant VkFormat := 38;
   VK_FORMAT_R8G8B8A8_USCALED : constant VkFormat := 39;
   VK_FORMAT_R8G8B8A8_SSCALED : constant VkFormat := 40;
   VK_FORMAT_R8G8B8A8_UINT : constant VkFormat := 41;
   VK_FORMAT_R8G8B8A8_SINT : constant VkFormat := 42;
   VK_FORMAT_R8G8B8A8_SRGB : constant VkFormat := 43;
   VK_FORMAT_B8G8R8A8_UNORM : constant VkFormat := 44;
   VK_FORMAT_B8G8R8A8_SNORM : constant VkFormat := 45;
   VK_FORMAT_B8G8R8A8_USCALED : constant VkFormat := 46;
   VK_FORMAT_B8G8R8A8_SSCALED : constant VkFormat := 47;
   VK_FORMAT_B8G8R8A8_UINT : constant VkFormat := 48;
   VK_FORMAT_B8G8R8A8_SINT : constant VkFormat := 49;
   VK_FORMAT_B8G8R8A8_SRGB : constant VkFormat := 50;
   VK_FORMAT_A8B8G8R8_UNORM_PACK32 : constant VkFormat := 51;
   VK_FORMAT_A8B8G8R8_SNORM_PACK32 : constant VkFormat := 52;
   VK_FORMAT_A8B8G8R8_USCALED_PACK32 : constant VkFormat := 53;
   VK_FORMAT_A8B8G8R8_SSCALED_PACK32 : constant VkFormat := 54;
   VK_FORMAT_A8B8G8R8_UINT_PACK32 : constant VkFormat := 55;
   VK_FORMAT_A8B8G8R8_SINT_PACK32 : constant VkFormat := 56;
   VK_FORMAT_A8B8G8R8_SRGB_PACK32 : constant VkFormat := 57;
   VK_FORMAT_A2R10G10B10_UNORM_PACK32 : constant VkFormat := 58;
   VK_FORMAT_A2R10G10B10_SNORM_PACK32 : constant VkFormat := 59;
   VK_FORMAT_A2R10G10B10_USCALED_PACK32 : constant VkFormat := 60;
   VK_FORMAT_A2R10G10B10_SSCALED_PACK32 : constant VkFormat := 61;
   VK_FORMAT_A2R10G10B10_UINT_PACK32 : constant VkFormat := 62;
   VK_FORMAT_A2R10G10B10_SINT_PACK32 : constant VkFormat := 63;
   VK_FORMAT_A2B10G10R10_UNORM_PACK32 : constant VkFormat := 64;
   VK_FORMAT_A2B10G10R10_SNORM_PACK32 : constant VkFormat := 65;
   VK_FORMAT_A2B10G10R10_USCALED_PACK32 : constant VkFormat := 66;
   VK_FORMAT_A2B10G10R10_SSCALED_PACK32 : constant VkFormat := 67;
   VK_FORMAT_A2B10G10R10_UINT_PACK32 : constant VkFormat := 68;
   VK_FORMAT_A2B10G10R10_SINT_PACK32 : constant VkFormat := 69;
   VK_FORMAT_R16_UNORM : constant VkFormat := 70;
   VK_FORMAT_R16_SNORM : constant VkFormat := 71;
   VK_FORMAT_R16_USCALED : constant VkFormat := 72;
   VK_FORMAT_R16_SSCALED : constant VkFormat := 73;
   VK_FORMAT_R16_UINT : constant VkFormat := 74;
   VK_FORMAT_R16_SINT : constant VkFormat := 75;
   VK_FORMAT_R16_SFLOAT : constant VkFormat := 76;
   VK_FORMAT_R16G16_UNORM : constant VkFormat := 77;
   VK_FORMAT_R16G16_SNORM : constant VkFormat := 78;
   VK_FORMAT_R16G16_USCALED : constant VkFormat := 79;
   VK_FORMAT_R16G16_SSCALED : constant VkFormat := 80;
   VK_FORMAT_R16G16_UINT : constant VkFormat := 81;
   VK_FORMAT_R16G16_SINT : constant VkFormat := 82;
   VK_FORMAT_R16G16_SFLOAT : constant VkFormat := 83;
   VK_FORMAT_R16G16B16_UNORM : constant VkFormat := 84;
   VK_FORMAT_R16G16B16_SNORM : constant VkFormat := 85;
   VK_FORMAT_R16G16B16_USCALED : constant VkFormat := 86;
   VK_FORMAT_R16G16B16_SSCALED : constant VkFormat := 87;
   VK_FORMAT_R16G16B16_UINT : constant VkFormat := 88;
   VK_FORMAT_R16G16B16_SINT : constant VkFormat := 89;
   VK_FORMAT_R16G16B16_SFLOAT : constant VkFormat := 90;
   VK_FORMAT_R16G16B16A16_UNORM : constant VkFormat := 91;
   VK_FORMAT_R16G16B16A16_SNORM : constant VkFormat := 92;
   VK_FORMAT_R16G16B16A16_USCALED : constant VkFormat := 93;
   VK_FORMAT_R16G16B16A16_SSCALED : constant VkFormat := 94;
   VK_FORMAT_R16G16B16A16_UINT : constant VkFormat := 95;
   VK_FORMAT_R16G16B16A16_SINT : constant VkFormat := 96;
   VK_FORMAT_R16G16B16A16_SFLOAT : constant VkFormat := 97;
   VK_FORMAT_R32_UINT : constant VkFormat := 98;
   VK_FORMAT_R32_SINT : constant VkFormat := 99;
   VK_FORMAT_R32_SFLOAT : constant VkFormat := 100;
   VK_FORMAT_R32G32_UINT : constant VkFormat := 101;
   VK_FORMAT_R32G32_SINT : constant VkFormat := 102;
   VK_FORMAT_R32G32_SFLOAT : constant VkFormat := 103;
   VK_FORMAT_R32G32B32_UINT : constant VkFormat := 104;
   VK_FORMAT_R32G32B32_SINT : constant VkFormat := 105;
   VK_FORMAT_R32G32B32_SFLOAT : constant VkFormat := 106;
   VK_FORMAT_R32G32B32A32_UINT : constant VkFormat := 107;
   VK_FORMAT_R32G32B32A32_SINT : constant VkFormat := 108;
   VK_FORMAT_R32G32B32A32_SFLOAT : constant VkFormat := 109;
   VK_FORMAT_R64_UINT : constant VkFormat := 110;
   VK_FORMAT_R64_SINT : constant VkFormat := 111;
   VK_FORMAT_R64_SFLOAT : constant VkFormat := 112;
   VK_FORMAT_R64G64_UINT : constant VkFormat := 113;
   VK_FORMAT_R64G64_SINT : constant VkFormat := 114;
   VK_FORMAT_R64G64_SFLOAT : constant VkFormat := 115;
   VK_FORMAT_R64G64B64_UINT : constant VkFormat := 116;
   VK_FORMAT_R64G64B64_SINT : constant VkFormat := 117;
   VK_FORMAT_R64G64B64_SFLOAT : constant VkFormat := 118;
   VK_FORMAT_R64G64B64A64_UINT : constant VkFormat := 119;
   VK_FORMAT_R64G64B64A64_SINT : constant VkFormat := 120;
   VK_FORMAT_R64G64B64A64_SFLOAT : constant VkFormat := 121;
   VK_FORMAT_B10G11R11_UFLOAT_PACK32 : constant VkFormat := 122;
   VK_FORMAT_E5B9G9R9_UFLOAT_PACK32 : constant VkFormat := 123;
   VK_FORMAT_D16_UNORM : constant VkFormat := 124;
   VK_FORMAT_X8_D24_UNORM_PACK32 : constant VkFormat := 125;
   VK_FORMAT_D32_SFLOAT : constant VkFormat := 126;
   VK_FORMAT_S8_UINT : constant VkFormat := 127;
   VK_FORMAT_D16_UNORM_S8_UINT : constant VkFormat := 128;
   VK_FORMAT_D24_UNORM_S8_UINT : constant VkFormat := 129;
   VK_FORMAT_D32_SFLOAT_S8_UINT : constant VkFormat := 130;
   VK_FORMAT_BC1_RGB_UNORM_BLOCK : constant VkFormat := 131;
   VK_FORMAT_BC1_RGB_SRGB_BLOCK : constant VkFormat := 132;
   VK_FORMAT_BC1_RGBA_UNORM_BLOCK : constant VkFormat := 133;
   VK_FORMAT_BC1_RGBA_SRGB_BLOCK : constant VkFormat := 134;
   VK_FORMAT_BC2_UNORM_BLOCK : constant VkFormat := 135;
   VK_FORMAT_BC2_SRGB_BLOCK : constant VkFormat := 136;
   VK_FORMAT_BC3_UNORM_BLOCK : constant VkFormat := 137;
   VK_FORMAT_BC3_SRGB_BLOCK : constant VkFormat := 138;
   VK_FORMAT_BC4_UNORM_BLOCK : constant VkFormat := 139;
   VK_FORMAT_BC4_SNORM_BLOCK : constant VkFormat := 140;
   VK_FORMAT_BC5_UNORM_BLOCK : constant VkFormat := 141;
   VK_FORMAT_BC5_SNORM_BLOCK : constant VkFormat := 142;
   VK_FORMAT_BC6H_UFLOAT_BLOCK : constant VkFormat := 143;
   VK_FORMAT_BC6H_SFLOAT_BLOCK : constant VkFormat := 144;
   VK_FORMAT_BC7_UNORM_BLOCK : constant VkFormat := 145;
   VK_FORMAT_BC7_SRGB_BLOCK : constant VkFormat := 146;
   VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK : constant VkFormat := 147;
   VK_FORMAT_ETC2_R8G8B8_SRGB_BLOCK : constant VkFormat := 148;
   VK_FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK : constant VkFormat := 149;
   VK_FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK : constant VkFormat := 150;
   VK_FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK : constant VkFormat := 151;
   VK_FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK : constant VkFormat := 152;
   VK_FORMAT_EAC_R11_UNORM_BLOCK : constant VkFormat := 153;
   VK_FORMAT_EAC_R11_SNORM_BLOCK : constant VkFormat := 154;
   VK_FORMAT_EAC_R11G11_UNORM_BLOCK : constant VkFormat := 155;
   VK_FORMAT_EAC_R11G11_SNORM_BLOCK : constant VkFormat := 156;
   VK_FORMAT_ASTC_4x4_UNORM_BLOCK : constant VkFormat := 157;
   VK_FORMAT_ASTC_4x4_SRGB_BLOCK : constant VkFormat := 158;
   VK_FORMAT_ASTC_5x4_UNORM_BLOCK : constant VkFormat := 159;
   VK_FORMAT_ASTC_5x4_SRGB_BLOCK : constant VkFormat := 160;
   VK_FORMAT_ASTC_5x5_UNORM_BLOCK : constant VkFormat := 161;
   VK_FORMAT_ASTC_5x5_SRGB_BLOCK : constant VkFormat := 162;
   VK_FORMAT_ASTC_6x5_UNORM_BLOCK : constant VkFormat := 163;
   VK_FORMAT_ASTC_6x5_SRGB_BLOCK : constant VkFormat := 164;
   VK_FORMAT_ASTC_6x6_UNORM_BLOCK : constant VkFormat := 165;
   VK_FORMAT_ASTC_6x6_SRGB_BLOCK : constant VkFormat := 166;
   VK_FORMAT_ASTC_8x5_UNORM_BLOCK : constant VkFormat := 167;
   VK_FORMAT_ASTC_8x5_SRGB_BLOCK : constant VkFormat := 168;
   VK_FORMAT_ASTC_8x6_UNORM_BLOCK : constant VkFormat := 169;
   VK_FORMAT_ASTC_8x6_SRGB_BLOCK : constant VkFormat := 170;
   VK_FORMAT_ASTC_8x8_UNORM_BLOCK : constant VkFormat := 171;
   VK_FORMAT_ASTC_8x8_SRGB_BLOCK : constant VkFormat := 172;
   VK_FORMAT_ASTC_10x5_UNORM_BLOCK : constant VkFormat := 173;
   VK_FORMAT_ASTC_10x5_SRGB_BLOCK : constant VkFormat := 174;
   VK_FORMAT_ASTC_10x6_UNORM_BLOCK : constant VkFormat := 175;
   VK_FORMAT_ASTC_10x6_SRGB_BLOCK : constant VkFormat := 176;
   VK_FORMAT_ASTC_10x8_UNORM_BLOCK : constant VkFormat := 177;
   VK_FORMAT_ASTC_10x8_SRGB_BLOCK : constant VkFormat := 178;
   VK_FORMAT_ASTC_10x10_UNORM_BLOCK : constant VkFormat := 179;
   VK_FORMAT_ASTC_10x10_SRGB_BLOCK : constant VkFormat := 180;
   VK_FORMAT_ASTC_12x10_UNORM_BLOCK : constant VkFormat := 181;
   VK_FORMAT_ASTC_12x10_SRGB_BLOCK : constant VkFormat := 182;
   VK_FORMAT_ASTC_12x12_UNORM_BLOCK : constant VkFormat := 183;
   VK_FORMAT_ASTC_12x12_SRGB_BLOCK : constant VkFormat := 184;
   VK_FORMAT_G8B8G8R8_422_UNORM : constant VkFormat := 1000156000;
   VK_FORMAT_B8G8R8G8_422_UNORM : constant VkFormat := 1000156001;
   VK_FORMAT_G8_B8_R8_3PLANE_420_UNORM : constant VkFormat := 1000156002;
   VK_FORMAT_G8_B8R8_2PLANE_420_UNORM : constant VkFormat := 1000156003;
   VK_FORMAT_G8_B8_R8_3PLANE_422_UNORM : constant VkFormat := 1000156004;
   VK_FORMAT_G8_B8R8_2PLANE_422_UNORM : constant VkFormat := 1000156005;
   VK_FORMAT_G8_B8_R8_3PLANE_444_UNORM : constant VkFormat := 1000156006;
   VK_FORMAT_R10X6_UNORM_PACK16 : constant VkFormat := 1000156007;
   VK_FORMAT_R10X6G10X6_UNORM_2PACK16 : constant VkFormat := 1000156008;
   VK_FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16 : constant VkFormat := 1000156009;
   VK_FORMAT_G10X6B10X6G10X6R10X6_422_UNORM_4PACK16 : constant VkFormat := 1000156010;
   VK_FORMAT_B10X6G10X6R10X6G10X6_422_UNORM_4PACK16 : constant VkFormat := 1000156011;
   VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_420_UNORM_3PACK16 : constant VkFormat := 1000156012;
   VK_FORMAT_G10X6_B10X6R10X6_2PLANE_420_UNORM_3PACK16 : constant VkFormat := 1000156013;
   VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_422_UNORM_3PACK16 : constant VkFormat := 1000156014;
   VK_FORMAT_G10X6_B10X6R10X6_2PLANE_422_UNORM_3PACK16 : constant VkFormat := 1000156015;
   VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_444_UNORM_3PACK16 : constant VkFormat := 1000156016;
   VK_FORMAT_R12X4_UNORM_PACK16 : constant VkFormat := 1000156017;
   VK_FORMAT_R12X4G12X4_UNORM_2PACK16 : constant VkFormat := 1000156018;
   VK_FORMAT_R12X4G12X4B12X4A12X4_UNORM_4PACK16 : constant VkFormat := 1000156019;
   VK_FORMAT_G12X4B12X4G12X4R12X4_422_UNORM_4PACK16 : constant VkFormat := 1000156020;
   VK_FORMAT_B12X4G12X4R12X4G12X4_422_UNORM_4PACK16 : constant VkFormat := 1000156021;
   VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_420_UNORM_3PACK16 : constant VkFormat := 1000156022;
   VK_FORMAT_G12X4_B12X4R12X4_2PLANE_420_UNORM_3PACK16 : constant VkFormat := 1000156023;
   VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_422_UNORM_3PACK16 : constant VkFormat := 1000156024;
   VK_FORMAT_G12X4_B12X4R12X4_2PLANE_422_UNORM_3PACK16 : constant VkFormat := 1000156025;
   VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_444_UNORM_3PACK16 : constant VkFormat := 1000156026;
   VK_FORMAT_G16B16G16R16_422_UNORM : constant VkFormat := 1000156027;
   VK_FORMAT_B16G16R16G16_422_UNORM : constant VkFormat := 1000156028;
   VK_FORMAT_G16_B16_R16_3PLANE_420_UNORM : constant VkFormat := 1000156029;
   VK_FORMAT_G16_B16R16_2PLANE_420_UNORM : constant VkFormat := 1000156030;
   VK_FORMAT_G16_B16_R16_3PLANE_422_UNORM : constant VkFormat := 1000156031;
   VK_FORMAT_G16_B16R16_2PLANE_422_UNORM : constant VkFormat := 1000156032;
   VK_FORMAT_G16_B16_R16_3PLANE_444_UNORM : constant VkFormat := 1000156033;
   VK_FORMAT_PVRTC1_2BPP_UNORM_BLOCK_IMG : constant VkFormat := 1000054000;
   VK_FORMAT_PVRTC1_4BPP_UNORM_BLOCK_IMG : constant VkFormat := 1000054001;
   VK_FORMAT_PVRTC2_2BPP_UNORM_BLOCK_IMG : constant VkFormat := 1000054002;
   VK_FORMAT_PVRTC2_4BPP_UNORM_BLOCK_IMG : constant VkFormat := 1000054003;
   VK_FORMAT_PVRTC1_2BPP_SRGB_BLOCK_IMG : constant VkFormat := 1000054004;
   VK_FORMAT_PVRTC1_4BPP_SRGB_BLOCK_IMG : constant VkFormat := 1000054005;
   VK_FORMAT_PVRTC2_2BPP_SRGB_BLOCK_IMG : constant VkFormat := 1000054006;
   VK_FORMAT_PVRTC2_4BPP_SRGB_BLOCK_IMG : constant VkFormat := 1000054007;
   VK_FORMAT_ASTC_4x4_SFLOAT_BLOCK_EXT : constant VkFormat := 1000066000;
   VK_FORMAT_ASTC_5x4_SFLOAT_BLOCK_EXT : constant VkFormat := 1000066001;
   VK_FORMAT_ASTC_5x5_SFLOAT_BLOCK_EXT : constant VkFormat := 1000066002;
   VK_FORMAT_ASTC_6x5_SFLOAT_BLOCK_EXT : constant VkFormat := 1000066003;
   VK_FORMAT_ASTC_6x6_SFLOAT_BLOCK_EXT : constant VkFormat := 1000066004;
   VK_FORMAT_ASTC_8x5_SFLOAT_BLOCK_EXT : constant VkFormat := 1000066005;
   VK_FORMAT_ASTC_8x6_SFLOAT_BLOCK_EXT : constant VkFormat := 1000066006;
   VK_FORMAT_ASTC_8x8_SFLOAT_BLOCK_EXT : constant VkFormat := 1000066007;
   VK_FORMAT_ASTC_10x5_SFLOAT_BLOCK_EXT : constant VkFormat := 1000066008;
   VK_FORMAT_ASTC_10x6_SFLOAT_BLOCK_EXT : constant VkFormat := 1000066009;
   VK_FORMAT_ASTC_10x8_SFLOAT_BLOCK_EXT : constant VkFormat := 1000066010;
   VK_FORMAT_ASTC_10x10_SFLOAT_BLOCK_EXT : constant VkFormat := 1000066011;
   VK_FORMAT_ASTC_12x10_SFLOAT_BLOCK_EXT : constant VkFormat := 1000066012;
   VK_FORMAT_ASTC_12x12_SFLOAT_BLOCK_EXT : constant VkFormat := 1000066013;
   VK_FORMAT_A4R4G4B4_UNORM_PACK16_EXT : constant VkFormat := 1000340000;
   VK_FORMAT_A4B4G4R4_UNORM_PACK16_EXT : constant VkFormat := 1000340001;
   VK_FORMAT_G8B8G8R8_422_UNORM_KHR : constant VkFormat := 1000156000;
   VK_FORMAT_B8G8R8G8_422_UNORM_KHR : constant VkFormat := 1000156001;
   VK_FORMAT_G8_B8_R8_3PLANE_420_UNORM_KHR : constant VkFormat := 1000156002;
   VK_FORMAT_G8_B8R8_2PLANE_420_UNORM_KHR : constant VkFormat := 1000156003;
   VK_FORMAT_G8_B8_R8_3PLANE_422_UNORM_KHR : constant VkFormat := 1000156004;
   VK_FORMAT_G8_B8R8_2PLANE_422_UNORM_KHR : constant VkFormat := 1000156005;
   VK_FORMAT_G8_B8_R8_3PLANE_444_UNORM_KHR : constant VkFormat := 1000156006;
   VK_FORMAT_R10X6_UNORM_PACK16_KHR : constant VkFormat := 1000156007;
   VK_FORMAT_R10X6G10X6_UNORM_2PACK16_KHR : constant VkFormat := 1000156008;
   VK_FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16_KHR : constant VkFormat := 1000156009;
   VK_FORMAT_G10X6B10X6G10X6R10X6_422_UNORM_4PACK16_KHR : constant VkFormat := 1000156010;
   VK_FORMAT_B10X6G10X6R10X6G10X6_422_UNORM_4PACK16_KHR : constant VkFormat := 1000156011;
   VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_420_UNORM_3PACK16_KHR : constant VkFormat := 1000156012;
   VK_FORMAT_G10X6_B10X6R10X6_2PLANE_420_UNORM_3PACK16_KHR : constant VkFormat := 1000156013;
   VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_422_UNORM_3PACK16_KHR : constant VkFormat := 1000156014;
   VK_FORMAT_G10X6_B10X6R10X6_2PLANE_422_UNORM_3PACK16_KHR : constant VkFormat := 1000156015;
   VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_444_UNORM_3PACK16_KHR : constant VkFormat := 1000156016;
   VK_FORMAT_R12X4_UNORM_PACK16_KHR : constant VkFormat := 1000156017;
   VK_FORMAT_R12X4G12X4_UNORM_2PACK16_KHR : constant VkFormat := 1000156018;
   VK_FORMAT_R12X4G12X4B12X4A12X4_UNORM_4PACK16_KHR : constant VkFormat := 1000156019;
   VK_FORMAT_G12X4B12X4G12X4R12X4_422_UNORM_4PACK16_KHR : constant VkFormat := 1000156020;
   VK_FORMAT_B12X4G12X4R12X4G12X4_422_UNORM_4PACK16_KHR : constant VkFormat := 1000156021;
   VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_420_UNORM_3PACK16_KHR : constant VkFormat := 1000156022;
   VK_FORMAT_G12X4_B12X4R12X4_2PLANE_420_UNORM_3PACK16_KHR : constant VkFormat := 1000156023;
   VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_422_UNORM_3PACK16_KHR : constant VkFormat := 1000156024;
   VK_FORMAT_G12X4_B12X4R12X4_2PLANE_422_UNORM_3PACK16_KHR : constant VkFormat := 1000156025;
   VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_444_UNORM_3PACK16_KHR : constant VkFormat := 1000156026;
   VK_FORMAT_G16B16G16R16_422_UNORM_KHR : constant VkFormat := 1000156027;
   VK_FORMAT_B16G16R16G16_422_UNORM_KHR : constant VkFormat := 1000156028;
   VK_FORMAT_G16_B16_R16_3PLANE_420_UNORM_KHR : constant VkFormat := 1000156029;
   VK_FORMAT_G16_B16R16_2PLANE_420_UNORM_KHR : constant VkFormat := 1000156030;
   VK_FORMAT_G16_B16_R16_3PLANE_422_UNORM_KHR : constant VkFormat := 1000156031;
   VK_FORMAT_G16_B16R16_2PLANE_422_UNORM_KHR : constant VkFormat := 1000156032;
   VK_FORMAT_G16_B16_R16_3PLANE_444_UNORM_KHR : constant VkFormat := 1000156033;
   VK_FORMAT_MAX_ENUM : constant VkFormat := 2147483647;  -- vulkan_core.h:862

   subtype VkImageTiling is unsigned;
   VK_IMAGE_TILING_OPTIMAL : constant VkImageTiling := 0;
   VK_IMAGE_TILING_LINEAR : constant VkImageTiling := 1;
   VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT : constant VkImageTiling := 1000158000;
   VK_IMAGE_TILING_MAX_ENUM : constant VkImageTiling := 2147483647;  -- vulkan_core.h:1143

   subtype VkImageType is unsigned;
   VK_IMAGE_TYPE_1D : constant VkImageType := 0;
   VK_IMAGE_TYPE_2D : constant VkImageType := 1;
   VK_IMAGE_TYPE_3D : constant VkImageType := 2;
   VK_IMAGE_TYPE_MAX_ENUM : constant VkImageType := 2147483647;  -- vulkan_core.h:1150

   subtype VkPhysicalDeviceType is unsigned;
   VK_PHYSICAL_DEVICE_TYPE_OTHER : constant VkPhysicalDeviceType := 0;
   VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU : constant VkPhysicalDeviceType := 1;
   VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU : constant VkPhysicalDeviceType := 2;
   VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU : constant VkPhysicalDeviceType := 3;
   VK_PHYSICAL_DEVICE_TYPE_CPU : constant VkPhysicalDeviceType := 4;
   VK_PHYSICAL_DEVICE_TYPE_MAX_ENUM : constant VkPhysicalDeviceType := 2147483647;  -- vulkan_core.h:1157

   subtype VkQueryType is unsigned;
   VK_QUERY_TYPE_OCCLUSION : constant VkQueryType := 0;
   VK_QUERY_TYPE_PIPELINE_STATISTICS : constant VkQueryType := 1;
   VK_QUERY_TYPE_TIMESTAMP : constant VkQueryType := 2;
   VK_QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT : constant VkQueryType := 1000028004;
   VK_QUERY_TYPE_PERFORMANCE_QUERY_KHR : constant VkQueryType := 1000116000;
   VK_QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_KHR : constant VkQueryType := 1000150000;
   VK_QUERY_TYPE_ACCELERATION_STRUCTURE_SERIALIZATION_SIZE_KHR : constant VkQueryType := 1000150001;
   VK_QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_NV : constant VkQueryType := 1000165000;
   VK_QUERY_TYPE_PERFORMANCE_QUERY_INTEL : constant VkQueryType := 1000210000;
   VK_QUERY_TYPE_MAX_ENUM : constant VkQueryType := 2147483647;  -- vulkan_core.h:1166

   subtype VkSharingMode is unsigned;
   VK_SHARING_MODE_EXCLUSIVE : constant VkSharingMode := 0;
   VK_SHARING_MODE_CONCURRENT : constant VkSharingMode := 1;
   VK_SHARING_MODE_MAX_ENUM : constant VkSharingMode := 2147483647;  -- vulkan_core.h:1179

   subtype VkComponentSwizzle is unsigned;
   VK_COMPONENT_SWIZZLE_IDENTITY : constant VkComponentSwizzle := 0;
   VK_COMPONENT_SWIZZLE_ZERO : constant VkComponentSwizzle := 1;
   VK_COMPONENT_SWIZZLE_ONE : constant VkComponentSwizzle := 2;
   VK_COMPONENT_SWIZZLE_R : constant VkComponentSwizzle := 3;
   VK_COMPONENT_SWIZZLE_G : constant VkComponentSwizzle := 4;
   VK_COMPONENT_SWIZZLE_B : constant VkComponentSwizzle := 5;
   VK_COMPONENT_SWIZZLE_A : constant VkComponentSwizzle := 6;
   VK_COMPONENT_SWIZZLE_MAX_ENUM : constant VkComponentSwizzle := 2147483647;  -- vulkan_core.h:1185

   subtype VkImageViewType is unsigned;
   VK_IMAGE_VIEW_TYPE_1D : constant VkImageViewType := 0;
   VK_IMAGE_VIEW_TYPE_2D : constant VkImageViewType := 1;
   VK_IMAGE_VIEW_TYPE_3D : constant VkImageViewType := 2;
   VK_IMAGE_VIEW_TYPE_CUBE : constant VkImageViewType := 3;
   VK_IMAGE_VIEW_TYPE_1D_ARRAY : constant VkImageViewType := 4;
   VK_IMAGE_VIEW_TYPE_2D_ARRAY : constant VkImageViewType := 5;
   VK_IMAGE_VIEW_TYPE_CUBE_ARRAY : constant VkImageViewType := 6;
   VK_IMAGE_VIEW_TYPE_MAX_ENUM : constant VkImageViewType := 2147483647;  -- vulkan_core.h:1196

   subtype VkBlendFactor is unsigned;
   VK_BLEND_FACTOR_ZERO : constant VkBlendFactor := 0;
   VK_BLEND_FACTOR_ONE : constant VkBlendFactor := 1;
   VK_BLEND_FACTOR_SRC_COLOR : constant VkBlendFactor := 2;
   VK_BLEND_FACTOR_ONE_MINUS_SRC_COLOR : constant VkBlendFactor := 3;
   VK_BLEND_FACTOR_DST_COLOR : constant VkBlendFactor := 4;
   VK_BLEND_FACTOR_ONE_MINUS_DST_COLOR : constant VkBlendFactor := 5;
   VK_BLEND_FACTOR_SRC_ALPHA : constant VkBlendFactor := 6;
   VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA : constant VkBlendFactor := 7;
   VK_BLEND_FACTOR_DST_ALPHA : constant VkBlendFactor := 8;
   VK_BLEND_FACTOR_ONE_MINUS_DST_ALPHA : constant VkBlendFactor := 9;
   VK_BLEND_FACTOR_CONSTANT_COLOR : constant VkBlendFactor := 10;
   VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR : constant VkBlendFactor := 11;
   VK_BLEND_FACTOR_CONSTANT_ALPHA : constant VkBlendFactor := 12;
   VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA : constant VkBlendFactor := 13;
   VK_BLEND_FACTOR_SRC_ALPHA_SATURATE : constant VkBlendFactor := 14;
   VK_BLEND_FACTOR_SRC1_COLOR : constant VkBlendFactor := 15;
   VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR : constant VkBlendFactor := 16;
   VK_BLEND_FACTOR_SRC1_ALPHA : constant VkBlendFactor := 17;
   VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA : constant VkBlendFactor := 18;
   VK_BLEND_FACTOR_MAX_ENUM : constant VkBlendFactor := 2147483647;  -- vulkan_core.h:1207

   subtype VkBlendOp is unsigned;
   VK_BLEND_OP_ADD : constant VkBlendOp := 0;
   VK_BLEND_OP_SUBTRACT : constant VkBlendOp := 1;
   VK_BLEND_OP_REVERSE_SUBTRACT : constant VkBlendOp := 2;
   VK_BLEND_OP_MIN : constant VkBlendOp := 3;
   VK_BLEND_OP_MAX : constant VkBlendOp := 4;
   VK_BLEND_OP_ZERO_EXT : constant VkBlendOp := 1000148000;
   VK_BLEND_OP_SRC_EXT : constant VkBlendOp := 1000148001;
   VK_BLEND_OP_DST_EXT : constant VkBlendOp := 1000148002;
   VK_BLEND_OP_SRC_OVER_EXT : constant VkBlendOp := 1000148003;
   VK_BLEND_OP_DST_OVER_EXT : constant VkBlendOp := 1000148004;
   VK_BLEND_OP_SRC_IN_EXT : constant VkBlendOp := 1000148005;
   VK_BLEND_OP_DST_IN_EXT : constant VkBlendOp := 1000148006;
   VK_BLEND_OP_SRC_OUT_EXT : constant VkBlendOp := 1000148007;
   VK_BLEND_OP_DST_OUT_EXT : constant VkBlendOp := 1000148008;
   VK_BLEND_OP_SRC_ATOP_EXT : constant VkBlendOp := 1000148009;
   VK_BLEND_OP_DST_ATOP_EXT : constant VkBlendOp := 1000148010;
   VK_BLEND_OP_XOR_EXT : constant VkBlendOp := 1000148011;
   VK_BLEND_OP_MULTIPLY_EXT : constant VkBlendOp := 1000148012;
   VK_BLEND_OP_SCREEN_EXT : constant VkBlendOp := 1000148013;
   VK_BLEND_OP_OVERLAY_EXT : constant VkBlendOp := 1000148014;
   VK_BLEND_OP_DARKEN_EXT : constant VkBlendOp := 1000148015;
   VK_BLEND_OP_LIGHTEN_EXT : constant VkBlendOp := 1000148016;
   VK_BLEND_OP_COLORDODGE_EXT : constant VkBlendOp := 1000148017;
   VK_BLEND_OP_COLORBURN_EXT : constant VkBlendOp := 1000148018;
   VK_BLEND_OP_HARDLIGHT_EXT : constant VkBlendOp := 1000148019;
   VK_BLEND_OP_SOFTLIGHT_EXT : constant VkBlendOp := 1000148020;
   VK_BLEND_OP_DIFFERENCE_EXT : constant VkBlendOp := 1000148021;
   VK_BLEND_OP_EXCLUSION_EXT : constant VkBlendOp := 1000148022;
   VK_BLEND_OP_INVERT_EXT : constant VkBlendOp := 1000148023;
   VK_BLEND_OP_INVERT_RGB_EXT : constant VkBlendOp := 1000148024;
   VK_BLEND_OP_LINEARDODGE_EXT : constant VkBlendOp := 1000148025;
   VK_BLEND_OP_LINEARBURN_EXT : constant VkBlendOp := 1000148026;
   VK_BLEND_OP_VIVIDLIGHT_EXT : constant VkBlendOp := 1000148027;
   VK_BLEND_OP_LINEARLIGHT_EXT : constant VkBlendOp := 1000148028;
   VK_BLEND_OP_PINLIGHT_EXT : constant VkBlendOp := 1000148029;
   VK_BLEND_OP_HARDMIX_EXT : constant VkBlendOp := 1000148030;
   VK_BLEND_OP_HSL_HUE_EXT : constant VkBlendOp := 1000148031;
   VK_BLEND_OP_HSL_SATURATION_EXT : constant VkBlendOp := 1000148032;
   VK_BLEND_OP_HSL_COLOR_EXT : constant VkBlendOp := 1000148033;
   VK_BLEND_OP_HSL_LUMINOSITY_EXT : constant VkBlendOp := 1000148034;
   VK_BLEND_OP_PLUS_EXT : constant VkBlendOp := 1000148035;
   VK_BLEND_OP_PLUS_CLAMPED_EXT : constant VkBlendOp := 1000148036;
   VK_BLEND_OP_PLUS_CLAMPED_ALPHA_EXT : constant VkBlendOp := 1000148037;
   VK_BLEND_OP_PLUS_DARKER_EXT : constant VkBlendOp := 1000148038;
   VK_BLEND_OP_MINUS_EXT : constant VkBlendOp := 1000148039;
   VK_BLEND_OP_MINUS_CLAMPED_EXT : constant VkBlendOp := 1000148040;
   VK_BLEND_OP_CONTRAST_EXT : constant VkBlendOp := 1000148041;
   VK_BLEND_OP_INVERT_OVG_EXT : constant VkBlendOp := 1000148042;
   VK_BLEND_OP_RED_EXT : constant VkBlendOp := 1000148043;
   VK_BLEND_OP_GREEN_EXT : constant VkBlendOp := 1000148044;
   VK_BLEND_OP_BLUE_EXT : constant VkBlendOp := 1000148045;
   VK_BLEND_OP_MAX_ENUM : constant VkBlendOp := 2147483647;  -- vulkan_core.h:1230

   subtype VkCompareOp is unsigned;
   VK_COMPARE_OP_NEVER : constant VkCompareOp := 0;
   VK_COMPARE_OP_LESS : constant VkCompareOp := 1;
   VK_COMPARE_OP_EQUAL : constant VkCompareOp := 2;
   VK_COMPARE_OP_LESS_OR_EQUAL : constant VkCompareOp := 3;
   VK_COMPARE_OP_GREATER : constant VkCompareOp := 4;
   VK_COMPARE_OP_NOT_EQUAL : constant VkCompareOp := 5;
   VK_COMPARE_OP_GREATER_OR_EQUAL : constant VkCompareOp := 6;
   VK_COMPARE_OP_ALWAYS : constant VkCompareOp := 7;
   VK_COMPARE_OP_MAX_ENUM : constant VkCompareOp := 2147483647;  -- vulkan_core.h:1285

   subtype VkDynamicState is unsigned;
   VK_DYNAMIC_STATE_VIEWPORT : constant VkDynamicState := 0;
   VK_DYNAMIC_STATE_SCISSOR : constant VkDynamicState := 1;
   VK_DYNAMIC_STATE_LINE_WIDTH : constant VkDynamicState := 2;
   VK_DYNAMIC_STATE_DEPTH_BIAS : constant VkDynamicState := 3;
   VK_DYNAMIC_STATE_BLEND_CONSTANTS : constant VkDynamicState := 4;
   VK_DYNAMIC_STATE_DEPTH_BOUNDS : constant VkDynamicState := 5;
   VK_DYNAMIC_STATE_STENCIL_COMPARE_MASK : constant VkDynamicState := 6;
   VK_DYNAMIC_STATE_STENCIL_WRITE_MASK : constant VkDynamicState := 7;
   VK_DYNAMIC_STATE_STENCIL_REFERENCE : constant VkDynamicState := 8;
   VK_DYNAMIC_STATE_VIEWPORT_W_SCALING_NV : constant VkDynamicState := 1000087000;
   VK_DYNAMIC_STATE_DISCARD_RECTANGLE_EXT : constant VkDynamicState := 1000099000;
   VK_DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT : constant VkDynamicState := 1000143000;
   VK_DYNAMIC_STATE_RAY_TRACING_PIPELINE_STACK_SIZE_KHR : constant VkDynamicState := 1000347000;
   VK_DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV : constant VkDynamicState := 1000164004;
   VK_DYNAMIC_STATE_VIEWPORT_COARSE_SAMPLE_ORDER_NV : constant VkDynamicState := 1000164006;
   VK_DYNAMIC_STATE_EXCLUSIVE_SCISSOR_NV : constant VkDynamicState := 1000205001;
   VK_DYNAMIC_STATE_FRAGMENT_SHADING_RATE_KHR : constant VkDynamicState := 1000226000;
   VK_DYNAMIC_STATE_LINE_STIPPLE_EXT : constant VkDynamicState := 1000259000;
   VK_DYNAMIC_STATE_CULL_MODE_EXT : constant VkDynamicState := 1000267000;
   VK_DYNAMIC_STATE_FRONT_FACE_EXT : constant VkDynamicState := 1000267001;
   VK_DYNAMIC_STATE_PRIMITIVE_TOPOLOGY_EXT : constant VkDynamicState := 1000267002;
   VK_DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT : constant VkDynamicState := 1000267003;
   VK_DYNAMIC_STATE_SCISSOR_WITH_COUNT_EXT : constant VkDynamicState := 1000267004;
   VK_DYNAMIC_STATE_VERTEX_INPUT_BINDING_STRIDE_EXT : constant VkDynamicState := 1000267005;
   VK_DYNAMIC_STATE_DEPTH_TEST_ENABLE_EXT : constant VkDynamicState := 1000267006;
   VK_DYNAMIC_STATE_DEPTH_WRITE_ENABLE_EXT : constant VkDynamicState := 1000267007;
   VK_DYNAMIC_STATE_DEPTH_COMPARE_OP_EXT : constant VkDynamicState := 1000267008;
   VK_DYNAMIC_STATE_DEPTH_BOUNDS_TEST_ENABLE_EXT : constant VkDynamicState := 1000267009;
   VK_DYNAMIC_STATE_STENCIL_TEST_ENABLE_EXT : constant VkDynamicState := 1000267010;
   VK_DYNAMIC_STATE_STENCIL_OP_EXT : constant VkDynamicState := 1000267011;
   VK_DYNAMIC_STATE_MAX_ENUM : constant VkDynamicState := 2147483647;  -- vulkan_core.h:1297

   subtype VkFrontFace is unsigned;
   VK_FRONT_FACE_COUNTER_CLOCKWISE : constant VkFrontFace := 0;
   VK_FRONT_FACE_CLOCKWISE : constant VkFrontFace := 1;
   VK_FRONT_FACE_MAX_ENUM : constant VkFrontFace := 2147483647;  -- vulkan_core.h:1331

   subtype VkVertexInputRate is unsigned;
   VK_VERTEX_INPUT_RATE_VERTEX : constant VkVertexInputRate := 0;
   VK_VERTEX_INPUT_RATE_INSTANCE : constant VkVertexInputRate := 1;
   VK_VERTEX_INPUT_RATE_MAX_ENUM : constant VkVertexInputRate := 2147483647;  -- vulkan_core.h:1337

   subtype VkPrimitiveTopology is unsigned;
   VK_PRIMITIVE_TOPOLOGY_POINT_LIST : constant VkPrimitiveTopology := 0;
   VK_PRIMITIVE_TOPOLOGY_LINE_LIST : constant VkPrimitiveTopology := 1;
   VK_PRIMITIVE_TOPOLOGY_LINE_STRIP : constant VkPrimitiveTopology := 2;
   VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST : constant VkPrimitiveTopology := 3;
   VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP : constant VkPrimitiveTopology := 4;
   VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN : constant VkPrimitiveTopology := 5;
   VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY : constant VkPrimitiveTopology := 6;
   VK_PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY : constant VkPrimitiveTopology := 7;
   VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY : constant VkPrimitiveTopology := 8;
   VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY : constant VkPrimitiveTopology := 9;
   VK_PRIMITIVE_TOPOLOGY_PATCH_LIST : constant VkPrimitiveTopology := 10;
   VK_PRIMITIVE_TOPOLOGY_MAX_ENUM : constant VkPrimitiveTopology := 2147483647;  -- vulkan_core.h:1343

   subtype VkPolygonMode is unsigned;
   VK_POLYGON_MODE_FILL : constant VkPolygonMode := 0;
   VK_POLYGON_MODE_LINE : constant VkPolygonMode := 1;
   VK_POLYGON_MODE_POINT : constant VkPolygonMode := 2;
   VK_POLYGON_MODE_FILL_RECTANGLE_NV : constant VkPolygonMode := 1000153000;
   VK_POLYGON_MODE_MAX_ENUM : constant VkPolygonMode := 2147483647;  -- vulkan_core.h:1358

   subtype VkStencilOp is unsigned;
   VK_STENCIL_OP_KEEP : constant VkStencilOp := 0;
   VK_STENCIL_OP_ZERO : constant VkStencilOp := 1;
   VK_STENCIL_OP_REPLACE : constant VkStencilOp := 2;
   VK_STENCIL_OP_INCREMENT_AND_CLAMP : constant VkStencilOp := 3;
   VK_STENCIL_OP_DECREMENT_AND_CLAMP : constant VkStencilOp := 4;
   VK_STENCIL_OP_INVERT : constant VkStencilOp := 5;
   VK_STENCIL_OP_INCREMENT_AND_WRAP : constant VkStencilOp := 6;
   VK_STENCIL_OP_DECREMENT_AND_WRAP : constant VkStencilOp := 7;
   VK_STENCIL_OP_MAX_ENUM : constant VkStencilOp := 2147483647;  -- vulkan_core.h:1366

   subtype VkLogicOp is unsigned;
   VK_LOGIC_OP_CLEAR : constant VkLogicOp := 0;
   VK_LOGIC_OP_AND : constant VkLogicOp := 1;
   VK_LOGIC_OP_AND_REVERSE : constant VkLogicOp := 2;
   VK_LOGIC_OP_COPY : constant VkLogicOp := 3;
   VK_LOGIC_OP_AND_INVERTED : constant VkLogicOp := 4;
   VK_LOGIC_OP_NO_OP : constant VkLogicOp := 5;
   VK_LOGIC_OP_XOR : constant VkLogicOp := 6;
   VK_LOGIC_OP_OR : constant VkLogicOp := 7;
   VK_LOGIC_OP_NOR : constant VkLogicOp := 8;
   VK_LOGIC_OP_EQUIVALENT : constant VkLogicOp := 9;
   VK_LOGIC_OP_INVERT : constant VkLogicOp := 10;
   VK_LOGIC_OP_OR_REVERSE : constant VkLogicOp := 11;
   VK_LOGIC_OP_COPY_INVERTED : constant VkLogicOp := 12;
   VK_LOGIC_OP_OR_INVERTED : constant VkLogicOp := 13;
   VK_LOGIC_OP_NAND : constant VkLogicOp := 14;
   VK_LOGIC_OP_SET : constant VkLogicOp := 15;
   VK_LOGIC_OP_MAX_ENUM : constant VkLogicOp := 2147483647;  -- vulkan_core.h:1378

   subtype VkBorderColor is unsigned;
   VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK : constant VkBorderColor := 0;
   VK_BORDER_COLOR_INT_TRANSPARENT_BLACK : constant VkBorderColor := 1;
   VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK : constant VkBorderColor := 2;
   VK_BORDER_COLOR_INT_OPAQUE_BLACK : constant VkBorderColor := 3;
   VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE : constant VkBorderColor := 4;
   VK_BORDER_COLOR_INT_OPAQUE_WHITE : constant VkBorderColor := 5;
   VK_BORDER_COLOR_FLOAT_CUSTOM_EXT : constant VkBorderColor := 1000287003;
   VK_BORDER_COLOR_INT_CUSTOM_EXT : constant VkBorderColor := 1000287004;
   VK_BORDER_COLOR_MAX_ENUM : constant VkBorderColor := 2147483647;  -- vulkan_core.h:1398

   subtype VkFilter is unsigned;
   VK_FILTER_NEAREST : constant VkFilter := 0;
   VK_FILTER_LINEAR : constant VkFilter := 1;
   VK_FILTER_CUBIC_IMG : constant VkFilter := 1000015000;
   VK_FILTER_CUBIC_EXT : constant VkFilter := 1000015000;
   VK_FILTER_MAX_ENUM : constant VkFilter := 2147483647;  -- vulkan_core.h:1410

   subtype VkSamplerAddressMode is unsigned;
   VK_SAMPLER_ADDRESS_MODE_REPEAT : constant VkSamplerAddressMode := 0;
   VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT : constant VkSamplerAddressMode := 1;
   VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE : constant VkSamplerAddressMode := 2;
   VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER : constant VkSamplerAddressMode := 3;
   VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE : constant VkSamplerAddressMode := 4;
   VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE_KHR : constant VkSamplerAddressMode := 4;
   VK_SAMPLER_ADDRESS_MODE_MAX_ENUM : constant VkSamplerAddressMode := 2147483647;  -- vulkan_core.h:1418

   subtype VkSamplerMipmapMode is unsigned;
   VK_SAMPLER_MIPMAP_MODE_NEAREST : constant VkSamplerMipmapMode := 0;
   VK_SAMPLER_MIPMAP_MODE_LINEAR : constant VkSamplerMipmapMode := 1;
   VK_SAMPLER_MIPMAP_MODE_MAX_ENUM : constant VkSamplerMipmapMode := 2147483647;  -- vulkan_core.h:1428

   subtype VkDescriptorType is unsigned;
   VK_DESCRIPTOR_TYPE_SAMPLER : constant VkDescriptorType := 0;
   VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER : constant VkDescriptorType := 1;
   VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE : constant VkDescriptorType := 2;
   VK_DESCRIPTOR_TYPE_STORAGE_IMAGE : constant VkDescriptorType := 3;
   VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER : constant VkDescriptorType := 4;
   VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER : constant VkDescriptorType := 5;
   VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER : constant VkDescriptorType := 6;
   VK_DESCRIPTOR_TYPE_STORAGE_BUFFER : constant VkDescriptorType := 7;
   VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC : constant VkDescriptorType := 8;
   VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC : constant VkDescriptorType := 9;
   VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT : constant VkDescriptorType := 10;
   VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT : constant VkDescriptorType := 1000138000;
   VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR : constant VkDescriptorType := 1000150000;
   VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV : constant VkDescriptorType := 1000165000;
   VK_DESCRIPTOR_TYPE_MUTABLE_VALVE : constant VkDescriptorType := 1000351000;
   VK_DESCRIPTOR_TYPE_MAX_ENUM : constant VkDescriptorType := 2147483647;  -- vulkan_core.h:1434

   subtype VkAttachmentLoadOp is unsigned;
   VK_ATTACHMENT_LOAD_OP_LOAD : constant VkAttachmentLoadOp := 0;
   VK_ATTACHMENT_LOAD_OP_CLEAR : constant VkAttachmentLoadOp := 1;
   VK_ATTACHMENT_LOAD_OP_DONT_CARE : constant VkAttachmentLoadOp := 2;
   VK_ATTACHMENT_LOAD_OP_MAX_ENUM : constant VkAttachmentLoadOp := 2147483647;  -- vulkan_core.h:1453

   subtype VkAttachmentStoreOp is unsigned;
   VK_ATTACHMENT_STORE_OP_STORE : constant VkAttachmentStoreOp := 0;
   VK_ATTACHMENT_STORE_OP_DONT_CARE : constant VkAttachmentStoreOp := 1;
   VK_ATTACHMENT_STORE_OP_NONE_QCOM : constant VkAttachmentStoreOp := 1000301000;
   VK_ATTACHMENT_STORE_OP_MAX_ENUM : constant VkAttachmentStoreOp := 2147483647;  -- vulkan_core.h:1460

   subtype VkPipelineBindPoint is unsigned;
   VK_PIPELINE_BIND_POINT_GRAPHICS : constant VkPipelineBindPoint := 0;
   VK_PIPELINE_BIND_POINT_COMPUTE : constant VkPipelineBindPoint := 1;
   VK_PIPELINE_BIND_POINT_RAY_TRACING_KHR : constant VkPipelineBindPoint := 1000165000;
   VK_PIPELINE_BIND_POINT_RAY_TRACING_NV : constant VkPipelineBindPoint := 1000165000;
   VK_PIPELINE_BIND_POINT_MAX_ENUM : constant VkPipelineBindPoint := 2147483647;  -- vulkan_core.h:1467

   subtype VkCommandBufferLevel is unsigned;
   VK_COMMAND_BUFFER_LEVEL_PRIMARY : constant VkCommandBufferLevel := 0;
   VK_COMMAND_BUFFER_LEVEL_SECONDARY : constant VkCommandBufferLevel := 1;
   VK_COMMAND_BUFFER_LEVEL_MAX_ENUM : constant VkCommandBufferLevel := 2147483647;  -- vulkan_core.h:1475

   subtype VkIndexType is unsigned;
   VK_INDEX_TYPE_UINT16 : constant VkIndexType := 0;
   VK_INDEX_TYPE_UINT32 : constant VkIndexType := 1;
   VK_INDEX_TYPE_NONE_KHR : constant VkIndexType := 1000165000;
   VK_INDEX_TYPE_UINT8_EXT : constant VkIndexType := 1000265000;
   VK_INDEX_TYPE_NONE_NV : constant VkIndexType := 1000165000;
   VK_INDEX_TYPE_MAX_ENUM : constant VkIndexType := 2147483647;  -- vulkan_core.h:1481

   subtype VkSubpassContents is unsigned;
   VK_SUBPASS_CONTENTS_INLINE : constant VkSubpassContents := 0;
   VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS : constant VkSubpassContents := 1;
   VK_SUBPASS_CONTENTS_MAX_ENUM : constant VkSubpassContents := 2147483647;  -- vulkan_core.h:1490

   subtype VkAccessFlagBits is unsigned;
   VK_ACCESS_INDIRECT_COMMAND_READ_BIT : constant VkAccessFlagBits := 1;
   VK_ACCESS_INDEX_READ_BIT : constant VkAccessFlagBits := 2;
   VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT : constant VkAccessFlagBits := 4;
   VK_ACCESS_UNIFORM_READ_BIT : constant VkAccessFlagBits := 8;
   VK_ACCESS_INPUT_ATTACHMENT_READ_BIT : constant VkAccessFlagBits := 16;
   VK_ACCESS_SHADER_READ_BIT : constant VkAccessFlagBits := 32;
   VK_ACCESS_SHADER_WRITE_BIT : constant VkAccessFlagBits := 64;
   VK_ACCESS_COLOR_ATTACHMENT_READ_BIT : constant VkAccessFlagBits := 128;
   VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT : constant VkAccessFlagBits := 256;
   VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT : constant VkAccessFlagBits := 512;
   VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT : constant VkAccessFlagBits := 1024;
   VK_ACCESS_TRANSFER_READ_BIT : constant VkAccessFlagBits := 2048;
   VK_ACCESS_TRANSFER_WRITE_BIT : constant VkAccessFlagBits := 4096;
   VK_ACCESS_HOST_READ_BIT : constant VkAccessFlagBits := 8192;
   VK_ACCESS_HOST_WRITE_BIT : constant VkAccessFlagBits := 16384;
   VK_ACCESS_MEMORY_READ_BIT : constant VkAccessFlagBits := 32768;
   VK_ACCESS_MEMORY_WRITE_BIT : constant VkAccessFlagBits := 65536;
   VK_ACCESS_TRANSFORM_FEEDBACK_WRITE_BIT_EXT : constant VkAccessFlagBits := 33554432;
   VK_ACCESS_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT : constant VkAccessFlagBits := 67108864;
   VK_ACCESS_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT : constant VkAccessFlagBits := 134217728;
   VK_ACCESS_CONDITIONAL_RENDERING_READ_BIT_EXT : constant VkAccessFlagBits := 1048576;
   VK_ACCESS_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT : constant VkAccessFlagBits := 524288;
   VK_ACCESS_ACCELERATION_STRUCTURE_READ_BIT_KHR : constant VkAccessFlagBits := 2097152;
   VK_ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_KHR : constant VkAccessFlagBits := 4194304;
   VK_ACCESS_SHADING_RATE_IMAGE_READ_BIT_NV : constant VkAccessFlagBits := 8388608;
   VK_ACCESS_FRAGMENT_DENSITY_MAP_READ_BIT_EXT : constant VkAccessFlagBits := 16777216;
   VK_ACCESS_COMMAND_PREPROCESS_READ_BIT_NV : constant VkAccessFlagBits := 131072;
   VK_ACCESS_COMMAND_PREPROCESS_WRITE_BIT_NV : constant VkAccessFlagBits := 262144;
   VK_ACCESS_NONE_KHR : constant VkAccessFlagBits := 0;
   VK_ACCESS_ACCELERATION_STRUCTURE_READ_BIT_NV : constant VkAccessFlagBits := 2097152;
   VK_ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_NV : constant VkAccessFlagBits := 4194304;
   VK_ACCESS_FRAGMENT_SHADING_RATE_ATTACHMENT_READ_BIT_KHR : constant VkAccessFlagBits := 8388608;
   VK_ACCESS_FLAG_BITS_MAX_ENUM : constant VkAccessFlagBits := 2147483647;  -- vulkan_core.h:1496

   subtype VkAccessFlags is VkFlags;  -- vulkan_core.h:1531

   subtype VkImageAspectFlagBits is unsigned;
   VK_IMAGE_ASPECT_COLOR_BIT : constant VkImageAspectFlagBits := 1;
   VK_IMAGE_ASPECT_DEPTH_BIT : constant VkImageAspectFlagBits := 2;
   VK_IMAGE_ASPECT_STENCIL_BIT : constant VkImageAspectFlagBits := 4;
   VK_IMAGE_ASPECT_METADATA_BIT : constant VkImageAspectFlagBits := 8;
   VK_IMAGE_ASPECT_PLANE_0_BIT : constant VkImageAspectFlagBits := 16;
   VK_IMAGE_ASPECT_PLANE_1_BIT : constant VkImageAspectFlagBits := 32;
   VK_IMAGE_ASPECT_PLANE_2_BIT : constant VkImageAspectFlagBits := 64;
   VK_IMAGE_ASPECT_MEMORY_PLANE_0_BIT_EXT : constant VkImageAspectFlagBits := 128;
   VK_IMAGE_ASPECT_MEMORY_PLANE_1_BIT_EXT : constant VkImageAspectFlagBits := 256;
   VK_IMAGE_ASPECT_MEMORY_PLANE_2_BIT_EXT : constant VkImageAspectFlagBits := 512;
   VK_IMAGE_ASPECT_MEMORY_PLANE_3_BIT_EXT : constant VkImageAspectFlagBits := 1024;
   VK_IMAGE_ASPECT_PLANE_0_BIT_KHR : constant VkImageAspectFlagBits := 16;
   VK_IMAGE_ASPECT_PLANE_1_BIT_KHR : constant VkImageAspectFlagBits := 32;
   VK_IMAGE_ASPECT_PLANE_2_BIT_KHR : constant VkImageAspectFlagBits := 64;
   VK_IMAGE_ASPECT_FLAG_BITS_MAX_ENUM : constant VkImageAspectFlagBits := 2147483647;  -- vulkan_core.h:1533

   subtype VkImageAspectFlags is VkFlags;  -- vulkan_core.h:1550

   subtype VkFormatFeatureFlagBits is unsigned;
   VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT : constant VkFormatFeatureFlagBits := 1;
   VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT : constant VkFormatFeatureFlagBits := 2;
   VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT : constant VkFormatFeatureFlagBits := 4;
   VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT : constant VkFormatFeatureFlagBits := 8;
   VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT : constant VkFormatFeatureFlagBits := 16;
   VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT : constant VkFormatFeatureFlagBits := 32;
   VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT : constant VkFormatFeatureFlagBits := 64;
   VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT : constant VkFormatFeatureFlagBits := 128;
   VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT : constant VkFormatFeatureFlagBits := 256;
   VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT : constant VkFormatFeatureFlagBits := 512;
   VK_FORMAT_FEATURE_BLIT_SRC_BIT : constant VkFormatFeatureFlagBits := 1024;
   VK_FORMAT_FEATURE_BLIT_DST_BIT : constant VkFormatFeatureFlagBits := 2048;
   VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT : constant VkFormatFeatureFlagBits := 4096;
   VK_FORMAT_FEATURE_TRANSFER_SRC_BIT : constant VkFormatFeatureFlagBits := 16384;
   VK_FORMAT_FEATURE_TRANSFER_DST_BIT : constant VkFormatFeatureFlagBits := 32768;
   VK_FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT : constant VkFormatFeatureFlagBits := 131072;
   VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT : constant VkFormatFeatureFlagBits := 262144;
   VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT : constant VkFormatFeatureFlagBits := 524288;
   VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT : constant VkFormatFeatureFlagBits := 1048576;
   VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT : constant VkFormatFeatureFlagBits := 2097152;
   VK_FORMAT_FEATURE_DISJOINT_BIT : constant VkFormatFeatureFlagBits := 4194304;
   VK_FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT : constant VkFormatFeatureFlagBits := 8388608;
   VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT : constant VkFormatFeatureFlagBits := 65536;
   VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG : constant VkFormatFeatureFlagBits := 8192;
   VK_FORMAT_FEATURE_ACCELERATION_STRUCTURE_VERTEX_BUFFER_BIT_KHR : constant VkFormatFeatureFlagBits := 536870912;
   VK_FORMAT_FEATURE_FRAGMENT_DENSITY_MAP_BIT_EXT : constant VkFormatFeatureFlagBits := 16777216;
   VK_FORMAT_FEATURE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR : constant VkFormatFeatureFlagBits := 1073741824;
   VK_FORMAT_FEATURE_TRANSFER_SRC_BIT_KHR : constant VkFormatFeatureFlagBits := 16384;
   VK_FORMAT_FEATURE_TRANSFER_DST_BIT_KHR : constant VkFormatFeatureFlagBits := 32768;
   VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT_EXT : constant VkFormatFeatureFlagBits := 65536;
   VK_FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT_KHR : constant VkFormatFeatureFlagBits := 131072;
   VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT_KHR : constant VkFormatFeatureFlagBits := 262144;
   VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT_KHR : constant VkFormatFeatureFlagBits := 524288;
   VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT_KHR : constant VkFormatFeatureFlagBits := 1048576;
   VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT_KHR : constant VkFormatFeatureFlagBits := 2097152;
   VK_FORMAT_FEATURE_DISJOINT_BIT_KHR : constant VkFormatFeatureFlagBits := 4194304;
   VK_FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT_KHR : constant VkFormatFeatureFlagBits := 8388608;
   VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT : constant VkFormatFeatureFlagBits := 8192;
   VK_FORMAT_FEATURE_FLAG_BITS_MAX_ENUM : constant VkFormatFeatureFlagBits := 2147483647;  -- vulkan_core.h:1552

   subtype VkFormatFeatureFlags is VkFlags;  -- vulkan_core.h:1593

   subtype VkImageCreateFlagBits is unsigned;
   VK_IMAGE_CREATE_SPARSE_BINDING_BIT : constant VkImageCreateFlagBits := 1;
   VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT : constant VkImageCreateFlagBits := 2;
   VK_IMAGE_CREATE_SPARSE_ALIASED_BIT : constant VkImageCreateFlagBits := 4;
   VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT : constant VkImageCreateFlagBits := 8;
   VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT : constant VkImageCreateFlagBits := 16;
   VK_IMAGE_CREATE_ALIAS_BIT : constant VkImageCreateFlagBits := 1024;
   VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT : constant VkImageCreateFlagBits := 64;
   VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT : constant VkImageCreateFlagBits := 32;
   VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT : constant VkImageCreateFlagBits := 128;
   VK_IMAGE_CREATE_EXTENDED_USAGE_BIT : constant VkImageCreateFlagBits := 256;
   VK_IMAGE_CREATE_PROTECTED_BIT : constant VkImageCreateFlagBits := 2048;
   VK_IMAGE_CREATE_DISJOINT_BIT : constant VkImageCreateFlagBits := 512;
   VK_IMAGE_CREATE_CORNER_SAMPLED_BIT_NV : constant VkImageCreateFlagBits := 8192;
   VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT : constant VkImageCreateFlagBits := 4096;
   VK_IMAGE_CREATE_SUBSAMPLED_BIT_EXT : constant VkImageCreateFlagBits := 16384;
   VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR : constant VkImageCreateFlagBits := 64;
   VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT_KHR : constant VkImageCreateFlagBits := 32;
   VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT_KHR : constant VkImageCreateFlagBits := 128;
   VK_IMAGE_CREATE_EXTENDED_USAGE_BIT_KHR : constant VkImageCreateFlagBits := 256;
   VK_IMAGE_CREATE_DISJOINT_BIT_KHR : constant VkImageCreateFlagBits := 512;
   VK_IMAGE_CREATE_ALIAS_BIT_KHR : constant VkImageCreateFlagBits := 1024;
   VK_IMAGE_CREATE_FLAG_BITS_MAX_ENUM : constant VkImageCreateFlagBits := 2147483647;  -- vulkan_core.h:1595

   subtype VkImageCreateFlags is VkFlags;  -- vulkan_core.h:1619

   subtype VkSampleCountFlagBits is unsigned;
   VK_SAMPLE_COUNT_1_BIT : constant VkSampleCountFlagBits := 1;
   VK_SAMPLE_COUNT_2_BIT : constant VkSampleCountFlagBits := 2;
   VK_SAMPLE_COUNT_4_BIT : constant VkSampleCountFlagBits := 4;
   VK_SAMPLE_COUNT_8_BIT : constant VkSampleCountFlagBits := 8;
   VK_SAMPLE_COUNT_16_BIT : constant VkSampleCountFlagBits := 16;
   VK_SAMPLE_COUNT_32_BIT : constant VkSampleCountFlagBits := 32;
   VK_SAMPLE_COUNT_64_BIT : constant VkSampleCountFlagBits := 64;
   VK_SAMPLE_COUNT_FLAG_BITS_MAX_ENUM : constant VkSampleCountFlagBits := 2147483647;  -- vulkan_core.h:1621

   subtype VkSampleCountFlags is VkFlags;  -- vulkan_core.h:1631

   subtype VkImageUsageFlagBits is unsigned;
   VK_IMAGE_USAGE_TRANSFER_SRC_BIT : constant VkImageUsageFlagBits := 1;
   VK_IMAGE_USAGE_TRANSFER_DST_BIT : constant VkImageUsageFlagBits := 2;
   VK_IMAGE_USAGE_SAMPLED_BIT : constant VkImageUsageFlagBits := 4;
   VK_IMAGE_USAGE_STORAGE_BIT : constant VkImageUsageFlagBits := 8;
   VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT : constant VkImageUsageFlagBits := 16;
   VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT : constant VkImageUsageFlagBits := 32;
   VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT : constant VkImageUsageFlagBits := 64;
   VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT : constant VkImageUsageFlagBits := 128;
   VK_IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV : constant VkImageUsageFlagBits := 256;
   VK_IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT : constant VkImageUsageFlagBits := 512;
   VK_IMAGE_USAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR : constant VkImageUsageFlagBits := 256;
   VK_IMAGE_USAGE_FLAG_BITS_MAX_ENUM : constant VkImageUsageFlagBits := 2147483647;  -- vulkan_core.h:1633

   subtype VkImageUsageFlags is VkFlags;  -- vulkan_core.h:1647

   subtype VkInstanceCreateFlags is VkFlags;  -- vulkan_core.h:1648

   subtype VkMemoryHeapFlagBits is unsigned;
   VK_MEMORY_HEAP_DEVICE_LOCAL_BIT : constant VkMemoryHeapFlagBits := 1;
   VK_MEMORY_HEAP_MULTI_INSTANCE_BIT : constant VkMemoryHeapFlagBits := 2;
   VK_MEMORY_HEAP_MULTI_INSTANCE_BIT_KHR : constant VkMemoryHeapFlagBits := 2;
   VK_MEMORY_HEAP_FLAG_BITS_MAX_ENUM : constant VkMemoryHeapFlagBits := 2147483647;  -- vulkan_core.h:1650

   subtype VkMemoryHeapFlags is VkFlags;  -- vulkan_core.h:1656

   subtype VkMemoryPropertyFlagBits is unsigned;
   VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT : constant VkMemoryPropertyFlagBits := 1;
   VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT : constant VkMemoryPropertyFlagBits := 2;
   VK_MEMORY_PROPERTY_HOST_COHERENT_BIT : constant VkMemoryPropertyFlagBits := 4;
   VK_MEMORY_PROPERTY_HOST_CACHED_BIT : constant VkMemoryPropertyFlagBits := 8;
   VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT : constant VkMemoryPropertyFlagBits := 16;
   VK_MEMORY_PROPERTY_PROTECTED_BIT : constant VkMemoryPropertyFlagBits := 32;
   VK_MEMORY_PROPERTY_DEVICE_COHERENT_BIT_AMD : constant VkMemoryPropertyFlagBits := 64;
   VK_MEMORY_PROPERTY_DEVICE_UNCACHED_BIT_AMD : constant VkMemoryPropertyFlagBits := 128;
   VK_MEMORY_PROPERTY_FLAG_BITS_MAX_ENUM : constant VkMemoryPropertyFlagBits := 2147483647;  -- vulkan_core.h:1658

   subtype VkMemoryPropertyFlags is VkFlags;  -- vulkan_core.h:1669

   subtype VkQueueFlagBits is unsigned;
   VK_QUEUE_GRAPHICS_BIT : constant VkQueueFlagBits := 1;
   VK_QUEUE_COMPUTE_BIT : constant VkQueueFlagBits := 2;
   VK_QUEUE_TRANSFER_BIT : constant VkQueueFlagBits := 4;
   VK_QUEUE_SPARSE_BINDING_BIT : constant VkQueueFlagBits := 8;
   VK_QUEUE_PROTECTED_BIT : constant VkQueueFlagBits := 16;
   VK_QUEUE_FLAG_BITS_MAX_ENUM : constant VkQueueFlagBits := 2147483647;  -- vulkan_core.h:1671

   subtype VkQueueFlags is VkFlags;  -- vulkan_core.h:1679

   subtype VkDeviceCreateFlags is VkFlags;  -- vulkan_core.h:1680

   subtype VkDeviceQueueCreateFlagBits is unsigned;
   VK_DEVICE_QUEUE_CREATE_PROTECTED_BIT : constant VkDeviceQueueCreateFlagBits := 1;
   VK_DEVICE_QUEUE_CREATE_FLAG_BITS_MAX_ENUM : constant VkDeviceQueueCreateFlagBits := 2147483647;  -- vulkan_core.h:1682

   subtype VkDeviceQueueCreateFlags is VkFlags;  -- vulkan_core.h:1686

   subtype VkPipelineStageFlagBits is unsigned;
   VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT : constant VkPipelineStageFlagBits := 1;
   VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT : constant VkPipelineStageFlagBits := 2;
   VK_PIPELINE_STAGE_VERTEX_INPUT_BIT : constant VkPipelineStageFlagBits := 4;
   VK_PIPELINE_STAGE_VERTEX_SHADER_BIT : constant VkPipelineStageFlagBits := 8;
   VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT : constant VkPipelineStageFlagBits := 16;
   VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT : constant VkPipelineStageFlagBits := 32;
   VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT : constant VkPipelineStageFlagBits := 64;
   VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT : constant VkPipelineStageFlagBits := 128;
   VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT : constant VkPipelineStageFlagBits := 256;
   VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT : constant VkPipelineStageFlagBits := 512;
   VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT : constant VkPipelineStageFlagBits := 1024;
   VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT : constant VkPipelineStageFlagBits := 2048;
   VK_PIPELINE_STAGE_TRANSFER_BIT : constant VkPipelineStageFlagBits := 4096;
   VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT : constant VkPipelineStageFlagBits := 8192;
   VK_PIPELINE_STAGE_HOST_BIT : constant VkPipelineStageFlagBits := 16384;
   VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT : constant VkPipelineStageFlagBits := 32768;
   VK_PIPELINE_STAGE_ALL_COMMANDS_BIT : constant VkPipelineStageFlagBits := 65536;
   VK_PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT : constant VkPipelineStageFlagBits := 16777216;
   VK_PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT : constant VkPipelineStageFlagBits := 262144;
   VK_PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_KHR : constant VkPipelineStageFlagBits := 33554432;
   VK_PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_KHR : constant VkPipelineStageFlagBits := 2097152;
   VK_PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV : constant VkPipelineStageFlagBits := 4194304;
   VK_PIPELINE_STAGE_TASK_SHADER_BIT_NV : constant VkPipelineStageFlagBits := 524288;
   VK_PIPELINE_STAGE_MESH_SHADER_BIT_NV : constant VkPipelineStageFlagBits := 1048576;
   VK_PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT : constant VkPipelineStageFlagBits := 8388608;
   VK_PIPELINE_STAGE_COMMAND_PREPROCESS_BIT_NV : constant VkPipelineStageFlagBits := 131072;
   VK_PIPELINE_STAGE_NONE_KHR : constant VkPipelineStageFlagBits := 0;
   VK_PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_NV : constant VkPipelineStageFlagBits := 2097152;
   VK_PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_NV : constant VkPipelineStageFlagBits := 33554432;
   VK_PIPELINE_STAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR : constant VkPipelineStageFlagBits := 4194304;
   VK_PIPELINE_STAGE_FLAG_BITS_MAX_ENUM : constant VkPipelineStageFlagBits := 2147483647;  -- vulkan_core.h:1688

   subtype VkPipelineStageFlags is VkFlags;  -- vulkan_core.h:1721

   subtype VkMemoryMapFlags is VkFlags;  -- vulkan_core.h:1722

   subtype VkSparseMemoryBindFlagBits is unsigned;
   VK_SPARSE_MEMORY_BIND_METADATA_BIT : constant VkSparseMemoryBindFlagBits := 1;
   VK_SPARSE_MEMORY_BIND_FLAG_BITS_MAX_ENUM : constant VkSparseMemoryBindFlagBits := 2147483647;  -- vulkan_core.h:1724

   subtype VkSparseMemoryBindFlags is VkFlags;  -- vulkan_core.h:1728

   subtype VkSparseImageFormatFlagBits is unsigned;
   VK_SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT : constant VkSparseImageFormatFlagBits := 1;
   VK_SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT : constant VkSparseImageFormatFlagBits := 2;
   VK_SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT : constant VkSparseImageFormatFlagBits := 4;
   VK_SPARSE_IMAGE_FORMAT_FLAG_BITS_MAX_ENUM : constant VkSparseImageFormatFlagBits := 2147483647;  -- vulkan_core.h:1730

   subtype VkSparseImageFormatFlags is VkFlags;  -- vulkan_core.h:1736

   subtype VkFenceCreateFlagBits is unsigned;
   VK_FENCE_CREATE_SIGNALED_BIT : constant VkFenceCreateFlagBits := 1;
   VK_FENCE_CREATE_FLAG_BITS_MAX_ENUM : constant VkFenceCreateFlagBits := 2147483647;  -- vulkan_core.h:1738

   subtype VkFenceCreateFlags is VkFlags;  -- vulkan_core.h:1742

   subtype VkSemaphoreCreateFlags is VkFlags;  -- vulkan_core.h:1743

   subtype VkEventCreateFlagBits is unsigned;
   VK_EVENT_CREATE_DEVICE_ONLY_BIT_KHR : constant VkEventCreateFlagBits := 1;
   VK_EVENT_CREATE_FLAG_BITS_MAX_ENUM : constant VkEventCreateFlagBits := 2147483647;  -- vulkan_core.h:1745

   subtype VkEventCreateFlags is VkFlags;  -- vulkan_core.h:1749

   subtype VkQueryPipelineStatisticFlagBits is unsigned;
   VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT : constant VkQueryPipelineStatisticFlagBits := 1;
   VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT : constant VkQueryPipelineStatisticFlagBits := 2;
   VK_QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT : constant VkQueryPipelineStatisticFlagBits := 4;
   VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT : constant VkQueryPipelineStatisticFlagBits := 8;
   VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT : constant VkQueryPipelineStatisticFlagBits := 16;
   VK_QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT : constant VkQueryPipelineStatisticFlagBits := 32;
   VK_QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT : constant VkQueryPipelineStatisticFlagBits := 64;
   VK_QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT : constant VkQueryPipelineStatisticFlagBits := 128;
   VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT : constant VkQueryPipelineStatisticFlagBits := 256;
   VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT : constant VkQueryPipelineStatisticFlagBits := 512;
   VK_QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT : constant VkQueryPipelineStatisticFlagBits := 1024;
   VK_QUERY_PIPELINE_STATISTIC_FLAG_BITS_MAX_ENUM : constant VkQueryPipelineStatisticFlagBits := 2147483647;  -- vulkan_core.h:1751

   subtype VkQueryPipelineStatisticFlags is VkFlags;  -- vulkan_core.h:1765

   subtype VkQueryPoolCreateFlags is VkFlags;  -- vulkan_core.h:1766

   subtype VkQueryResultFlagBits is unsigned;
   VK_QUERY_RESULT_64_BIT : constant VkQueryResultFlagBits := 1;
   VK_QUERY_RESULT_WAIT_BIT : constant VkQueryResultFlagBits := 2;
   VK_QUERY_RESULT_WITH_AVAILABILITY_BIT : constant VkQueryResultFlagBits := 4;
   VK_QUERY_RESULT_PARTIAL_BIT : constant VkQueryResultFlagBits := 8;
   VK_QUERY_RESULT_FLAG_BITS_MAX_ENUM : constant VkQueryResultFlagBits := 2147483647;  -- vulkan_core.h:1768

   subtype VkQueryResultFlags is VkFlags;  -- vulkan_core.h:1775

   subtype VkBufferCreateFlagBits is unsigned;
   VK_BUFFER_CREATE_SPARSE_BINDING_BIT : constant VkBufferCreateFlagBits := 1;
   VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT : constant VkBufferCreateFlagBits := 2;
   VK_BUFFER_CREATE_SPARSE_ALIASED_BIT : constant VkBufferCreateFlagBits := 4;
   VK_BUFFER_CREATE_PROTECTED_BIT : constant VkBufferCreateFlagBits := 8;
   VK_BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT : constant VkBufferCreateFlagBits := 16;
   VK_BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_EXT : constant VkBufferCreateFlagBits := 16;
   VK_BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_KHR : constant VkBufferCreateFlagBits := 16;
   VK_BUFFER_CREATE_FLAG_BITS_MAX_ENUM : constant VkBufferCreateFlagBits := 2147483647;  -- vulkan_core.h:1777

   subtype VkBufferCreateFlags is VkFlags;  -- vulkan_core.h:1787

   subtype VkBufferUsageFlagBits is unsigned;
   VK_BUFFER_USAGE_TRANSFER_SRC_BIT : constant VkBufferUsageFlagBits := 1;
   VK_BUFFER_USAGE_TRANSFER_DST_BIT : constant VkBufferUsageFlagBits := 2;
   VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT : constant VkBufferUsageFlagBits := 4;
   VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT : constant VkBufferUsageFlagBits := 8;
   VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT : constant VkBufferUsageFlagBits := 16;
   VK_BUFFER_USAGE_STORAGE_BUFFER_BIT : constant VkBufferUsageFlagBits := 32;
   VK_BUFFER_USAGE_INDEX_BUFFER_BIT : constant VkBufferUsageFlagBits := 64;
   VK_BUFFER_USAGE_VERTEX_BUFFER_BIT : constant VkBufferUsageFlagBits := 128;
   VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT : constant VkBufferUsageFlagBits := 256;
   VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT : constant VkBufferUsageFlagBits := 131072;
   VK_BUFFER_USAGE_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT : constant VkBufferUsageFlagBits := 2048;
   VK_BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT : constant VkBufferUsageFlagBits := 4096;
   VK_BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT : constant VkBufferUsageFlagBits := 512;
   VK_BUFFER_USAGE_ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR : constant VkBufferUsageFlagBits := 524288;
   VK_BUFFER_USAGE_ACCELERATION_STRUCTURE_STORAGE_BIT_KHR : constant VkBufferUsageFlagBits := 1048576;
   VK_BUFFER_USAGE_SHADER_BINDING_TABLE_BIT_KHR : constant VkBufferUsageFlagBits := 1024;
   VK_BUFFER_USAGE_RAY_TRACING_BIT_NV : constant VkBufferUsageFlagBits := 1024;
   VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_EXT : constant VkBufferUsageFlagBits := 131072;
   VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_KHR : constant VkBufferUsageFlagBits := 131072;
   VK_BUFFER_USAGE_FLAG_BITS_MAX_ENUM : constant VkBufferUsageFlagBits := 2147483647;  -- vulkan_core.h:1789

   subtype VkBufferUsageFlags is VkFlags;  -- vulkan_core.h:1811

   subtype VkBufferViewCreateFlags is VkFlags;  -- vulkan_core.h:1812

   subtype VkImageViewCreateFlagBits is unsigned;
   VK_IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT : constant VkImageViewCreateFlagBits := 1;
   VK_IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DEFERRED_BIT_EXT : constant VkImageViewCreateFlagBits := 2;
   VK_IMAGE_VIEW_CREATE_FLAG_BITS_MAX_ENUM : constant VkImageViewCreateFlagBits := 2147483647;  -- vulkan_core.h:1814

   subtype VkImageViewCreateFlags is VkFlags;  -- vulkan_core.h:1819

   subtype VkShaderModuleCreateFlagBits is unsigned;
   VK_SHADER_MODULE_CREATE_FLAG_BITS_MAX_ENUM : constant VkShaderModuleCreateFlagBits := 2147483647;  -- vulkan_core.h:1821

   subtype VkShaderModuleCreateFlags is VkFlags;  -- vulkan_core.h:1824

   subtype VkPipelineCacheCreateFlagBits is unsigned;
   VK_PIPELINE_CACHE_CREATE_EXTERNALLY_SYNCHRONIZED_BIT_EXT : constant VkPipelineCacheCreateFlagBits := 1;
   VK_PIPELINE_CACHE_CREATE_FLAG_BITS_MAX_ENUM : constant VkPipelineCacheCreateFlagBits := 2147483647;  -- vulkan_core.h:1826

   subtype VkPipelineCacheCreateFlags is VkFlags;  -- vulkan_core.h:1830

   subtype VkColorComponentFlagBits is unsigned;
   VK_COLOR_COMPONENT_R_BIT : constant VkColorComponentFlagBits := 1;
   VK_COLOR_COMPONENT_G_BIT : constant VkColorComponentFlagBits := 2;
   VK_COLOR_COMPONENT_B_BIT : constant VkColorComponentFlagBits := 4;
   VK_COLOR_COMPONENT_A_BIT : constant VkColorComponentFlagBits := 8;
   VK_COLOR_COMPONENT_FLAG_BITS_MAX_ENUM : constant VkColorComponentFlagBits := 2147483647;  -- vulkan_core.h:1832

   subtype VkColorComponentFlags is VkFlags;  -- vulkan_core.h:1839

   subtype VkPipelineCreateFlagBits is unsigned;
   VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT : constant VkPipelineCreateFlagBits := 1;
   VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT : constant VkPipelineCreateFlagBits := 2;
   VK_PIPELINE_CREATE_DERIVATIVE_BIT : constant VkPipelineCreateFlagBits := 4;
   VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT : constant VkPipelineCreateFlagBits := 8;
   VK_PIPELINE_CREATE_DISPATCH_BASE_BIT : constant VkPipelineCreateFlagBits := 16;
   VK_PIPELINE_CREATE_RAY_TRACING_NO_NULL_ANY_HIT_SHADERS_BIT_KHR : constant VkPipelineCreateFlagBits := 16384;
   VK_PIPELINE_CREATE_RAY_TRACING_NO_NULL_CLOSEST_HIT_SHADERS_BIT_KHR : constant VkPipelineCreateFlagBits := 32768;
   VK_PIPELINE_CREATE_RAY_TRACING_NO_NULL_MISS_SHADERS_BIT_KHR : constant VkPipelineCreateFlagBits := 65536;
   VK_PIPELINE_CREATE_RAY_TRACING_NO_NULL_INTERSECTION_SHADERS_BIT_KHR : constant VkPipelineCreateFlagBits := 131072;
   VK_PIPELINE_CREATE_RAY_TRACING_SKIP_TRIANGLES_BIT_KHR : constant VkPipelineCreateFlagBits := 4096;
   VK_PIPELINE_CREATE_RAY_TRACING_SKIP_AABBS_BIT_KHR : constant VkPipelineCreateFlagBits := 8192;
   VK_PIPELINE_CREATE_RAY_TRACING_SHADER_GROUP_HANDLE_CAPTURE_REPLAY_BIT_KHR : constant VkPipelineCreateFlagBits := 524288;
   VK_PIPELINE_CREATE_DEFER_COMPILE_BIT_NV : constant VkPipelineCreateFlagBits := 32;
   VK_PIPELINE_CREATE_CAPTURE_STATISTICS_BIT_KHR : constant VkPipelineCreateFlagBits := 64;
   VK_PIPELINE_CREATE_CAPTURE_INTERNAL_REPRESENTATIONS_BIT_KHR : constant VkPipelineCreateFlagBits := 128;
   VK_PIPELINE_CREATE_INDIRECT_BINDABLE_BIT_NV : constant VkPipelineCreateFlagBits := 262144;
   VK_PIPELINE_CREATE_LIBRARY_BIT_KHR : constant VkPipelineCreateFlagBits := 2048;
   VK_PIPELINE_CREATE_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT_EXT : constant VkPipelineCreateFlagBits := 256;
   VK_PIPELINE_CREATE_EARLY_RETURN_ON_FAILURE_BIT_EXT : constant VkPipelineCreateFlagBits := 512;
   VK_PIPELINE_CREATE_DISPATCH_BASE : constant VkPipelineCreateFlagBits := 16;
   VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT_KHR : constant VkPipelineCreateFlagBits := 8;
   VK_PIPELINE_CREATE_DISPATCH_BASE_KHR : constant VkPipelineCreateFlagBits := 16;
   VK_PIPELINE_CREATE_FLAG_BITS_MAX_ENUM : constant VkPipelineCreateFlagBits := 2147483647;  -- vulkan_core.h:1841

   subtype VkPipelineCreateFlags is VkFlags;  -- vulkan_core.h:1866

   subtype VkPipelineShaderStageCreateFlagBits is unsigned;
   VK_PIPELINE_SHADER_STAGE_CREATE_ALLOW_VARYING_SUBGROUP_SIZE_BIT_EXT : constant VkPipelineShaderStageCreateFlagBits := 1;
   VK_PIPELINE_SHADER_STAGE_CREATE_REQUIRE_FULL_SUBGROUPS_BIT_EXT : constant VkPipelineShaderStageCreateFlagBits := 2;
   VK_PIPELINE_SHADER_STAGE_CREATE_FLAG_BITS_MAX_ENUM : constant VkPipelineShaderStageCreateFlagBits := 2147483647;  -- vulkan_core.h:1868

   subtype VkPipelineShaderStageCreateFlags is VkFlags;  -- vulkan_core.h:1873

   subtype VkShaderStageFlagBits is unsigned;
   VK_SHADER_STAGE_VERTEX_BIT : constant VkShaderStageFlagBits := 1;
   VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT : constant VkShaderStageFlagBits := 2;
   VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT : constant VkShaderStageFlagBits := 4;
   VK_SHADER_STAGE_GEOMETRY_BIT : constant VkShaderStageFlagBits := 8;
   VK_SHADER_STAGE_FRAGMENT_BIT : constant VkShaderStageFlagBits := 16;
   VK_SHADER_STAGE_COMPUTE_BIT : constant VkShaderStageFlagBits := 32;
   VK_SHADER_STAGE_ALL_GRAPHICS : constant VkShaderStageFlagBits := 31;
   VK_SHADER_STAGE_ALL : constant VkShaderStageFlagBits := 2147483647;
   VK_SHADER_STAGE_RAYGEN_BIT_KHR : constant VkShaderStageFlagBits := 256;
   VK_SHADER_STAGE_ANY_HIT_BIT_KHR : constant VkShaderStageFlagBits := 512;
   VK_SHADER_STAGE_CLOSEST_HIT_BIT_KHR : constant VkShaderStageFlagBits := 1024;
   VK_SHADER_STAGE_MISS_BIT_KHR : constant VkShaderStageFlagBits := 2048;
   VK_SHADER_STAGE_INTERSECTION_BIT_KHR : constant VkShaderStageFlagBits := 4096;
   VK_SHADER_STAGE_CALLABLE_BIT_KHR : constant VkShaderStageFlagBits := 8192;
   VK_SHADER_STAGE_TASK_BIT_NV : constant VkShaderStageFlagBits := 64;
   VK_SHADER_STAGE_MESH_BIT_NV : constant VkShaderStageFlagBits := 128;
   VK_SHADER_STAGE_RAYGEN_BIT_NV : constant VkShaderStageFlagBits := 256;
   VK_SHADER_STAGE_ANY_HIT_BIT_NV : constant VkShaderStageFlagBits := 512;
   VK_SHADER_STAGE_CLOSEST_HIT_BIT_NV : constant VkShaderStageFlagBits := 1024;
   VK_SHADER_STAGE_MISS_BIT_NV : constant VkShaderStageFlagBits := 2048;
   VK_SHADER_STAGE_INTERSECTION_BIT_NV : constant VkShaderStageFlagBits := 4096;
   VK_SHADER_STAGE_CALLABLE_BIT_NV : constant VkShaderStageFlagBits := 8192;
   VK_SHADER_STAGE_FLAG_BITS_MAX_ENUM : constant VkShaderStageFlagBits := 2147483647;  -- vulkan_core.h:1875

   subtype VkCullModeFlagBits is unsigned;
   VK_CULL_MODE_NONE : constant VkCullModeFlagBits := 0;
   VK_CULL_MODE_FRONT_BIT : constant VkCullModeFlagBits := 1;
   VK_CULL_MODE_BACK_BIT : constant VkCullModeFlagBits := 2;
   VK_CULL_MODE_FRONT_AND_BACK : constant VkCullModeFlagBits := 3;
   VK_CULL_MODE_FLAG_BITS_MAX_ENUM : constant VkCullModeFlagBits := 2147483647;  -- vulkan_core.h:1901

   subtype VkCullModeFlags is VkFlags;  -- vulkan_core.h:1908

   subtype VkPipelineVertexInputStateCreateFlags is VkFlags;  -- vulkan_core.h:1909

   subtype VkPipelineInputAssemblyStateCreateFlags is VkFlags;  -- vulkan_core.h:1910

   subtype VkPipelineTessellationStateCreateFlags is VkFlags;  -- vulkan_core.h:1911

   subtype VkPipelineViewportStateCreateFlags is VkFlags;  -- vulkan_core.h:1912

   subtype VkPipelineRasterizationStateCreateFlags is VkFlags;  -- vulkan_core.h:1913

   subtype VkPipelineMultisampleStateCreateFlags is VkFlags;  -- vulkan_core.h:1914

   subtype VkPipelineDepthStencilStateCreateFlags is VkFlags;  -- vulkan_core.h:1915

   subtype VkPipelineColorBlendStateCreateFlags is VkFlags;  -- vulkan_core.h:1916

   subtype VkPipelineDynamicStateCreateFlags is VkFlags;  -- vulkan_core.h:1917

   subtype VkPipelineLayoutCreateFlags is VkFlags;  -- vulkan_core.h:1918

   subtype VkShaderStageFlags is VkFlags;  -- vulkan_core.h:1919

   subtype VkSamplerCreateFlagBits is unsigned;
   VK_SAMPLER_CREATE_SUBSAMPLED_BIT_EXT : constant VkSamplerCreateFlagBits := 1;
   VK_SAMPLER_CREATE_SUBSAMPLED_COARSE_RECONSTRUCTION_BIT_EXT : constant VkSamplerCreateFlagBits := 2;
   VK_SAMPLER_CREATE_FLAG_BITS_MAX_ENUM : constant VkSamplerCreateFlagBits := 2147483647;  -- vulkan_core.h:1921

   subtype VkSamplerCreateFlags is VkFlags;  -- vulkan_core.h:1926

   subtype VkDescriptorPoolCreateFlagBits is unsigned;
   VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT : constant VkDescriptorPoolCreateFlagBits := 1;
   VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT : constant VkDescriptorPoolCreateFlagBits := 2;
   VK_DESCRIPTOR_POOL_CREATE_HOST_ONLY_BIT_VALVE : constant VkDescriptorPoolCreateFlagBits := 4;
   VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT : constant VkDescriptorPoolCreateFlagBits := 2;
   VK_DESCRIPTOR_POOL_CREATE_FLAG_BITS_MAX_ENUM : constant VkDescriptorPoolCreateFlagBits := 2147483647;  -- vulkan_core.h:1928

   subtype VkDescriptorPoolCreateFlags is VkFlags;  -- vulkan_core.h:1935

   subtype VkDescriptorPoolResetFlags is VkFlags;  -- vulkan_core.h:1936

   subtype VkDescriptorSetLayoutCreateFlagBits is unsigned;
   VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT : constant VkDescriptorSetLayoutCreateFlagBits := 2;
   VK_DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR : constant VkDescriptorSetLayoutCreateFlagBits := 1;
   VK_DESCRIPTOR_SET_LAYOUT_CREATE_HOST_ONLY_POOL_BIT_VALVE : constant VkDescriptorSetLayoutCreateFlagBits := 4;
   VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT : constant VkDescriptorSetLayoutCreateFlagBits := 2;
   VK_DESCRIPTOR_SET_LAYOUT_CREATE_FLAG_BITS_MAX_ENUM : constant VkDescriptorSetLayoutCreateFlagBits := 2147483647;  -- vulkan_core.h:1938

   subtype VkDescriptorSetLayoutCreateFlags is VkFlags;  -- vulkan_core.h:1945

   subtype VkAttachmentDescriptionFlagBits is unsigned;
   VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT : constant VkAttachmentDescriptionFlagBits := 1;
   VK_ATTACHMENT_DESCRIPTION_FLAG_BITS_MAX_ENUM : constant VkAttachmentDescriptionFlagBits := 2147483647;  -- vulkan_core.h:1947

   subtype VkAttachmentDescriptionFlags is VkFlags;  -- vulkan_core.h:1951

   subtype VkDependencyFlagBits is unsigned;
   VK_DEPENDENCY_BY_REGION_BIT : constant VkDependencyFlagBits := 1;
   VK_DEPENDENCY_DEVICE_GROUP_BIT : constant VkDependencyFlagBits := 4;
   VK_DEPENDENCY_VIEW_LOCAL_BIT : constant VkDependencyFlagBits := 2;
   VK_DEPENDENCY_VIEW_LOCAL_BIT_KHR : constant VkDependencyFlagBits := 2;
   VK_DEPENDENCY_DEVICE_GROUP_BIT_KHR : constant VkDependencyFlagBits := 4;
   VK_DEPENDENCY_FLAG_BITS_MAX_ENUM : constant VkDependencyFlagBits := 2147483647;  -- vulkan_core.h:1953

   subtype VkDependencyFlags is VkFlags;  -- vulkan_core.h:1961

   subtype VkFramebufferCreateFlagBits is unsigned;
   VK_FRAMEBUFFER_CREATE_IMAGELESS_BIT : constant VkFramebufferCreateFlagBits := 1;
   VK_FRAMEBUFFER_CREATE_IMAGELESS_BIT_KHR : constant VkFramebufferCreateFlagBits := 1;
   VK_FRAMEBUFFER_CREATE_FLAG_BITS_MAX_ENUM : constant VkFramebufferCreateFlagBits := 2147483647;  -- vulkan_core.h:1963

   subtype VkFramebufferCreateFlags is VkFlags;  -- vulkan_core.h:1968

   subtype VkRenderPassCreateFlagBits is unsigned;
   VK_RENDER_PASS_CREATE_TRANSFORM_BIT_QCOM : constant VkRenderPassCreateFlagBits := 2;
   VK_RENDER_PASS_CREATE_FLAG_BITS_MAX_ENUM : constant VkRenderPassCreateFlagBits := 2147483647;  -- vulkan_core.h:1970

   subtype VkRenderPassCreateFlags is VkFlags;  -- vulkan_core.h:1974

   subtype VkSubpassDescriptionFlagBits is unsigned;
   VK_SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX : constant VkSubpassDescriptionFlagBits := 1;
   VK_SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX : constant VkSubpassDescriptionFlagBits := 2;
   VK_SUBPASS_DESCRIPTION_FRAGMENT_REGION_BIT_QCOM : constant VkSubpassDescriptionFlagBits := 4;
   VK_SUBPASS_DESCRIPTION_SHADER_RESOLVE_BIT_QCOM : constant VkSubpassDescriptionFlagBits := 8;
   VK_SUBPASS_DESCRIPTION_FLAG_BITS_MAX_ENUM : constant VkSubpassDescriptionFlagBits := 2147483647;  -- vulkan_core.h:1976

   subtype VkSubpassDescriptionFlags is VkFlags;  -- vulkan_core.h:1983

   subtype VkCommandPoolCreateFlagBits is unsigned;
   VK_COMMAND_POOL_CREATE_TRANSIENT_BIT : constant VkCommandPoolCreateFlagBits := 1;
   VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT : constant VkCommandPoolCreateFlagBits := 2;
   VK_COMMAND_POOL_CREATE_PROTECTED_BIT : constant VkCommandPoolCreateFlagBits := 4;
   VK_COMMAND_POOL_CREATE_FLAG_BITS_MAX_ENUM : constant VkCommandPoolCreateFlagBits := 2147483647;  -- vulkan_core.h:1985

   subtype VkCommandPoolCreateFlags is VkFlags;  -- vulkan_core.h:1991

   subtype VkCommandPoolResetFlagBits is unsigned;
   VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT : constant VkCommandPoolResetFlagBits := 1;
   VK_COMMAND_POOL_RESET_FLAG_BITS_MAX_ENUM : constant VkCommandPoolResetFlagBits := 2147483647;  -- vulkan_core.h:1993

   subtype VkCommandPoolResetFlags is VkFlags;  -- vulkan_core.h:1997

   subtype VkCommandBufferUsageFlagBits is unsigned;
   VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT : constant VkCommandBufferUsageFlagBits := 1;
   VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT : constant VkCommandBufferUsageFlagBits := 2;
   VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT : constant VkCommandBufferUsageFlagBits := 4;
   VK_COMMAND_BUFFER_USAGE_FLAG_BITS_MAX_ENUM : constant VkCommandBufferUsageFlagBits := 2147483647;  -- vulkan_core.h:1999

   subtype VkCommandBufferUsageFlags is VkFlags;  -- vulkan_core.h:2005

   subtype VkQueryControlFlagBits is unsigned;
   VK_QUERY_CONTROL_PRECISE_BIT : constant VkQueryControlFlagBits := 1;
   VK_QUERY_CONTROL_FLAG_BITS_MAX_ENUM : constant VkQueryControlFlagBits := 2147483647;  -- vulkan_core.h:2007

   subtype VkQueryControlFlags is VkFlags;  -- vulkan_core.h:2011

   subtype VkCommandBufferResetFlagBits is unsigned;
   VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT : constant VkCommandBufferResetFlagBits := 1;
   VK_COMMAND_BUFFER_RESET_FLAG_BITS_MAX_ENUM : constant VkCommandBufferResetFlagBits := 2147483647;  -- vulkan_core.h:2013

   subtype VkCommandBufferResetFlags is VkFlags;  -- vulkan_core.h:2017

   subtype VkStencilFaceFlagBits is unsigned;
   VK_STENCIL_FACE_FRONT_BIT : constant VkStencilFaceFlagBits := 1;
   VK_STENCIL_FACE_BACK_BIT : constant VkStencilFaceFlagBits := 2;
   VK_STENCIL_FACE_FRONT_AND_BACK : constant VkStencilFaceFlagBits := 3;
   VK_STENCIL_FRONT_AND_BACK : constant VkStencilFaceFlagBits := 3;
   VK_STENCIL_FACE_FLAG_BITS_MAX_ENUM : constant VkStencilFaceFlagBits := 2147483647;  -- vulkan_core.h:2019

   subtype VkStencilFaceFlags is VkFlags;  -- vulkan_core.h:2026

   type VkExtent2D is record
      width : aliased stdint_h.uint32_t;  -- vulkan_core.h:2028
      height : aliased stdint_h.uint32_t;  -- vulkan_core.h:2029
   end record;
   pragma Convention (C_Pass_By_Copy, VkExtent2D);  -- vulkan_core.h:2027

   type VkExtent3D is record
      width : aliased stdint_h.uint32_t;  -- vulkan_core.h:2033
      height : aliased stdint_h.uint32_t;  -- vulkan_core.h:2034
      depth : aliased stdint_h.uint32_t;  -- vulkan_core.h:2035
   end record;
   pragma Convention (C_Pass_By_Copy, VkExtent3D);  -- vulkan_core.h:2032

   type VkOffset2D is record
      x : aliased stdint_h.int32_t;  -- vulkan_core.h:2039
      y : aliased stdint_h.int32_t;  -- vulkan_core.h:2040
   end record;
   pragma Convention (C_Pass_By_Copy, VkOffset2D);  -- vulkan_core.h:2038

   type VkOffset3D is record
      x : aliased stdint_h.int32_t;  -- vulkan_core.h:2044
      y : aliased stdint_h.int32_t;  -- vulkan_core.h:2045
      z : aliased stdint_h.int32_t;  -- vulkan_core.h:2046
   end record;
   pragma Convention (C_Pass_By_Copy, VkOffset3D);  -- vulkan_core.h:2043

   type VkRect2D is record
      offset : aliased VkOffset2D;  -- vulkan_core.h:2050
      extent : aliased VkExtent2D;  -- vulkan_core.h:2051
   end record;
   pragma Convention (C_Pass_By_Copy, VkRect2D);  -- vulkan_core.h:2049

   type VkBaseInStructure is record
      sType : aliased VkStructureType;  -- vulkan_core.h:2055
      pNext : access constant VkBaseInStructure;  -- vulkan_core.h:2056
   end record;
   pragma Convention (C_Pass_By_Copy, VkBaseInStructure);  -- vulkan_core.h:2054

   type VkBaseOutStructure is record
      sType : aliased VkStructureType;  -- vulkan_core.h:2060
      pNext : access VkBaseOutStructure;  -- vulkan_core.h:2061
   end record;
   pragma Convention (C_Pass_By_Copy, VkBaseOutStructure);  -- vulkan_core.h:2059

   type VkBufferMemoryBarrier is record
      sType : aliased VkStructureType;  -- vulkan_core.h:2065
      pNext : System.Address;  -- vulkan_core.h:2066
      srcAccessMask : aliased VkAccessFlags;  -- vulkan_core.h:2067
      dstAccessMask : aliased VkAccessFlags;  -- vulkan_core.h:2068
      srcQueueFamilyIndex : aliased stdint_h.uint32_t;  -- vulkan_core.h:2069
      dstQueueFamilyIndex : aliased stdint_h.uint32_t;  -- vulkan_core.h:2070
      buffer : VkBuffer;  -- vulkan_core.h:2071
      offset : aliased VkDeviceSize;  -- vulkan_core.h:2072
      size : aliased VkDeviceSize;  -- vulkan_core.h:2073
   end record;
   pragma Convention (C_Pass_By_Copy, VkBufferMemoryBarrier);  -- vulkan_core.h:2064

   type VkDispatchIndirectCommand is record
      x : aliased stdint_h.uint32_t;  -- vulkan_core.h:2077
      y : aliased stdint_h.uint32_t;  -- vulkan_core.h:2078
      z : aliased stdint_h.uint32_t;  -- vulkan_core.h:2079
   end record;
   pragma Convention (C_Pass_By_Copy, VkDispatchIndirectCommand);  -- vulkan_core.h:2076

   type VkDrawIndexedIndirectCommand is record
      indexCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:2083
      instanceCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:2084
      firstIndex : aliased stdint_h.uint32_t;  -- vulkan_core.h:2085
      vertexOffset : aliased stdint_h.int32_t;  -- vulkan_core.h:2086
      firstInstance : aliased stdint_h.uint32_t;  -- vulkan_core.h:2087
   end record;
   pragma Convention (C_Pass_By_Copy, VkDrawIndexedIndirectCommand);  -- vulkan_core.h:2082

   type VkDrawIndirectCommand is record
      vertexCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:2091
      instanceCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:2092
      firstVertex : aliased stdint_h.uint32_t;  -- vulkan_core.h:2093
      firstInstance : aliased stdint_h.uint32_t;  -- vulkan_core.h:2094
   end record;
   pragma Convention (C_Pass_By_Copy, VkDrawIndirectCommand);  -- vulkan_core.h:2090

   type VkImageSubresourceRange is record
      aspectMask : aliased VkImageAspectFlags;  -- vulkan_core.h:2098
      baseMipLevel : aliased stdint_h.uint32_t;  -- vulkan_core.h:2099
      levelCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:2100
      baseArrayLayer : aliased stdint_h.uint32_t;  -- vulkan_core.h:2101
      layerCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:2102
   end record;
   pragma Convention (C_Pass_By_Copy, VkImageSubresourceRange);  -- vulkan_core.h:2097

   type VkImageMemoryBarrier is record
      sType : aliased VkStructureType;  -- vulkan_core.h:2106
      pNext : System.Address;  -- vulkan_core.h:2107
      srcAccessMask : aliased VkAccessFlags;  -- vulkan_core.h:2108
      dstAccessMask : aliased VkAccessFlags;  -- vulkan_core.h:2109
      oldLayout : aliased VkImageLayout;  -- vulkan_core.h:2110
      newLayout : aliased VkImageLayout;  -- vulkan_core.h:2111
      srcQueueFamilyIndex : aliased stdint_h.uint32_t;  -- vulkan_core.h:2112
      dstQueueFamilyIndex : aliased stdint_h.uint32_t;  -- vulkan_core.h:2113
      image : VkImage;  -- vulkan_core.h:2114
      subresourceRange : aliased VkImageSubresourceRange;  -- vulkan_core.h:2115
   end record;
   pragma Convention (C_Pass_By_Copy, VkImageMemoryBarrier);  -- vulkan_core.h:2105

   type VkMemoryBarrier is record
      sType : aliased VkStructureType;  -- vulkan_core.h:2119
      pNext : System.Address;  -- vulkan_core.h:2120
      srcAccessMask : aliased VkAccessFlags;  -- vulkan_core.h:2121
      dstAccessMask : aliased VkAccessFlags;  -- vulkan_core.h:2122
   end record;
   pragma Convention (C_Pass_By_Copy, VkMemoryBarrier);  -- vulkan_core.h:2118

   type PFN_vkAllocationFunction is access function
        (arg1 : System.Address;
         arg2 : crtdefs_h.size_t;
         arg3 : crtdefs_h.size_t;
         arg4 : VkSystemAllocationScope) return System.Address;
   pragma Convention (C, PFN_vkAllocationFunction);  -- vulkan_core.h:2125

   type PFN_vkFreeFunction is access procedure (arg1 : System.Address; arg2 : System.Address);
   pragma Convention (C, PFN_vkFreeFunction);  -- vulkan_core.h:2131

   type PFN_vkInternalAllocationNotification is access procedure
        (arg1 : System.Address;
         arg2 : crtdefs_h.size_t;
         arg3 : VkInternalAllocationType;
         arg4 : VkSystemAllocationScope);
   pragma Convention (C, PFN_vkInternalAllocationNotification);  -- vulkan_core.h:2135

   type PFN_vkInternalFreeNotification is access procedure
        (arg1 : System.Address;
         arg2 : crtdefs_h.size_t;
         arg3 : VkInternalAllocationType;
         arg4 : VkSystemAllocationScope);
   pragma Convention (C, PFN_vkInternalFreeNotification);  -- vulkan_core.h:2141

   type PFN_vkReallocationFunction is access function
        (arg1 : System.Address;
         arg2 : System.Address;
         arg3 : crtdefs_h.size_t;
         arg4 : crtdefs_h.size_t;
         arg5 : VkSystemAllocationScope) return System.Address;
   pragma Convention (C, PFN_vkReallocationFunction);  -- vulkan_core.h:2147

   type PFN_vkVoidFunction is access procedure;
   pragma Convention (C, PFN_vkVoidFunction);  -- vulkan_core.h:2154

   type VkAllocationCallbacks is record
      pUserData : System.Address;  -- vulkan_core.h:2156
      pfnAllocation : PFN_vkAllocationFunction;  -- vulkan_core.h:2157
      pfnReallocation : PFN_vkReallocationFunction;  -- vulkan_core.h:2158
      pfnFree : PFN_vkFreeFunction;  -- vulkan_core.h:2159
      pfnInternalAllocation : PFN_vkInternalAllocationNotification;  -- vulkan_core.h:2160
      pfnInternalFree : PFN_vkInternalFreeNotification;  -- vulkan_core.h:2161
   end record;
   pragma Convention (C_Pass_By_Copy, VkAllocationCallbacks);  -- vulkan_core.h:2155

   type VkApplicationInfo is record
      sType : aliased VkStructureType := VK_STRUCTURE_TYPE_APPLICATION_INFO;
      pNext : System.Address := System.Null_Address ;
      pApplicationName : Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.Null_Ptr;
      applicationVersion : aliased stdint_h.uint32_t := 0;
      pEngineName : Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.Null_Ptr;
      engineVersion : aliased stdint_h.uint32_t := 0;
      apiVersion : aliased stdint_h.uint32_t; -- In absense of macro to use here, should force using software to initialize.
   end record;
   pragma Convention (C_Pass_By_Copy, VkApplicationInfo);  -- vulkan_core.h:2164

   type VkFormatProperties is record
      linearTilingFeatures : aliased VkFormatFeatureFlags;  -- vulkan_core.h:2175
      optimalTilingFeatures : aliased VkFormatFeatureFlags;  -- vulkan_core.h:2176
      bufferFeatures : aliased VkFormatFeatureFlags;  -- vulkan_core.h:2177
   end record;
   pragma Convention (C_Pass_By_Copy, VkFormatProperties);  -- vulkan_core.h:2174

   type VkImageFormatProperties is record
      maxExtent : aliased VkExtent3D;  -- vulkan_core.h:2181
      maxMipLevels : aliased stdint_h.uint32_t;  -- vulkan_core.h:2182
      maxArrayLayers : aliased stdint_h.uint32_t;  -- vulkan_core.h:2183
      sampleCounts : aliased VkSampleCountFlags;  -- vulkan_core.h:2184
      maxResourceSize : aliased VkDeviceSize;  -- vulkan_core.h:2185
   end record;
   pragma Convention (C_Pass_By_Copy, VkImageFormatProperties);  -- vulkan_core.h:2180

   type VkInstanceCreateInfo is record
      sType : aliased VkStructureType := VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO;
      pNext : System.Address := System.Null_Address ;
      flags : aliased VkInstanceCreateFlags := 0;
      pApplicationInfo : System.Address := System.Null_Address ;
      enabledLayerCount : aliased stdint_h.uint32_t := 0;
      ppEnabledLayerNames : System.Address := System.Null_Address ;
      enabledExtensionCount : aliased stdint_h.uint32_t := 0;
      ppEnabledExtensionNames : System.Address := System.Null_Address ;
   end record;
   pragma Convention (C_Pass_By_Copy, VkInstanceCreateInfo);  -- vulkan_core.h:2188

   type VkMemoryHeap is record
      size : aliased VkDeviceSize;  -- vulkan_core.h:2200
      flags : aliased VkMemoryHeapFlags;  -- vulkan_core.h:2201
   end record;
   pragma Convention (C_Pass_By_Copy, VkMemoryHeap);  -- vulkan_core.h:2199

   type VkMemoryType is record
      propertyFlags : aliased VkMemoryPropertyFlags;  -- vulkan_core.h:2205
      heapIndex : aliased stdint_h.uint32_t;  -- vulkan_core.h:2206
   end record;
   pragma Convention (C_Pass_By_Copy, VkMemoryType);  -- vulkan_core.h:2204

   type VkPhysicalDeviceFeatures is record
      robustBufferAccess : aliased VkBool32;  -- vulkan_core.h:2210
      fullDrawIndexUint32 : aliased VkBool32;  -- vulkan_core.h:2211
      imageCubeArray : aliased VkBool32;  -- vulkan_core.h:2212
      independentBlend : aliased VkBool32;  -- vulkan_core.h:2213
      geometryShader : aliased VkBool32;  -- vulkan_core.h:2214
      tessellationShader : aliased VkBool32;  -- vulkan_core.h:2215
      sampleRateShading : aliased VkBool32;  -- vulkan_core.h:2216
      dualSrcBlend : aliased VkBool32;  -- vulkan_core.h:2217
      logicOp : aliased VkBool32;  -- vulkan_core.h:2218
      multiDrawIndirect : aliased VkBool32;  -- vulkan_core.h:2219
      drawIndirectFirstInstance : aliased VkBool32;  -- vulkan_core.h:2220
      depthClamp : aliased VkBool32;  -- vulkan_core.h:2221
      depthBiasClamp : aliased VkBool32;  -- vulkan_core.h:2222
      fillModeNonSolid : aliased VkBool32;  -- vulkan_core.h:2223
      depthBounds : aliased VkBool32;  -- vulkan_core.h:2224
      wideLines : aliased VkBool32;  -- vulkan_core.h:2225
      largePoints : aliased VkBool32;  -- vulkan_core.h:2226
      alphaToOne : aliased VkBool32;  -- vulkan_core.h:2227
      multiViewport : aliased VkBool32;  -- vulkan_core.h:2228
      samplerAnisotropy : aliased VkBool32;  -- vulkan_core.h:2229
      textureCompressionETC2 : aliased VkBool32;  -- vulkan_core.h:2230
      textureCompressionASTC_LDR : aliased VkBool32;  -- vulkan_core.h:2231
      textureCompressionBC : aliased VkBool32;  -- vulkan_core.h:2232
      occlusionQueryPrecise : aliased VkBool32;  -- vulkan_core.h:2233
      pipelineStatisticsQuery : aliased VkBool32;  -- vulkan_core.h:2234
      vertexPipelineStoresAndAtomics : aliased VkBool32;  -- vulkan_core.h:2235
      fragmentStoresAndAtomics : aliased VkBool32;  -- vulkan_core.h:2236
      shaderTessellationAndGeometryPointSize : aliased VkBool32;  -- vulkan_core.h:2237
      shaderImageGatherExtended : aliased VkBool32;  -- vulkan_core.h:2238
      shaderStorageImageExtendedFormats : aliased VkBool32;  -- vulkan_core.h:2239
      shaderStorageImageMultisample : aliased VkBool32;  -- vulkan_core.h:2240
      shaderStorageImageReadWithoutFormat : aliased VkBool32;  -- vulkan_core.h:2241
      shaderStorageImageWriteWithoutFormat : aliased VkBool32;  -- vulkan_core.h:2242
      shaderUniformBufferArrayDynamicIndexing : aliased VkBool32;  -- vulkan_core.h:2243
      shaderSampledImageArrayDynamicIndexing : aliased VkBool32;  -- vulkan_core.h:2244
      shaderStorageBufferArrayDynamicIndexing : aliased VkBool32;  -- vulkan_core.h:2245
      shaderStorageImageArrayDynamicIndexing : aliased VkBool32;  -- vulkan_core.h:2246
      shaderClipDistance : aliased VkBool32;  -- vulkan_core.h:2247
      shaderCullDistance : aliased VkBool32;  -- vulkan_core.h:2248
      shaderFloat64 : aliased VkBool32;  -- vulkan_core.h:2249
      shaderInt64 : aliased VkBool32;  -- vulkan_core.h:2250
      shaderInt16 : aliased VkBool32;  -- vulkan_core.h:2251
      shaderResourceResidency : aliased VkBool32;  -- vulkan_core.h:2252
      shaderResourceMinLod : aliased VkBool32;  -- vulkan_core.h:2253
      sparseBinding : aliased VkBool32;  -- vulkan_core.h:2254
      sparseResidencyBuffer : aliased VkBool32;  -- vulkan_core.h:2255
      sparseResidencyImage2D : aliased VkBool32;  -- vulkan_core.h:2256
      sparseResidencyImage3D : aliased VkBool32;  -- vulkan_core.h:2257
      sparseResidency2Samples : aliased VkBool32;  -- vulkan_core.h:2258
      sparseResidency4Samples : aliased VkBool32;  -- vulkan_core.h:2259
      sparseResidency8Samples : aliased VkBool32;  -- vulkan_core.h:2260
      sparseResidency16Samples : aliased VkBool32;  -- vulkan_core.h:2261
      sparseResidencyAliased : aliased VkBool32;  -- vulkan_core.h:2262
      variableMultisampleRate : aliased VkBool32;  -- vulkan_core.h:2263
      inheritedQueries : aliased VkBool32;  -- vulkan_core.h:2264
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceFeatures);  -- vulkan_core.h:2209

   type VkPhysicalDeviceLimits_maxComputeWorkGroupCount_array is array (0 .. 2) of aliased stdint_h.uint32_t;
   type VkPhysicalDeviceLimits_maxComputeWorkGroupSize_array is array (0 .. 2) of aliased stdint_h.uint32_t;
   type VkPhysicalDeviceLimits_maxViewportDimensions_array is array (0 .. 1) of aliased stdint_h.uint32_t;
   type VkPhysicalDeviceLimits_viewportBoundsRange_array is array (0 .. 1) of aliased float;
   type VkPhysicalDeviceLimits_pointSizeRange_array is array (0 .. 1) of aliased float;
   type VkPhysicalDeviceLimits_lineWidthRange_array is array (0 .. 1) of aliased float;
   type VkPhysicalDeviceLimits is record
      maxImageDimension1D : aliased stdint_h.uint32_t;  -- vulkan_core.h:2268
      maxImageDimension2D : aliased stdint_h.uint32_t;  -- vulkan_core.h:2269
      maxImageDimension3D : aliased stdint_h.uint32_t;  -- vulkan_core.h:2270
      maxImageDimensionCube : aliased stdint_h.uint32_t;  -- vulkan_core.h:2271
      maxImageArrayLayers : aliased stdint_h.uint32_t;  -- vulkan_core.h:2272
      maxTexelBufferElements : aliased stdint_h.uint32_t;  -- vulkan_core.h:2273
      maxUniformBufferRange : aliased stdint_h.uint32_t;  -- vulkan_core.h:2274
      maxStorageBufferRange : aliased stdint_h.uint32_t;  -- vulkan_core.h:2275
      maxPushConstantsSize : aliased stdint_h.uint32_t;  -- vulkan_core.h:2276
      maxMemoryAllocationCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:2277
      maxSamplerAllocationCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:2278
      bufferImageGranularity : aliased VkDeviceSize;  -- vulkan_core.h:2279
      sparseAddressSpaceSize : aliased VkDeviceSize;  -- vulkan_core.h:2280
      maxBoundDescriptorSets : aliased stdint_h.uint32_t;  -- vulkan_core.h:2281
      maxPerStageDescriptorSamplers : aliased stdint_h.uint32_t;  -- vulkan_core.h:2282
      maxPerStageDescriptorUniformBuffers : aliased stdint_h.uint32_t;  -- vulkan_core.h:2283
      maxPerStageDescriptorStorageBuffers : aliased stdint_h.uint32_t;  -- vulkan_core.h:2284
      maxPerStageDescriptorSampledImages : aliased stdint_h.uint32_t;  -- vulkan_core.h:2285
      maxPerStageDescriptorStorageImages : aliased stdint_h.uint32_t;  -- vulkan_core.h:2286
      maxPerStageDescriptorInputAttachments : aliased stdint_h.uint32_t;  -- vulkan_core.h:2287
      maxPerStageResources : aliased stdint_h.uint32_t;  -- vulkan_core.h:2288
      maxDescriptorSetSamplers : aliased stdint_h.uint32_t;  -- vulkan_core.h:2289
      maxDescriptorSetUniformBuffers : aliased stdint_h.uint32_t;  -- vulkan_core.h:2290
      maxDescriptorSetUniformBuffersDynamic : aliased stdint_h.uint32_t;  -- vulkan_core.h:2291
      maxDescriptorSetStorageBuffers : aliased stdint_h.uint32_t;  -- vulkan_core.h:2292
      maxDescriptorSetStorageBuffersDynamic : aliased stdint_h.uint32_t;  -- vulkan_core.h:2293
      maxDescriptorSetSampledImages : aliased stdint_h.uint32_t;  -- vulkan_core.h:2294
      maxDescriptorSetStorageImages : aliased stdint_h.uint32_t;  -- vulkan_core.h:2295
      maxDescriptorSetInputAttachments : aliased stdint_h.uint32_t;  -- vulkan_core.h:2296
      maxVertexInputAttributes : aliased stdint_h.uint32_t;  -- vulkan_core.h:2297
      maxVertexInputBindings : aliased stdint_h.uint32_t;  -- vulkan_core.h:2298
      maxVertexInputAttributeOffset : aliased stdint_h.uint32_t;  -- vulkan_core.h:2299
      maxVertexInputBindingStride : aliased stdint_h.uint32_t;  -- vulkan_core.h:2300
      maxVertexOutputComponents : aliased stdint_h.uint32_t;  -- vulkan_core.h:2301
      maxTessellationGenerationLevel : aliased stdint_h.uint32_t;  -- vulkan_core.h:2302
      maxTessellationPatchSize : aliased stdint_h.uint32_t;  -- vulkan_core.h:2303
      maxTessellationControlPerVertexInputComponents : aliased stdint_h.uint32_t;  -- vulkan_core.h:2304
      maxTessellationControlPerVertexOutputComponents : aliased stdint_h.uint32_t;  -- vulkan_core.h:2305
      maxTessellationControlPerPatchOutputComponents : aliased stdint_h.uint32_t;  -- vulkan_core.h:2306
      maxTessellationControlTotalOutputComponents : aliased stdint_h.uint32_t;  -- vulkan_core.h:2307
      maxTessellationEvaluationInputComponents : aliased stdint_h.uint32_t;  -- vulkan_core.h:2308
      maxTessellationEvaluationOutputComponents : aliased stdint_h.uint32_t;  -- vulkan_core.h:2309
      maxGeometryShaderInvocations : aliased stdint_h.uint32_t;  -- vulkan_core.h:2310
      maxGeometryInputComponents : aliased stdint_h.uint32_t;  -- vulkan_core.h:2311
      maxGeometryOutputComponents : aliased stdint_h.uint32_t;  -- vulkan_core.h:2312
      maxGeometryOutputVertices : aliased stdint_h.uint32_t;  -- vulkan_core.h:2313
      maxGeometryTotalOutputComponents : aliased stdint_h.uint32_t;  -- vulkan_core.h:2314
      maxFragmentInputComponents : aliased stdint_h.uint32_t;  -- vulkan_core.h:2315
      maxFragmentOutputAttachments : aliased stdint_h.uint32_t;  -- vulkan_core.h:2316
      maxFragmentDualSrcAttachments : aliased stdint_h.uint32_t;  -- vulkan_core.h:2317
      maxFragmentCombinedOutputResources : aliased stdint_h.uint32_t;  -- vulkan_core.h:2318
      maxComputeSharedMemorySize : aliased stdint_h.uint32_t;  -- vulkan_core.h:2319
      maxComputeWorkGroupCount : aliased VkPhysicalDeviceLimits_maxComputeWorkGroupCount_array;  -- vulkan_core.h:2320
      maxComputeWorkGroupInvocations : aliased stdint_h.uint32_t;  -- vulkan_core.h:2321
      maxComputeWorkGroupSize : aliased VkPhysicalDeviceLimits_maxComputeWorkGroupSize_array;  -- vulkan_core.h:2322
      subPixelPrecisionBits : aliased stdint_h.uint32_t;  -- vulkan_core.h:2323
      subTexelPrecisionBits : aliased stdint_h.uint32_t;  -- vulkan_core.h:2324
      mipmapPrecisionBits : aliased stdint_h.uint32_t;  -- vulkan_core.h:2325
      maxDrawIndexedIndexValue : aliased stdint_h.uint32_t;  -- vulkan_core.h:2326
      maxDrawIndirectCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:2327
      maxSamplerLodBias : aliased float;  -- vulkan_core.h:2328
      maxSamplerAnisotropy : aliased float;  -- vulkan_core.h:2329
      maxViewports : aliased stdint_h.uint32_t;  -- vulkan_core.h:2330
      maxViewportDimensions : aliased VkPhysicalDeviceLimits_maxViewportDimensions_array;  -- vulkan_core.h:2331
      viewportBoundsRange : aliased VkPhysicalDeviceLimits_viewportBoundsRange_array;  -- vulkan_core.h:2332
      viewportSubPixelBits : aliased stdint_h.uint32_t;  -- vulkan_core.h:2333
      minMemoryMapAlignment : aliased crtdefs_h.size_t;  -- vulkan_core.h:2334
      minTexelBufferOffsetAlignment : aliased VkDeviceSize;  -- vulkan_core.h:2335
      minUniformBufferOffsetAlignment : aliased VkDeviceSize;  -- vulkan_core.h:2336
      minStorageBufferOffsetAlignment : aliased VkDeviceSize;  -- vulkan_core.h:2337
      minTexelOffset : aliased stdint_h.int32_t;  -- vulkan_core.h:2338
      maxTexelOffset : aliased stdint_h.uint32_t;  -- vulkan_core.h:2339
      minTexelGatherOffset : aliased stdint_h.int32_t;  -- vulkan_core.h:2340
      maxTexelGatherOffset : aliased stdint_h.uint32_t;  -- vulkan_core.h:2341
      minInterpolationOffset : aliased float;  -- vulkan_core.h:2342
      maxInterpolationOffset : aliased float;  -- vulkan_core.h:2343
      subPixelInterpolationOffsetBits : aliased stdint_h.uint32_t;  -- vulkan_core.h:2344
      maxFramebufferWidth : aliased stdint_h.uint32_t;  -- vulkan_core.h:2345
      maxFramebufferHeight : aliased stdint_h.uint32_t;  -- vulkan_core.h:2346
      maxFramebufferLayers : aliased stdint_h.uint32_t;  -- vulkan_core.h:2347
      framebufferColorSampleCounts : aliased VkSampleCountFlags;  -- vulkan_core.h:2348
      framebufferDepthSampleCounts : aliased VkSampleCountFlags;  -- vulkan_core.h:2349
      framebufferStencilSampleCounts : aliased VkSampleCountFlags;  -- vulkan_core.h:2350
      framebufferNoAttachmentsSampleCounts : aliased VkSampleCountFlags;  -- vulkan_core.h:2351
      maxColorAttachments : aliased stdint_h.uint32_t;  -- vulkan_core.h:2352
      sampledImageColorSampleCounts : aliased VkSampleCountFlags;  -- vulkan_core.h:2353
      sampledImageIntegerSampleCounts : aliased VkSampleCountFlags;  -- vulkan_core.h:2354
      sampledImageDepthSampleCounts : aliased VkSampleCountFlags;  -- vulkan_core.h:2355
      sampledImageStencilSampleCounts : aliased VkSampleCountFlags;  -- vulkan_core.h:2356
      storageImageSampleCounts : aliased VkSampleCountFlags;  -- vulkan_core.h:2357
      maxSampleMaskWords : aliased stdint_h.uint32_t;  -- vulkan_core.h:2358
      timestampComputeAndGraphics : aliased VkBool32;  -- vulkan_core.h:2359
      timestampPeriod : aliased float;  -- vulkan_core.h:2360
      maxClipDistances : aliased stdint_h.uint32_t;  -- vulkan_core.h:2361
      maxCullDistances : aliased stdint_h.uint32_t;  -- vulkan_core.h:2362
      maxCombinedClipAndCullDistances : aliased stdint_h.uint32_t;  -- vulkan_core.h:2363
      discreteQueuePriorities : aliased stdint_h.uint32_t;  -- vulkan_core.h:2364
      pointSizeRange : aliased VkPhysicalDeviceLimits_pointSizeRange_array;  -- vulkan_core.h:2365
      lineWidthRange : aliased VkPhysicalDeviceLimits_lineWidthRange_array;  -- vulkan_core.h:2366
      pointSizeGranularity : aliased float;  -- vulkan_core.h:2367
      lineWidthGranularity : aliased float;  -- vulkan_core.h:2368
      strictLines : aliased VkBool32;  -- vulkan_core.h:2369
      standardSampleLocations : aliased VkBool32;  -- vulkan_core.h:2370
      optimalBufferCopyOffsetAlignment : aliased VkDeviceSize;  -- vulkan_core.h:2371
      optimalBufferCopyRowPitchAlignment : aliased VkDeviceSize;  -- vulkan_core.h:2372
      nonCoherentAtomSize : aliased VkDeviceSize;  -- vulkan_core.h:2373
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceLimits);  -- vulkan_core.h:2267

   type VkPhysicalDeviceMemoryProperties_memoryTypes_array is array (0 .. 31) of aliased VkMemoryType;
   type VkPhysicalDeviceMemoryProperties_memoryHeaps_array is array (0 .. 15) of aliased VkMemoryHeap;
   type VkPhysicalDeviceMemoryProperties is record
      memoryTypeCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:2377
      memoryTypes : aliased VkPhysicalDeviceMemoryProperties_memoryTypes_array;  -- vulkan_core.h:2378
      memoryHeapCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:2379
      memoryHeaps : aliased VkPhysicalDeviceMemoryProperties_memoryHeaps_array;  -- vulkan_core.h:2380
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceMemoryProperties);  -- vulkan_core.h:2376

   type VkPhysicalDeviceSparseProperties is record
      residencyStandard2DBlockShape : aliased VkBool32;  -- vulkan_core.h:2384
      residencyStandard2DMultisampleBlockShape : aliased VkBool32;  -- vulkan_core.h:2385
      residencyStandard3DBlockShape : aliased VkBool32;  -- vulkan_core.h:2386
      residencyAlignedMipSize : aliased VkBool32;  -- vulkan_core.h:2387
      residencyNonResidentStrict : aliased VkBool32;  -- vulkan_core.h:2388
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceSparseProperties);  -- vulkan_core.h:2383

   subtype VkPhysicalDeviceProperties_deviceName_array is Interfaces.C.char_array (0 .. 255);
   type VkPhysicalDeviceProperties_pipelineCacheUUID_array is array (0 .. 15) of aliased stdint_h.uint8_t;
   type VkPhysicalDeviceProperties is record
      apiVersion : aliased stdint_h.uint32_t;  -- vulkan_core.h:2392
      driverVersion : aliased stdint_h.uint32_t;  -- vulkan_core.h:2393
      vendorID : aliased stdint_h.uint32_t;  -- vulkan_core.h:2394
      deviceID : aliased stdint_h.uint32_t;  -- vulkan_core.h:2395
      deviceType : aliased VkPhysicalDeviceType;  -- vulkan_core.h:2396
      deviceName : aliased VkPhysicalDeviceProperties_deviceName_array;  -- vulkan_core.h:2397
      pipelineCacheUUID : aliased VkPhysicalDeviceProperties_pipelineCacheUUID_array;  -- vulkan_core.h:2398
      limits : aliased VkPhysicalDeviceLimits;  -- vulkan_core.h:2399
      sparseProperties : aliased VkPhysicalDeviceSparseProperties;  -- vulkan_core.h:2400
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceProperties);  -- vulkan_core.h:2391

   type VkQueueFamilyProperties is record
      queueFlags : aliased VkQueueFlags;  -- vulkan_core.h:2404
      queueCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:2405
      timestampValidBits : aliased stdint_h.uint32_t;  -- vulkan_core.h:2406
      minImageTransferGranularity : aliased VkExtent3D;  -- vulkan_core.h:2407
   end record;
   pragma Convention (C_Pass_By_Copy, VkQueueFamilyProperties);  -- vulkan_core.h:2403

   type VkDeviceQueueCreateInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:2411
      pNext : System.Address;  -- vulkan_core.h:2412
      flags : aliased VkDeviceQueueCreateFlags;  -- vulkan_core.h:2413
      queueFamilyIndex : aliased stdint_h.uint32_t;  -- vulkan_core.h:2414
      queueCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:2415
      pQueuePriorities : access float;  -- vulkan_core.h:2416
   end record;
   pragma Convention (C_Pass_By_Copy, VkDeviceQueueCreateInfo);  -- vulkan_core.h:2410

   type VkDeviceCreateInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:2420
      pNext : System.Address;  -- vulkan_core.h:2421
      flags : aliased VkDeviceCreateFlags;  -- vulkan_core.h:2422
      queueCreateInfoCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:2423
      pQueueCreateInfos : System.Address;  -- vulkan_core.h:2424
      enabledLayerCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:2425
      ppEnabledLayerNames : System.Address;  -- vulkan_core.h:2426
      enabledExtensionCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:2427
      ppEnabledExtensionNames : System.Address;  -- vulkan_core.h:2428
      pEnabledFeatures : System.Address;  -- vulkan_core.h:2429
   end record;
   pragma Convention (C_Pass_By_Copy, VkDeviceCreateInfo);  -- vulkan_core.h:2419

   subtype VkExtensionProperties_extensionName_array is Interfaces.C.char_array (0 .. 255);
   type VkExtensionProperties is record
      extensionName : aliased VkExtensionProperties_extensionName_array;  -- vulkan_core.h:2433
      specVersion : aliased stdint_h.uint32_t;  -- vulkan_core.h:2434
   end record;
   pragma Convention (C_Pass_By_Copy, VkExtensionProperties);  -- vulkan_core.h:2432

   subtype VkLayerProperties_layerName_array is Interfaces.C.char_array (0 .. 255);
   subtype VkLayerProperties_description_array is Interfaces.C.char_array (0 .. 255);
   type VkLayerProperties is record
      layerName : aliased VkLayerProperties_layerName_array;  -- vulkan_core.h:2438
      specVersion : aliased stdint_h.uint32_t;  -- vulkan_core.h:2439
      implementationVersion : aliased stdint_h.uint32_t;  -- vulkan_core.h:2440
      description : aliased VkLayerProperties_description_array;  -- vulkan_core.h:2441
   end record;
   pragma Convention (C, VkLayerProperties);  -- vulkan_core.h:2437

   type VkSubmitInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:2445
      pNext : System.Address;  -- vulkan_core.h:2446
      waitSemaphoreCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:2447
      pWaitSemaphores : System.Address;  -- vulkan_core.h:2448
      pWaitDstStageMask : access VkPipelineStageFlags;  -- vulkan_core.h:2449
      commandBufferCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:2450
      pCommandBuffers : System.Address;  -- vulkan_core.h:2451
      signalSemaphoreCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:2452
      pSignalSemaphores : System.Address;  -- vulkan_core.h:2453
   end record;
   pragma Convention (C_Pass_By_Copy, VkSubmitInfo);  -- vulkan_core.h:2444

   type VkMappedMemoryRange is record
      sType : aliased VkStructureType;  -- vulkan_core.h:2457
      pNext : System.Address;  -- vulkan_core.h:2458
      memory : VkDeviceMemory;  -- vulkan_core.h:2459
      offset : aliased VkDeviceSize;  -- vulkan_core.h:2460
      size : aliased VkDeviceSize;  -- vulkan_core.h:2461
   end record;
   pragma Convention (C_Pass_By_Copy, VkMappedMemoryRange);  -- vulkan_core.h:2456

   type VkMemoryAllocateInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:2465
      pNext : System.Address;  -- vulkan_core.h:2466
      allocationSize : aliased VkDeviceSize;  -- vulkan_core.h:2467
      memoryTypeIndex : aliased stdint_h.uint32_t;  -- vulkan_core.h:2468
   end record;
   pragma Convention (C_Pass_By_Copy, VkMemoryAllocateInfo);  -- vulkan_core.h:2464

   type VkMemoryRequirements is record
      size : aliased VkDeviceSize;  -- vulkan_core.h:2472
      alignment : aliased VkDeviceSize;  -- vulkan_core.h:2473
      memoryTypeBits : aliased stdint_h.uint32_t;  -- vulkan_core.h:2474
   end record;
   pragma Convention (C_Pass_By_Copy, VkMemoryRequirements);  -- vulkan_core.h:2471

   type VkSparseMemoryBind is record
      resourceOffset : aliased VkDeviceSize;  -- vulkan_core.h:2478
      size : aliased VkDeviceSize;  -- vulkan_core.h:2479
      memory : VkDeviceMemory;  -- vulkan_core.h:2480
      memoryOffset : aliased VkDeviceSize;  -- vulkan_core.h:2481
      flags : aliased VkSparseMemoryBindFlags;  -- vulkan_core.h:2482
   end record;
   pragma Convention (C_Pass_By_Copy, VkSparseMemoryBind);  -- vulkan_core.h:2477

   type VkSparseBufferMemoryBindInfo is record
      buffer : VkBuffer;  -- vulkan_core.h:2486
      bindCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:2487
      pBinds : System.Address;  -- vulkan_core.h:2488
   end record;
   pragma Convention (C_Pass_By_Copy, VkSparseBufferMemoryBindInfo);  -- vulkan_core.h:2485

   type VkSparseImageOpaqueMemoryBindInfo is record
      image : VkImage;  -- vulkan_core.h:2492
      bindCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:2493
      pBinds : System.Address;  -- vulkan_core.h:2494
   end record;
   pragma Convention (C_Pass_By_Copy, VkSparseImageOpaqueMemoryBindInfo);  -- vulkan_core.h:2491

   type VkImageSubresource is record
      aspectMask : aliased VkImageAspectFlags;  -- vulkan_core.h:2498
      mipLevel : aliased stdint_h.uint32_t;  -- vulkan_core.h:2499
      arrayLayer : aliased stdint_h.uint32_t;  -- vulkan_core.h:2500
   end record;
   pragma Convention (C_Pass_By_Copy, VkImageSubresource);  -- vulkan_core.h:2497

   type VkSparseImageMemoryBind is record
      subresource : aliased VkImageSubresource;  -- vulkan_core.h:2504
      offset : aliased VkOffset3D;  -- vulkan_core.h:2505
      extent : aliased VkExtent3D;  -- vulkan_core.h:2506
      memory : VkDeviceMemory;  -- vulkan_core.h:2507
      memoryOffset : aliased VkDeviceSize;  -- vulkan_core.h:2508
      flags : aliased VkSparseMemoryBindFlags;  -- vulkan_core.h:2509
   end record;
   pragma Convention (C_Pass_By_Copy, VkSparseImageMemoryBind);  -- vulkan_core.h:2503

   type VkSparseImageMemoryBindInfo is record
      image : VkImage;  -- vulkan_core.h:2513
      bindCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:2514
      pBinds : System.Address;  -- vulkan_core.h:2515
   end record;
   pragma Convention (C_Pass_By_Copy, VkSparseImageMemoryBindInfo);  -- vulkan_core.h:2512

   type VkBindSparseInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:2519
      pNext : System.Address;  -- vulkan_core.h:2520
      waitSemaphoreCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:2521
      pWaitSemaphores : System.Address;  -- vulkan_core.h:2522
      bufferBindCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:2523
      pBufferBinds : System.Address;  -- vulkan_core.h:2524
      imageOpaqueBindCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:2525
      pImageOpaqueBinds : System.Address;  -- vulkan_core.h:2526
      imageBindCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:2527
      pImageBinds : System.Address;  -- vulkan_core.h:2528
      signalSemaphoreCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:2529
      pSignalSemaphores : System.Address;  -- vulkan_core.h:2530
   end record;
   pragma Convention (C_Pass_By_Copy, VkBindSparseInfo);  -- vulkan_core.h:2518

   type VkSparseImageFormatProperties is record
      aspectMask : aliased VkImageAspectFlags;  -- vulkan_core.h:2534
      imageGranularity : aliased VkExtent3D;  -- vulkan_core.h:2535
      flags : aliased VkSparseImageFormatFlags;  -- vulkan_core.h:2536
   end record;
   pragma Convention (C_Pass_By_Copy, VkSparseImageFormatProperties);  -- vulkan_core.h:2533

   type VkSparseImageMemoryRequirements is record
      formatProperties : aliased VkSparseImageFormatProperties;  -- vulkan_core.h:2540
      imageMipTailFirstLod : aliased stdint_h.uint32_t;  -- vulkan_core.h:2541
      imageMipTailSize : aliased VkDeviceSize;  -- vulkan_core.h:2542
      imageMipTailOffset : aliased VkDeviceSize;  -- vulkan_core.h:2543
      imageMipTailStride : aliased VkDeviceSize;  -- vulkan_core.h:2544
   end record;
   pragma Convention (C_Pass_By_Copy, VkSparseImageMemoryRequirements);  -- vulkan_core.h:2539

   type VkFenceCreateInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:2548
      pNext : System.Address;  -- vulkan_core.h:2549
      flags : aliased VkFenceCreateFlags;  -- vulkan_core.h:2550
   end record;
   pragma Convention (C_Pass_By_Copy, VkFenceCreateInfo);  -- vulkan_core.h:2547

   type VkSemaphoreCreateInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:2554
      pNext : System.Address;  -- vulkan_core.h:2555
      flags : aliased VkSemaphoreCreateFlags;  -- vulkan_core.h:2556
   end record;
   pragma Convention (C_Pass_By_Copy, VkSemaphoreCreateInfo);  -- vulkan_core.h:2553

   type VkEventCreateInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:2560
      pNext : System.Address;  -- vulkan_core.h:2561
      flags : aliased VkEventCreateFlags;  -- vulkan_core.h:2562
   end record;
   pragma Convention (C_Pass_By_Copy, VkEventCreateInfo);  -- vulkan_core.h:2559

   type VkQueryPoolCreateInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:2566
      pNext : System.Address;  -- vulkan_core.h:2567
      flags : aliased VkQueryPoolCreateFlags;  -- vulkan_core.h:2568
      queryType : aliased VkQueryType;  -- vulkan_core.h:2569
      queryCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:2570
      pipelineStatistics : aliased VkQueryPipelineStatisticFlags;  -- vulkan_core.h:2571
   end record;
   pragma Convention (C_Pass_By_Copy, VkQueryPoolCreateInfo);  -- vulkan_core.h:2565

   type VkBufferCreateInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:2575
      pNext : System.Address;  -- vulkan_core.h:2576
      flags : aliased VkBufferCreateFlags;  -- vulkan_core.h:2577
      size : aliased VkDeviceSize;  -- vulkan_core.h:2578
      usage : aliased VkBufferUsageFlags;  -- vulkan_core.h:2579
      sharingMode : aliased VkSharingMode;  -- vulkan_core.h:2580
      queueFamilyIndexCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:2581
      pQueueFamilyIndices : access stdint_h.uint32_t;  -- vulkan_core.h:2582
   end record;
   pragma Convention (C_Pass_By_Copy, VkBufferCreateInfo);  -- vulkan_core.h:2574

   type VkBufferViewCreateInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:2586
      pNext : System.Address;  -- vulkan_core.h:2587
      flags : aliased VkBufferViewCreateFlags;  -- vulkan_core.h:2588
      buffer : VkBuffer;  -- vulkan_core.h:2589
      format : aliased VkFormat;  -- vulkan_core.h:2590
      offset : aliased VkDeviceSize;  -- vulkan_core.h:2591
      c_range : aliased VkDeviceSize;  -- vulkan_core.h:2592
   end record;
   pragma Convention (C_Pass_By_Copy, VkBufferViewCreateInfo);  -- vulkan_core.h:2585

   type VkImageCreateInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:2596
      pNext : System.Address;  -- vulkan_core.h:2597
      flags : aliased VkImageCreateFlags;  -- vulkan_core.h:2598
      imageType : aliased VkImageType;  -- vulkan_core.h:2599
      format : aliased VkFormat;  -- vulkan_core.h:2600
      extent : aliased VkExtent3D;  -- vulkan_core.h:2601
      mipLevels : aliased stdint_h.uint32_t;  -- vulkan_core.h:2602
      arrayLayers : aliased stdint_h.uint32_t;  -- vulkan_core.h:2603
      samples : aliased VkSampleCountFlagBits;  -- vulkan_core.h:2604
      tiling : aliased VkImageTiling;  -- vulkan_core.h:2605
      usage : aliased VkImageUsageFlags;  -- vulkan_core.h:2606
      sharingMode : aliased VkSharingMode;  -- vulkan_core.h:2607
      queueFamilyIndexCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:2608
      pQueueFamilyIndices : access stdint_h.uint32_t;  -- vulkan_core.h:2609
      initialLayout : aliased VkImageLayout;  -- vulkan_core.h:2610
   end record;
   pragma Convention (C_Pass_By_Copy, VkImageCreateInfo);  -- vulkan_core.h:2595

   type VkSubresourceLayout is record
      offset : aliased VkDeviceSize;  -- vulkan_core.h:2614
      size : aliased VkDeviceSize;  -- vulkan_core.h:2615
      rowPitch : aliased VkDeviceSize;  -- vulkan_core.h:2616
      arrayPitch : aliased VkDeviceSize;  -- vulkan_core.h:2617
      depthPitch : aliased VkDeviceSize;  -- vulkan_core.h:2618
   end record;
   pragma Convention (C_Pass_By_Copy, VkSubresourceLayout);  -- vulkan_core.h:2613

   type VkComponentMapping is record
      r : aliased VkComponentSwizzle;  -- vulkan_core.h:2622
      g : aliased VkComponentSwizzle;  -- vulkan_core.h:2623
      b : aliased VkComponentSwizzle;  -- vulkan_core.h:2624
      a : aliased VkComponentSwizzle;  -- vulkan_core.h:2625
   end record;
   pragma Convention (C_Pass_By_Copy, VkComponentMapping);  -- vulkan_core.h:2621

   type VkImageViewCreateInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:2629
      pNext : System.Address;  -- vulkan_core.h:2630
      flags : aliased VkImageViewCreateFlags;  -- vulkan_core.h:2631
      image : VkImage;  -- vulkan_core.h:2632
      viewType : aliased VkImageViewType;  -- vulkan_core.h:2633
      format : aliased VkFormat;  -- vulkan_core.h:2634
      components : aliased VkComponentMapping;  -- vulkan_core.h:2635
      subresourceRange : aliased VkImageSubresourceRange;  -- vulkan_core.h:2636
   end record;
   pragma Convention (C_Pass_By_Copy, VkImageViewCreateInfo);  -- vulkan_core.h:2628

   type VkShaderModuleCreateInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:2640
      pNext : System.Address;  -- vulkan_core.h:2641
      flags : aliased VkShaderModuleCreateFlags;  -- vulkan_core.h:2642
      codeSize : aliased crtdefs_h.size_t;  -- vulkan_core.h:2643
      pCode : access stdint_h.uint32_t;  -- vulkan_core.h:2644
   end record;
   pragma Convention (C_Pass_By_Copy, VkShaderModuleCreateInfo);  -- vulkan_core.h:2639

   type VkPipelineCacheCreateInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:2648
      pNext : System.Address;  -- vulkan_core.h:2649
      flags : aliased VkPipelineCacheCreateFlags;  -- vulkan_core.h:2650
      initialDataSize : aliased crtdefs_h.size_t;  -- vulkan_core.h:2651
      pInitialData : System.Address;  -- vulkan_core.h:2652
   end record;
   pragma Convention (C_Pass_By_Copy, VkPipelineCacheCreateInfo);  -- vulkan_core.h:2647

   type VkSpecializationMapEntry is record
      constantID : aliased stdint_h.uint32_t;  -- vulkan_core.h:2656
      offset : aliased stdint_h.uint32_t;  -- vulkan_core.h:2657
      size : aliased crtdefs_h.size_t;  -- vulkan_core.h:2658
   end record;
   pragma Convention (C_Pass_By_Copy, VkSpecializationMapEntry);  -- vulkan_core.h:2655

   type VkSpecializationInfo is record
      mapEntryCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:2662
      pMapEntries : System.Address;  -- vulkan_core.h:2663
      dataSize : aliased crtdefs_h.size_t;  -- vulkan_core.h:2664
      pData : System.Address;  -- vulkan_core.h:2665
   end record;
   pragma Convention (C_Pass_By_Copy, VkSpecializationInfo);  -- vulkan_core.h:2661

   type VkPipelineShaderStageCreateInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:2669
      pNext : System.Address;  -- vulkan_core.h:2670
      flags : aliased VkPipelineShaderStageCreateFlags;  -- vulkan_core.h:2671
      stage : aliased VkShaderStageFlagBits;  -- vulkan_core.h:2672
      module : VkShaderModule;  -- vulkan_core.h:2673
      pName : Interfaces.C.Strings.chars_ptr;  -- vulkan_core.h:2674
      pSpecializationInfo : System.Address;  -- vulkan_core.h:2675
   end record;
   pragma Convention (C_Pass_By_Copy, VkPipelineShaderStageCreateInfo);  -- vulkan_core.h:2668

   type VkComputePipelineCreateInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:2679
      pNext : System.Address;  -- vulkan_core.h:2680
      flags : aliased VkPipelineCreateFlags;  -- vulkan_core.h:2681
      stage : aliased VkPipelineShaderStageCreateInfo;  -- vulkan_core.h:2682
      layout : VkPipelineLayout;  -- vulkan_core.h:2683
      basePipelineHandle : VkPipeline;  -- vulkan_core.h:2684
      basePipelineIndex : aliased stdint_h.int32_t;  -- vulkan_core.h:2685
   end record;
   pragma Convention (C_Pass_By_Copy, VkComputePipelineCreateInfo);  -- vulkan_core.h:2678

   type VkVertexInputBindingDescription is record
      binding : aliased stdint_h.uint32_t;  -- vulkan_core.h:2689
      stride : aliased stdint_h.uint32_t;  -- vulkan_core.h:2690
      inputRate : aliased VkVertexInputRate;  -- vulkan_core.h:2691
   end record;
   pragma Convention (C_Pass_By_Copy, VkVertexInputBindingDescription);  -- vulkan_core.h:2688

   type VkVertexInputAttributeDescription is record
      location : aliased stdint_h.uint32_t;  -- vulkan_core.h:2695
      binding : aliased stdint_h.uint32_t;  -- vulkan_core.h:2696
      format : aliased VkFormat;  -- vulkan_core.h:2697
      offset : aliased stdint_h.uint32_t;  -- vulkan_core.h:2698
   end record;
   pragma Convention (C_Pass_By_Copy, VkVertexInputAttributeDescription);  -- vulkan_core.h:2694

   type VkPipelineVertexInputStateCreateInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:2702
      pNext : System.Address;  -- vulkan_core.h:2703
      flags : aliased VkPipelineVertexInputStateCreateFlags;  -- vulkan_core.h:2704
      vertexBindingDescriptionCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:2705
      pVertexBindingDescriptions : System.Address;  -- vulkan_core.h:2706
      vertexAttributeDescriptionCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:2707
      pVertexAttributeDescriptions : System.Address;  -- vulkan_core.h:2708
   end record;
   pragma Convention (C_Pass_By_Copy, VkPipelineVertexInputStateCreateInfo);  -- vulkan_core.h:2701

   type VkPipelineInputAssemblyStateCreateInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:2712
      pNext : System.Address;  -- vulkan_core.h:2713
      flags : aliased VkPipelineInputAssemblyStateCreateFlags;  -- vulkan_core.h:2714
      topology : aliased VkPrimitiveTopology;  -- vulkan_core.h:2715
      primitiveRestartEnable : aliased VkBool32;  -- vulkan_core.h:2716
   end record;
   pragma Convention (C_Pass_By_Copy, VkPipelineInputAssemblyStateCreateInfo);  -- vulkan_core.h:2711

   type VkPipelineTessellationStateCreateInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:2720
      pNext : System.Address;  -- vulkan_core.h:2721
      flags : aliased VkPipelineTessellationStateCreateFlags;  -- vulkan_core.h:2722
      patchControlPoints : aliased stdint_h.uint32_t;  -- vulkan_core.h:2723
   end record;
   pragma Convention (C_Pass_By_Copy, VkPipelineTessellationStateCreateInfo);  -- vulkan_core.h:2719

   type VkViewport is record
      x : aliased float;  -- vulkan_core.h:2727
      y : aliased float;  -- vulkan_core.h:2728
      width : aliased float;  -- vulkan_core.h:2729
      height : aliased float;  -- vulkan_core.h:2730
      minDepth : aliased float;  -- vulkan_core.h:2731
      maxDepth : aliased float;  -- vulkan_core.h:2732
   end record;
   pragma Convention (C_Pass_By_Copy, VkViewport);  -- vulkan_core.h:2726

   type VkPipelineViewportStateCreateInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:2736
      pNext : System.Address;  -- vulkan_core.h:2737
      flags : aliased VkPipelineViewportStateCreateFlags;  -- vulkan_core.h:2738
      viewportCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:2739
      pViewports : System.Address;  -- vulkan_core.h:2740
      scissorCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:2741
      pScissors : System.Address;  -- vulkan_core.h:2742
   end record;
   pragma Convention (C_Pass_By_Copy, VkPipelineViewportStateCreateInfo);  -- vulkan_core.h:2735

   type VkPipelineRasterizationStateCreateInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:2746
      pNext : System.Address;  -- vulkan_core.h:2747
      flags : aliased VkPipelineRasterizationStateCreateFlags;  -- vulkan_core.h:2748
      depthClampEnable : aliased VkBool32;  -- vulkan_core.h:2749
      rasterizerDiscardEnable : aliased VkBool32;  -- vulkan_core.h:2750
      polygonMode : aliased VkPolygonMode;  -- vulkan_core.h:2751
      cullMode : aliased VkCullModeFlags;  -- vulkan_core.h:2752
      frontFace : aliased VkFrontFace;  -- vulkan_core.h:2753
      depthBiasEnable : aliased VkBool32;  -- vulkan_core.h:2754
      depthBiasConstantFactor : aliased float;  -- vulkan_core.h:2755
      depthBiasClamp : aliased float;  -- vulkan_core.h:2756
      depthBiasSlopeFactor : aliased float;  -- vulkan_core.h:2757
      lineWidth : aliased float;  -- vulkan_core.h:2758
   end record;
   pragma Convention (C_Pass_By_Copy, VkPipelineRasterizationStateCreateInfo);  -- vulkan_core.h:2745

   type VkPipelineMultisampleStateCreateInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:2762
      pNext : System.Address;  -- vulkan_core.h:2763
      flags : aliased VkPipelineMultisampleStateCreateFlags;  -- vulkan_core.h:2764
      rasterizationSamples : aliased VkSampleCountFlagBits;  -- vulkan_core.h:2765
      sampleShadingEnable : aliased VkBool32;  -- vulkan_core.h:2766
      minSampleShading : aliased float;  -- vulkan_core.h:2767
      pSampleMask : access VkSampleMask;  -- vulkan_core.h:2768
      alphaToCoverageEnable : aliased VkBool32;  -- vulkan_core.h:2769
      alphaToOneEnable : aliased VkBool32;  -- vulkan_core.h:2770
   end record;
   pragma Convention (C_Pass_By_Copy, VkPipelineMultisampleStateCreateInfo);  -- vulkan_core.h:2761

   type VkStencilOpState is record
      failOp : aliased VkStencilOp;  -- vulkan_core.h:2774
      passOp : aliased VkStencilOp;  -- vulkan_core.h:2775
      depthFailOp : aliased VkStencilOp;  -- vulkan_core.h:2776
      compareOp : aliased VkCompareOp;  -- vulkan_core.h:2777
      compareMask : aliased stdint_h.uint32_t;  -- vulkan_core.h:2778
      writeMask : aliased stdint_h.uint32_t;  -- vulkan_core.h:2779
      reference : aliased stdint_h.uint32_t;  -- vulkan_core.h:2780
   end record;
   pragma Convention (C_Pass_By_Copy, VkStencilOpState);  -- vulkan_core.h:2773

   type VkPipelineDepthStencilStateCreateInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:2784
      pNext : System.Address;  -- vulkan_core.h:2785
      flags : aliased VkPipelineDepthStencilStateCreateFlags;  -- vulkan_core.h:2786
      depthTestEnable : aliased VkBool32;  -- vulkan_core.h:2787
      depthWriteEnable : aliased VkBool32;  -- vulkan_core.h:2788
      depthCompareOp : aliased VkCompareOp;  -- vulkan_core.h:2789
      depthBoundsTestEnable : aliased VkBool32;  -- vulkan_core.h:2790
      stencilTestEnable : aliased VkBool32;  -- vulkan_core.h:2791
      front : aliased VkStencilOpState;  -- vulkan_core.h:2792
      back : aliased VkStencilOpState;  -- vulkan_core.h:2793
      minDepthBounds : aliased float;  -- vulkan_core.h:2794
      maxDepthBounds : aliased float;  -- vulkan_core.h:2795
   end record;
   pragma Convention (C_Pass_By_Copy, VkPipelineDepthStencilStateCreateInfo);  -- vulkan_core.h:2783

   type VkPipelineColorBlendAttachmentState is record
      blendEnable : aliased VkBool32;  -- vulkan_core.h:2799
      srcColorBlendFactor : aliased VkBlendFactor;  -- vulkan_core.h:2800
      dstColorBlendFactor : aliased VkBlendFactor;  -- vulkan_core.h:2801
      colorBlendOp : aliased VkBlendOp;  -- vulkan_core.h:2802
      srcAlphaBlendFactor : aliased VkBlendFactor;  -- vulkan_core.h:2803
      dstAlphaBlendFactor : aliased VkBlendFactor;  -- vulkan_core.h:2804
      alphaBlendOp : aliased VkBlendOp;  -- vulkan_core.h:2805
      colorWriteMask : aliased VkColorComponentFlags;  -- vulkan_core.h:2806
   end record;
   pragma Convention (C_Pass_By_Copy, VkPipelineColorBlendAttachmentState);  -- vulkan_core.h:2798

   type VkPipelineColorBlendStateCreateInfo_blendConstants_array is array (0 .. 3) of aliased float;
   type VkPipelineColorBlendStateCreateInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:2810
      pNext : System.Address;  -- vulkan_core.h:2811
      flags : aliased VkPipelineColorBlendStateCreateFlags;  -- vulkan_core.h:2812
      logicOpEnable : aliased VkBool32;  -- vulkan_core.h:2813
      logicOp : aliased VkLogicOp;  -- vulkan_core.h:2814
      attachmentCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:2815
      pAttachments : System.Address;  -- vulkan_core.h:2816
      blendConstants : aliased VkPipelineColorBlendStateCreateInfo_blendConstants_array;  -- vulkan_core.h:2817
   end record;
   pragma Convention (C_Pass_By_Copy, VkPipelineColorBlendStateCreateInfo);  -- vulkan_core.h:2809

   type VkPipelineDynamicStateCreateInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:2821
      pNext : System.Address;  -- vulkan_core.h:2822
      flags : aliased VkPipelineDynamicStateCreateFlags;  -- vulkan_core.h:2823
      dynamicStateCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:2824
      pDynamicStates : System.Address;  -- vulkan_core.h:2825
   end record;
   pragma Convention (C_Pass_By_Copy, VkPipelineDynamicStateCreateInfo);  -- vulkan_core.h:2820

   type VkGraphicsPipelineCreateInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:2829
      pNext : System.Address;  -- vulkan_core.h:2830
      flags : aliased VkPipelineCreateFlags;  -- vulkan_core.h:2831
      stageCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:2832
      pStages : System.Address;  -- vulkan_core.h:2833
      pVertexInputState : System.Address;  -- vulkan_core.h:2834
      pInputAssemblyState : System.Address;  -- vulkan_core.h:2835
      pTessellationState : System.Address;  -- vulkan_core.h:2836
      pViewportState : System.Address;  -- vulkan_core.h:2837
      pRasterizationState : System.Address;  -- vulkan_core.h:2838
      pMultisampleState : System.Address;  -- vulkan_core.h:2839
      pDepthStencilState : System.Address;  -- vulkan_core.h:2840
      pColorBlendState : System.Address;  -- vulkan_core.h:2841
      pDynamicState : System.Address;  -- vulkan_core.h:2842
      layout : VkPipelineLayout;  -- vulkan_core.h:2843
      renderPass : VkRenderPass;  -- vulkan_core.h:2844
      subpass : aliased stdint_h.uint32_t;  -- vulkan_core.h:2845
      basePipelineHandle : VkPipeline;  -- vulkan_core.h:2846
      basePipelineIndex : aliased stdint_h.int32_t;  -- vulkan_core.h:2847
   end record;
   pragma Convention (C_Pass_By_Copy, VkGraphicsPipelineCreateInfo);  -- vulkan_core.h:2828

   type VkPushConstantRange is record
      stageFlags : aliased VkShaderStageFlags;  -- vulkan_core.h:2851
      offset : aliased stdint_h.uint32_t;  -- vulkan_core.h:2852
      size : aliased stdint_h.uint32_t;  -- vulkan_core.h:2853
   end record;
   pragma Convention (C_Pass_By_Copy, VkPushConstantRange);  -- vulkan_core.h:2850

   type VkPipelineLayoutCreateInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:2857
      pNext : System.Address;  -- vulkan_core.h:2858
      flags : aliased VkPipelineLayoutCreateFlags;  -- vulkan_core.h:2859
      setLayoutCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:2860
      pSetLayouts : System.Address;  -- vulkan_core.h:2861
      pushConstantRangeCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:2862
      pPushConstantRanges : System.Address;  -- vulkan_core.h:2863
   end record;
   pragma Convention (C_Pass_By_Copy, VkPipelineLayoutCreateInfo);  -- vulkan_core.h:2856

   type VkSamplerCreateInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:2867
      pNext : System.Address;  -- vulkan_core.h:2868
      flags : aliased VkSamplerCreateFlags;  -- vulkan_core.h:2869
      magFilter : aliased VkFilter;  -- vulkan_core.h:2870
      minFilter : aliased VkFilter;  -- vulkan_core.h:2871
      mipmapMode : aliased VkSamplerMipmapMode;  -- vulkan_core.h:2872
      addressModeU : aliased VkSamplerAddressMode;  -- vulkan_core.h:2873
      addressModeV : aliased VkSamplerAddressMode;  -- vulkan_core.h:2874
      addressModeW : aliased VkSamplerAddressMode;  -- vulkan_core.h:2875
      mipLodBias : aliased float;  -- vulkan_core.h:2876
      anisotropyEnable : aliased VkBool32;  -- vulkan_core.h:2877
      maxAnisotropy : aliased float;  -- vulkan_core.h:2878
      compareEnable : aliased VkBool32;  -- vulkan_core.h:2879
      compareOp : aliased VkCompareOp;  -- vulkan_core.h:2880
      minLod : aliased float;  -- vulkan_core.h:2881
      maxLod : aliased float;  -- vulkan_core.h:2882
      borderColor : aliased VkBorderColor;  -- vulkan_core.h:2883
      unnormalizedCoordinates : aliased VkBool32;  -- vulkan_core.h:2884
   end record;
   pragma Convention (C_Pass_By_Copy, VkSamplerCreateInfo);  -- vulkan_core.h:2866

   type VkCopyDescriptorSet is record
      sType : aliased VkStructureType;  -- vulkan_core.h:2888
      pNext : System.Address;  -- vulkan_core.h:2889
      srcSet : VkDescriptorSet;  -- vulkan_core.h:2890
      srcBinding : aliased stdint_h.uint32_t;  -- vulkan_core.h:2891
      srcArrayElement : aliased stdint_h.uint32_t;  -- vulkan_core.h:2892
      dstSet : VkDescriptorSet;  -- vulkan_core.h:2893
      dstBinding : aliased stdint_h.uint32_t;  -- vulkan_core.h:2894
      dstArrayElement : aliased stdint_h.uint32_t;  -- vulkan_core.h:2895
      descriptorCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:2896
   end record;
   pragma Convention (C_Pass_By_Copy, VkCopyDescriptorSet);  -- vulkan_core.h:2887

   type VkDescriptorBufferInfo is record
      buffer : VkBuffer;  -- vulkan_core.h:2900
      offset : aliased VkDeviceSize;  -- vulkan_core.h:2901
      c_range : aliased VkDeviceSize;  -- vulkan_core.h:2902
   end record;
   pragma Convention (C_Pass_By_Copy, VkDescriptorBufferInfo);  -- vulkan_core.h:2899

   type VkDescriptorImageInfo is record
      sampler : VkSampler;  -- vulkan_core.h:2906
      imageView : VkImageView;  -- vulkan_core.h:2907
      imageLayout : aliased VkImageLayout;  -- vulkan_core.h:2908
   end record;
   pragma Convention (C_Pass_By_Copy, VkDescriptorImageInfo);  -- vulkan_core.h:2905

   type VkDescriptorPoolSize is record
      c_type : aliased VkDescriptorType;  -- vulkan_core.h:2912
      descriptorCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:2913
   end record;
   pragma Convention (C_Pass_By_Copy, VkDescriptorPoolSize);  -- vulkan_core.h:2911

   type VkDescriptorPoolCreateInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:2917
      pNext : System.Address;  -- vulkan_core.h:2918
      flags : aliased VkDescriptorPoolCreateFlags;  -- vulkan_core.h:2919
      maxSets : aliased stdint_h.uint32_t;  -- vulkan_core.h:2920
      poolSizeCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:2921
      pPoolSizes : System.Address;  -- vulkan_core.h:2922
   end record;
   pragma Convention (C_Pass_By_Copy, VkDescriptorPoolCreateInfo);  -- vulkan_core.h:2916

   type VkDescriptorSetAllocateInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:2926
      pNext : System.Address;  -- vulkan_core.h:2927
      descriptorPool : VkDescriptorPool;  -- vulkan_core.h:2928
      descriptorSetCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:2929
      pSetLayouts : System.Address;  -- vulkan_core.h:2930
   end record;
   pragma Convention (C_Pass_By_Copy, VkDescriptorSetAllocateInfo);  -- vulkan_core.h:2925

   type VkDescriptorSetLayoutBinding is record
      binding : aliased stdint_h.uint32_t;  -- vulkan_core.h:2934
      descriptorType : aliased VkDescriptorType;  -- vulkan_core.h:2935
      descriptorCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:2936
      stageFlags : aliased VkShaderStageFlags;  -- vulkan_core.h:2937
      pImmutableSamplers : System.Address;  -- vulkan_core.h:2938
   end record;
   pragma Convention (C_Pass_By_Copy, VkDescriptorSetLayoutBinding);  -- vulkan_core.h:2933

   type VkDescriptorSetLayoutCreateInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:2942
      pNext : System.Address;  -- vulkan_core.h:2943
      flags : aliased VkDescriptorSetLayoutCreateFlags;  -- vulkan_core.h:2944
      bindingCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:2945
      pBindings : System.Address;  -- vulkan_core.h:2946
   end record;
   pragma Convention (C_Pass_By_Copy, VkDescriptorSetLayoutCreateInfo);  -- vulkan_core.h:2941

   type VkWriteDescriptorSet is record
      sType : aliased VkStructureType;  -- vulkan_core.h:2950
      pNext : System.Address;  -- vulkan_core.h:2951
      dstSet : VkDescriptorSet;  -- vulkan_core.h:2952
      dstBinding : aliased stdint_h.uint32_t;  -- vulkan_core.h:2953
      dstArrayElement : aliased stdint_h.uint32_t;  -- vulkan_core.h:2954
      descriptorCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:2955
      descriptorType : aliased VkDescriptorType;  -- vulkan_core.h:2956
      pImageInfo : System.Address;  -- vulkan_core.h:2957
      pBufferInfo : System.Address;  -- vulkan_core.h:2958
      pTexelBufferView : System.Address;  -- vulkan_core.h:2959
   end record;
   pragma Convention (C_Pass_By_Copy, VkWriteDescriptorSet);  -- vulkan_core.h:2949

   type VkAttachmentDescription is record
      flags : aliased VkAttachmentDescriptionFlags;  -- vulkan_core.h:2963
      format : aliased VkFormat;  -- vulkan_core.h:2964
      samples : aliased VkSampleCountFlagBits;  -- vulkan_core.h:2965
      loadOp : aliased VkAttachmentLoadOp;  -- vulkan_core.h:2966
      storeOp : aliased VkAttachmentStoreOp;  -- vulkan_core.h:2967
      stencilLoadOp : aliased VkAttachmentLoadOp;  -- vulkan_core.h:2968
      stencilStoreOp : aliased VkAttachmentStoreOp;  -- vulkan_core.h:2969
      initialLayout : aliased VkImageLayout;  -- vulkan_core.h:2970
      finalLayout : aliased VkImageLayout;  -- vulkan_core.h:2971
   end record;
   pragma Convention (C_Pass_By_Copy, VkAttachmentDescription);  -- vulkan_core.h:2962

   type VkAttachmentReference is record
      attachment : aliased stdint_h.uint32_t;  -- vulkan_core.h:2975
      layout : aliased VkImageLayout;  -- vulkan_core.h:2976
   end record;
   pragma Convention (C_Pass_By_Copy, VkAttachmentReference);  -- vulkan_core.h:2974

   type VkFramebufferCreateInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:2980
      pNext : System.Address;  -- vulkan_core.h:2981
      flags : aliased VkFramebufferCreateFlags;  -- vulkan_core.h:2982
      renderPass : VkRenderPass;  -- vulkan_core.h:2983
      attachmentCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:2984
      pAttachments : System.Address;  -- vulkan_core.h:2985
      width : aliased stdint_h.uint32_t;  -- vulkan_core.h:2986
      height : aliased stdint_h.uint32_t;  -- vulkan_core.h:2987
      layers : aliased stdint_h.uint32_t;  -- vulkan_core.h:2988
   end record;
   pragma Convention (C_Pass_By_Copy, VkFramebufferCreateInfo);  -- vulkan_core.h:2979

   type VkSubpassDescription is record
      flags : aliased VkSubpassDescriptionFlags;  -- vulkan_core.h:2992
      pipelineBindPoint : aliased VkPipelineBindPoint;  -- vulkan_core.h:2993
      inputAttachmentCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:2994
      pInputAttachments : System.Address;  -- vulkan_core.h:2995
      colorAttachmentCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:2996
      pColorAttachments : System.Address;  -- vulkan_core.h:2997
      pResolveAttachments : System.Address;  -- vulkan_core.h:2998
      pDepthStencilAttachment : System.Address;  -- vulkan_core.h:2999
      preserveAttachmentCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:3000
      pPreserveAttachments : access stdint_h.uint32_t;  -- vulkan_core.h:3001
   end record;
   pragma Convention (C_Pass_By_Copy, VkSubpassDescription);  -- vulkan_core.h:2991

   type VkSubpassDependency is record
      srcSubpass : aliased stdint_h.uint32_t;  -- vulkan_core.h:3005
      dstSubpass : aliased stdint_h.uint32_t;  -- vulkan_core.h:3006
      srcStageMask : aliased VkPipelineStageFlags;  -- vulkan_core.h:3007
      dstStageMask : aliased VkPipelineStageFlags;  -- vulkan_core.h:3008
      srcAccessMask : aliased VkAccessFlags;  -- vulkan_core.h:3009
      dstAccessMask : aliased VkAccessFlags;  -- vulkan_core.h:3010
      dependencyFlags : aliased VkDependencyFlags;  -- vulkan_core.h:3011
   end record;
   pragma Convention (C_Pass_By_Copy, VkSubpassDependency);  -- vulkan_core.h:3004

   type VkRenderPassCreateInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:3015
      pNext : System.Address;  -- vulkan_core.h:3016
      flags : aliased VkRenderPassCreateFlags;  -- vulkan_core.h:3017
      attachmentCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:3018
      pAttachments : System.Address;  -- vulkan_core.h:3019
      subpassCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:3020
      pSubpasses : System.Address;  -- vulkan_core.h:3021
      dependencyCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:3022
      pDependencies : System.Address;  -- vulkan_core.h:3023
   end record;
   pragma Convention (C_Pass_By_Copy, VkRenderPassCreateInfo);  -- vulkan_core.h:3014

   type VkCommandPoolCreateInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:3027
      pNext : System.Address;  -- vulkan_core.h:3028
      flags : aliased VkCommandPoolCreateFlags;  -- vulkan_core.h:3029
      queueFamilyIndex : aliased stdint_h.uint32_t;  -- vulkan_core.h:3030
   end record;
   pragma Convention (C_Pass_By_Copy, VkCommandPoolCreateInfo);  -- vulkan_core.h:3026

   type VkCommandBufferAllocateInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:3034
      pNext : System.Address;  -- vulkan_core.h:3035
      commandPool : VkCommandPool;  -- vulkan_core.h:3036
      level : aliased VkCommandBufferLevel;  -- vulkan_core.h:3037
      commandBufferCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:3038
   end record;
   pragma Convention (C_Pass_By_Copy, VkCommandBufferAllocateInfo);  -- vulkan_core.h:3033

   type VkCommandBufferInheritanceInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:3042
      pNext : System.Address;  -- vulkan_core.h:3043
      renderPass : VkRenderPass;  -- vulkan_core.h:3044
      subpass : aliased stdint_h.uint32_t;  -- vulkan_core.h:3045
      framebuffer : VkFramebuffer;  -- vulkan_core.h:3046
      occlusionQueryEnable : aliased VkBool32;  -- vulkan_core.h:3047
      queryFlags : aliased VkQueryControlFlags;  -- vulkan_core.h:3048
      pipelineStatistics : aliased VkQueryPipelineStatisticFlags;  -- vulkan_core.h:3049
   end record;
   pragma Convention (C_Pass_By_Copy, VkCommandBufferInheritanceInfo);  -- vulkan_core.h:3041

   type VkCommandBufferBeginInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:3053
      pNext : System.Address;  -- vulkan_core.h:3054
      flags : aliased VkCommandBufferUsageFlags;  -- vulkan_core.h:3055
      pInheritanceInfo : System.Address;  -- vulkan_core.h:3056
   end record;
   pragma Convention (C_Pass_By_Copy, VkCommandBufferBeginInfo);  -- vulkan_core.h:3052

   type VkBufferCopy is record
      srcOffset : aliased VkDeviceSize;  -- vulkan_core.h:3060
      dstOffset : aliased VkDeviceSize;  -- vulkan_core.h:3061
      size : aliased VkDeviceSize;  -- vulkan_core.h:3062
   end record;
   pragma Convention (C_Pass_By_Copy, VkBufferCopy);  -- vulkan_core.h:3059

   type VkImageSubresourceLayers is record
      aspectMask : aliased VkImageAspectFlags;  -- vulkan_core.h:3066
      mipLevel : aliased stdint_h.uint32_t;  -- vulkan_core.h:3067
      baseArrayLayer : aliased stdint_h.uint32_t;  -- vulkan_core.h:3068
      layerCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:3069
   end record;
   pragma Convention (C_Pass_By_Copy, VkImageSubresourceLayers);  -- vulkan_core.h:3065

   type VkBufferImageCopy is record
      bufferOffset : aliased VkDeviceSize;  -- vulkan_core.h:3073
      bufferRowLength : aliased stdint_h.uint32_t;  -- vulkan_core.h:3074
      bufferImageHeight : aliased stdint_h.uint32_t;  -- vulkan_core.h:3075
      imageSubresource : aliased VkImageSubresourceLayers;  -- vulkan_core.h:3076
      imageOffset : aliased VkOffset3D;  -- vulkan_core.h:3077
      imageExtent : aliased VkExtent3D;  -- vulkan_core.h:3078
   end record;
   pragma Convention (C_Pass_By_Copy, VkBufferImageCopy);  -- vulkan_core.h:3072

   type VkClearColorValue_float32_array is array (0 .. 3) of aliased float;
   type VkClearColorValue_int32_array is array (0 .. 3) of aliased stdint_h.int32_t;
   type VkClearColorValue_uint32_array is array (0 .. 3) of aliased stdint_h.uint32_t;
   type VkClearColorValue (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            float32 : aliased VkClearColorValue_float32_array;  -- vulkan_core.h:3082
         when 1 =>
            int32 : aliased VkClearColorValue_int32_array;  -- vulkan_core.h:3083
         when others =>
            uint32 : aliased VkClearColorValue_uint32_array;  -- vulkan_core.h:3084
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, VkClearColorValue);
   pragma Unchecked_Union (VkClearColorValue);  -- vulkan_core.h:3081

   type VkClearDepthStencilValue is record
      depth : aliased float;  -- vulkan_core.h:3088
      stencil : aliased stdint_h.uint32_t;  -- vulkan_core.h:3089
   end record;
   pragma Convention (C_Pass_By_Copy, VkClearDepthStencilValue);  -- vulkan_core.h:3087

   type VkClearValue (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            color : VkClearColorValue;  -- vulkan_core.h:3093
         when others =>
            depthStencil : aliased VkClearDepthStencilValue;  -- vulkan_core.h:3094
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, VkClearValue);
   pragma Unchecked_Union (VkClearValue);  -- vulkan_core.h:3092

   type VkClearAttachment is record
      aspectMask : aliased VkImageAspectFlags;  -- vulkan_core.h:3098
      colorAttachment : aliased stdint_h.uint32_t;  -- vulkan_core.h:3099
      clearValue : VkClearValue;  -- vulkan_core.h:3100
   end record;
   pragma Convention (C_Pass_By_Copy, VkClearAttachment);  -- vulkan_core.h:3097

   type VkClearRect is record
      rect : aliased VkRect2D;  -- vulkan_core.h:3104
      baseArrayLayer : aliased stdint_h.uint32_t;  -- vulkan_core.h:3105
      layerCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:3106
   end record;
   pragma Convention (C_Pass_By_Copy, VkClearRect);  -- vulkan_core.h:3103

   type VkImageBlit_srcOffsets_array is array (0 .. 1) of aliased VkOffset3D;
   type VkImageBlit_dstOffsets_array is array (0 .. 1) of aliased VkOffset3D;
   type VkImageBlit is record
      srcSubresource : aliased VkImageSubresourceLayers;  -- vulkan_core.h:3110
      srcOffsets : aliased VkImageBlit_srcOffsets_array;  -- vulkan_core.h:3111
      dstSubresource : aliased VkImageSubresourceLayers;  -- vulkan_core.h:3112
      dstOffsets : aliased VkImageBlit_dstOffsets_array;  -- vulkan_core.h:3113
   end record;
   pragma Convention (C_Pass_By_Copy, VkImageBlit);  -- vulkan_core.h:3109

   type VkImageCopy is record
      srcSubresource : aliased VkImageSubresourceLayers;  -- vulkan_core.h:3117
      srcOffset : aliased VkOffset3D;  -- vulkan_core.h:3118
      dstSubresource : aliased VkImageSubresourceLayers;  -- vulkan_core.h:3119
      dstOffset : aliased VkOffset3D;  -- vulkan_core.h:3120
      extent : aliased VkExtent3D;  -- vulkan_core.h:3121
   end record;
   pragma Convention (C_Pass_By_Copy, VkImageCopy);  -- vulkan_core.h:3116

   type VkImageResolve is record
      srcSubresource : aliased VkImageSubresourceLayers;  -- vulkan_core.h:3125
      srcOffset : aliased VkOffset3D;  -- vulkan_core.h:3126
      dstSubresource : aliased VkImageSubresourceLayers;  -- vulkan_core.h:3127
      dstOffset : aliased VkOffset3D;  -- vulkan_core.h:3128
      extent : aliased VkExtent3D;  -- vulkan_core.h:3129
   end record;
   pragma Convention (C_Pass_By_Copy, VkImageResolve);  -- vulkan_core.h:3124

   type VkRenderPassBeginInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:3133
      pNext : System.Address;  -- vulkan_core.h:3134
      renderPass : VkRenderPass;  -- vulkan_core.h:3135
      framebuffer : VkFramebuffer;  -- vulkan_core.h:3136
      renderArea : aliased VkRect2D;  -- vulkan_core.h:3137
      clearValueCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:3138
      pClearValues : System.Address;  -- vulkan_core.h:3139
   end record;
   pragma Convention (C_Pass_By_Copy, VkRenderPassBeginInfo);  -- vulkan_core.h:3132

   type PFN_vkCreateInstance is access function
        (arg1 : System.Address;
         arg2 : System.Address;
         arg3 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateInstance);  -- vulkan_core.h:3142

   type PFN_vkDestroyInstance is access procedure (arg1 : VkInstance; arg2 : System.Address);
   pragma Convention (C, PFN_vkDestroyInstance);  -- vulkan_core.h:3143

   type PFN_vkEnumeratePhysicalDevices is access function
        (arg1 : VkInstance;
         arg2 : access stdint_h.uint32_t;
         arg3 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkEnumeratePhysicalDevices);  -- vulkan_core.h:3144

   type PFN_vkGetPhysicalDeviceFeatures is access procedure (arg1 : VkPhysicalDevice; arg2 : access VkPhysicalDeviceFeatures);
   pragma Convention (C, PFN_vkGetPhysicalDeviceFeatures);  -- vulkan_core.h:3145

   type PFN_vkGetPhysicalDeviceFormatProperties is access procedure
        (arg1 : VkPhysicalDevice;
         arg2 : VkFormat;
         arg3 : access VkFormatProperties);
   pragma Convention (C, PFN_vkGetPhysicalDeviceFormatProperties);  -- vulkan_core.h:3146

   type PFN_vkGetPhysicalDeviceImageFormatProperties is access function
        (arg1 : VkPhysicalDevice;
         arg2 : VkFormat;
         arg3 : VkImageType;
         arg4 : VkImageTiling;
         arg5 : VkImageUsageFlags;
         arg6 : VkImageCreateFlags;
         arg7 : access VkImageFormatProperties) return VkResult;
   pragma Convention (C, PFN_vkGetPhysicalDeviceImageFormatProperties);  -- vulkan_core.h:3147

   type PFN_vkGetPhysicalDeviceProperties is access procedure (arg1 : VkPhysicalDevice; arg2 : access VkPhysicalDeviceProperties);
   pragma Convention (C, PFN_vkGetPhysicalDeviceProperties);  -- vulkan_core.h:3148

   type PFN_vkGetPhysicalDeviceQueueFamilyProperties is access procedure
        (arg1 : VkPhysicalDevice;
         arg2 : access stdint_h.uint32_t;
         arg3 : access VkQueueFamilyProperties);
   pragma Convention (C, PFN_vkGetPhysicalDeviceQueueFamilyProperties);  -- vulkan_core.h:3149

   type PFN_vkGetPhysicalDeviceMemoryProperties is access procedure (arg1 : VkPhysicalDevice; arg2 : access VkPhysicalDeviceMemoryProperties);
   pragma Convention (C, PFN_vkGetPhysicalDeviceMemoryProperties);  -- vulkan_core.h:3150

   type PFN_vkGetInstanceProcAddr is access function (arg1 : VkInstance; arg2 : Interfaces.C.Strings.chars_ptr) return PFN_vkVoidFunction;
   pragma Convention (C, PFN_vkGetInstanceProcAddr);  -- vulkan_core.h:3151

   type PFN_vkGetDeviceProcAddr is access function (arg1 : VkDevice; arg2 : Interfaces.C.Strings.chars_ptr) return PFN_vkVoidFunction;
   pragma Convention (C, PFN_vkGetDeviceProcAddr);  -- vulkan_core.h:3152

   type PFN_vkCreateDevice is access function
        (arg1 : VkPhysicalDevice;
         arg2 : System.Address;
         arg3 : System.Address;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateDevice);  -- vulkan_core.h:3153

   type PFN_vkDestroyDevice is access procedure (arg1 : VkDevice; arg2 : System.Address);
   pragma Convention (C, PFN_vkDestroyDevice);  -- vulkan_core.h:3154

   type PFN_vkEnumerateInstanceExtensionProperties is access function
        (arg1 : Interfaces.C.Strings.chars_ptr;
         arg2 : access stdint_h.uint32_t;
         arg3 : access VkExtensionProperties) return VkResult;
   pragma Convention (C, PFN_vkEnumerateInstanceExtensionProperties);  -- vulkan_core.h:3155

   type PFN_vkEnumerateDeviceExtensionProperties is access function
        (arg1 : VkPhysicalDevice;
         arg2 : Interfaces.C.Strings.chars_ptr;
         arg3 : access stdint_h.uint32_t;
         arg4 : access VkExtensionProperties) return VkResult;
   pragma Convention (C, PFN_vkEnumerateDeviceExtensionProperties);  -- vulkan_core.h:3156

   type PFN_vkEnumerateInstanceLayerProperties is access function (arg1 : access stdint_h.uint32_t; arg2 : access VkLayerProperties) return VkResult;
   pragma Convention (C, PFN_vkEnumerateInstanceLayerProperties);  -- vulkan_core.h:3157

   type PFN_vkEnumerateDeviceLayerProperties is access function
        (arg1 : VkPhysicalDevice;
         arg2 : access stdint_h.uint32_t;
         arg3 : access VkLayerProperties) return VkResult;
   pragma Convention (C, PFN_vkEnumerateDeviceLayerProperties);  -- vulkan_core.h:3158

   type PFN_vkGetDeviceQueue is access procedure
        (arg1 : VkDevice;
         arg2 : stdint_h.uint32_t;
         arg3 : stdint_h.uint32_t;
         arg4 : System.Address);
   pragma Convention (C, PFN_vkGetDeviceQueue);  -- vulkan_core.h:3159

   type PFN_vkQueueSubmit is access function
        (arg1 : VkQueue;
         arg2 : stdint_h.uint32_t;
         arg3 : System.Address;
         arg4 : VkFence) return VkResult;
   pragma Convention (C, PFN_vkQueueSubmit);  -- vulkan_core.h:3160

   type PFN_vkQueueWaitIdle is access function (arg1 : VkQueue) return VkResult;
   pragma Convention (C, PFN_vkQueueWaitIdle);  -- vulkan_core.h:3161

   type PFN_vkDeviceWaitIdle is access function (arg1 : VkDevice) return VkResult;
   pragma Convention (C, PFN_vkDeviceWaitIdle);  -- vulkan_core.h:3162

   type PFN_vkAllocateMemory is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : System.Address;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkAllocateMemory);  -- vulkan_core.h:3163

   type PFN_vkFreeMemory is access procedure
        (arg1 : VkDevice;
         arg2 : VkDeviceMemory;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkFreeMemory);  -- vulkan_core.h:3164

   type PFN_vkMapMemory is access function
        (arg1 : VkDevice;
         arg2 : VkDeviceMemory;
         arg3 : VkDeviceSize;
         arg4 : VkDeviceSize;
         arg5 : VkMemoryMapFlags;
         arg6 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkMapMemory);  -- vulkan_core.h:3165

   type PFN_vkUnmapMemory is access procedure (arg1 : VkDevice; arg2 : VkDeviceMemory);
   pragma Convention (C, PFN_vkUnmapMemory);  -- vulkan_core.h:3166

   type PFN_vkFlushMappedMemoryRanges is access function
        (arg1 : VkDevice;
         arg2 : stdint_h.uint32_t;
         arg3 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkFlushMappedMemoryRanges);  -- vulkan_core.h:3167

   type PFN_vkInvalidateMappedMemoryRanges is access function
        (arg1 : VkDevice;
         arg2 : stdint_h.uint32_t;
         arg3 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkInvalidateMappedMemoryRanges);  -- vulkan_core.h:3168

   type PFN_vkGetDeviceMemoryCommitment is access procedure
        (arg1 : VkDevice;
         arg2 : VkDeviceMemory;
         arg3 : access VkDeviceSize);
   pragma Convention (C, PFN_vkGetDeviceMemoryCommitment);  -- vulkan_core.h:3169

   type PFN_vkBindBufferMemory is access function
        (arg1 : VkDevice;
         arg2 : VkBuffer;
         arg3 : VkDeviceMemory;
         arg4 : VkDeviceSize) return VkResult;
   pragma Convention (C, PFN_vkBindBufferMemory);  -- vulkan_core.h:3170

   type PFN_vkBindImageMemory is access function
        (arg1 : VkDevice;
         arg2 : VkImage;
         arg3 : VkDeviceMemory;
         arg4 : VkDeviceSize) return VkResult;
   pragma Convention (C, PFN_vkBindImageMemory);  -- vulkan_core.h:3171

   type PFN_vkGetBufferMemoryRequirements is access procedure
        (arg1 : VkDevice;
         arg2 : VkBuffer;
         arg3 : access VkMemoryRequirements);
   pragma Convention (C, PFN_vkGetBufferMemoryRequirements);  -- vulkan_core.h:3172

   type PFN_vkGetImageMemoryRequirements is access procedure
        (arg1 : VkDevice;
         arg2 : VkImage;
         arg3 : access VkMemoryRequirements);
   pragma Convention (C, PFN_vkGetImageMemoryRequirements);  -- vulkan_core.h:3173

   type PFN_vkGetImageSparseMemoryRequirements is access procedure
        (arg1 : VkDevice;
         arg2 : VkImage;
         arg3 : access stdint_h.uint32_t;
         arg4 : access VkSparseImageMemoryRequirements);
   pragma Convention (C, PFN_vkGetImageSparseMemoryRequirements);  -- vulkan_core.h:3174

   type PFN_vkGetPhysicalDeviceSparseImageFormatProperties is access procedure
        (arg1 : VkPhysicalDevice;
         arg2 : VkFormat;
         arg3 : VkImageType;
         arg4 : VkSampleCountFlagBits;
         arg5 : VkImageUsageFlags;
         arg6 : VkImageTiling;
         arg7 : access stdint_h.uint32_t;
         arg8 : access VkSparseImageFormatProperties);
   pragma Convention (C, PFN_vkGetPhysicalDeviceSparseImageFormatProperties);  -- vulkan_core.h:3175

   type PFN_vkQueueBindSparse is access function
        (arg1 : VkQueue;
         arg2 : stdint_h.uint32_t;
         arg3 : System.Address;
         arg4 : VkFence) return VkResult;
   pragma Convention (C, PFN_vkQueueBindSparse);  -- vulkan_core.h:3176

   type PFN_vkCreateFence is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : System.Address;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateFence);  -- vulkan_core.h:3177

   type PFN_vkDestroyFence is access procedure
        (arg1 : VkDevice;
         arg2 : VkFence;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkDestroyFence);  -- vulkan_core.h:3178

   type PFN_vkResetFences is access function
        (arg1 : VkDevice;
         arg2 : stdint_h.uint32_t;
         arg3 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkResetFences);  -- vulkan_core.h:3179

   type PFN_vkGetFenceStatus is access function (arg1 : VkDevice; arg2 : VkFence) return VkResult;
   pragma Convention (C, PFN_vkGetFenceStatus);  -- vulkan_core.h:3180

   type PFN_vkWaitForFences is access function
        (arg1 : VkDevice;
         arg2 : stdint_h.uint32_t;
         arg3 : System.Address;
         arg4 : VkBool32;
         arg5 : stdint_h.uint64_t) return VkResult;
   pragma Convention (C, PFN_vkWaitForFences);  -- vulkan_core.h:3181

   type PFN_vkCreateSemaphore is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : System.Address;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateSemaphore);  -- vulkan_core.h:3182

   type PFN_vkDestroySemaphore is access procedure
        (arg1 : VkDevice;
         arg2 : VkSemaphore;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkDestroySemaphore);  -- vulkan_core.h:3183

   type PFN_vkCreateEvent is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : System.Address;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateEvent);  -- vulkan_core.h:3184

   type PFN_vkDestroyEvent is access procedure
        (arg1 : VkDevice;
         arg2 : VkEvent;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkDestroyEvent);  -- vulkan_core.h:3185

   type PFN_vkGetEventStatus is access function (arg1 : VkDevice; arg2 : VkEvent) return VkResult;
   pragma Convention (C, PFN_vkGetEventStatus);  -- vulkan_core.h:3186

   type PFN_vkSetEvent is access function (arg1 : VkDevice; arg2 : VkEvent) return VkResult;
   pragma Convention (C, PFN_vkSetEvent);  -- vulkan_core.h:3187

   type PFN_vkResetEvent is access function (arg1 : VkDevice; arg2 : VkEvent) return VkResult;
   pragma Convention (C, PFN_vkResetEvent);  -- vulkan_core.h:3188

   type PFN_vkCreateQueryPool is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : System.Address;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateQueryPool);  -- vulkan_core.h:3189

   type PFN_vkDestroyQueryPool is access procedure
        (arg1 : VkDevice;
         arg2 : VkQueryPool;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkDestroyQueryPool);  -- vulkan_core.h:3190

   type PFN_vkGetQueryPoolResults is access function
        (arg1 : VkDevice;
         arg2 : VkQueryPool;
         arg3 : stdint_h.uint32_t;
         arg4 : stdint_h.uint32_t;
         arg5 : crtdefs_h.size_t;
         arg6 : System.Address;
         arg7 : VkDeviceSize;
         arg8 : VkQueryResultFlags) return VkResult;
   pragma Convention (C, PFN_vkGetQueryPoolResults);  -- vulkan_core.h:3191

   type PFN_vkCreateBuffer is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : System.Address;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateBuffer);  -- vulkan_core.h:3192

   type PFN_vkDestroyBuffer is access procedure
        (arg1 : VkDevice;
         arg2 : VkBuffer;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkDestroyBuffer);  -- vulkan_core.h:3193

   type PFN_vkCreateBufferView is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : System.Address;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateBufferView);  -- vulkan_core.h:3194

   type PFN_vkDestroyBufferView is access procedure
        (arg1 : VkDevice;
         arg2 : VkBufferView;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkDestroyBufferView);  -- vulkan_core.h:3195

   type PFN_vkCreateImage is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : System.Address;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateImage);  -- vulkan_core.h:3196

   type PFN_vkDestroyImage is access procedure
        (arg1 : VkDevice;
         arg2 : VkImage;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkDestroyImage);  -- vulkan_core.h:3197

   type PFN_vkGetImageSubresourceLayout is access procedure
        (arg1 : VkDevice;
         arg2 : VkImage;
         arg3 : System.Address;
         arg4 : access VkSubresourceLayout);
   pragma Convention (C, PFN_vkGetImageSubresourceLayout);  -- vulkan_core.h:3198

   type PFN_vkCreateImageView is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : System.Address;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateImageView);  -- vulkan_core.h:3199

   type PFN_vkDestroyImageView is access procedure
        (arg1 : VkDevice;
         arg2 : VkImageView;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkDestroyImageView);  -- vulkan_core.h:3200

   type PFN_vkCreateShaderModule is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : System.Address;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateShaderModule);  -- vulkan_core.h:3201

   type PFN_vkDestroyShaderModule is access procedure
        (arg1 : VkDevice;
         arg2 : VkShaderModule;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkDestroyShaderModule);  -- vulkan_core.h:3202

   type PFN_vkCreatePipelineCache is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : System.Address;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreatePipelineCache);  -- vulkan_core.h:3203

   type PFN_vkDestroyPipelineCache is access procedure
        (arg1 : VkDevice;
         arg2 : VkPipelineCache;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkDestroyPipelineCache);  -- vulkan_core.h:3204

   type PFN_vkGetPipelineCacheData is access function
        (arg1 : VkDevice;
         arg2 : VkPipelineCache;
         arg3 : access crtdefs_h.size_t;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkGetPipelineCacheData);  -- vulkan_core.h:3205

   type PFN_vkMergePipelineCaches is access function
        (arg1 : VkDevice;
         arg2 : VkPipelineCache;
         arg3 : stdint_h.uint32_t;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkMergePipelineCaches);  -- vulkan_core.h:3206

   type PFN_vkCreateGraphicsPipelines is access function
        (arg1 : VkDevice;
         arg2 : VkPipelineCache;
         arg3 : stdint_h.uint32_t;
         arg4 : System.Address;
         arg5 : System.Address;
         arg6 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateGraphicsPipelines);  -- vulkan_core.h:3207

   type PFN_vkCreateComputePipelines is access function
        (arg1 : VkDevice;
         arg2 : VkPipelineCache;
         arg3 : stdint_h.uint32_t;
         arg4 : System.Address;
         arg5 : System.Address;
         arg6 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateComputePipelines);  -- vulkan_core.h:3208

   type PFN_vkDestroyPipeline is access procedure
        (arg1 : VkDevice;
         arg2 : VkPipeline;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkDestroyPipeline);  -- vulkan_core.h:3209

   type PFN_vkCreatePipelineLayout is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : System.Address;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreatePipelineLayout);  -- vulkan_core.h:3210

   type PFN_vkDestroyPipelineLayout is access procedure
        (arg1 : VkDevice;
         arg2 : VkPipelineLayout;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkDestroyPipelineLayout);  -- vulkan_core.h:3211

   type PFN_vkCreateSampler is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : System.Address;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateSampler);  -- vulkan_core.h:3212

   type PFN_vkDestroySampler is access procedure
        (arg1 : VkDevice;
         arg2 : VkSampler;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkDestroySampler);  -- vulkan_core.h:3213

   type PFN_vkCreateDescriptorSetLayout is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : System.Address;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateDescriptorSetLayout);  -- vulkan_core.h:3214

   type PFN_vkDestroyDescriptorSetLayout is access procedure
        (arg1 : VkDevice;
         arg2 : VkDescriptorSetLayout;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkDestroyDescriptorSetLayout);  -- vulkan_core.h:3215

   type PFN_vkCreateDescriptorPool is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : System.Address;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateDescriptorPool);  -- vulkan_core.h:3216

   type PFN_vkDestroyDescriptorPool is access procedure
        (arg1 : VkDevice;
         arg2 : VkDescriptorPool;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkDestroyDescriptorPool);  -- vulkan_core.h:3217

   type PFN_vkResetDescriptorPool is access function
        (arg1 : VkDevice;
         arg2 : VkDescriptorPool;
         arg3 : VkDescriptorPoolResetFlags) return VkResult;
   pragma Convention (C, PFN_vkResetDescriptorPool);  -- vulkan_core.h:3218

   type PFN_vkAllocateDescriptorSets is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkAllocateDescriptorSets);  -- vulkan_core.h:3219

   type PFN_vkFreeDescriptorSets is access function
        (arg1 : VkDevice;
         arg2 : VkDescriptorPool;
         arg3 : stdint_h.uint32_t;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkFreeDescriptorSets);  -- vulkan_core.h:3220

   type PFN_vkUpdateDescriptorSets is access procedure
        (arg1 : VkDevice;
         arg2 : stdint_h.uint32_t;
         arg3 : System.Address;
         arg4 : stdint_h.uint32_t;
         arg5 : System.Address);
   pragma Convention (C, PFN_vkUpdateDescriptorSets);  -- vulkan_core.h:3221

   type PFN_vkCreateFramebuffer is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : System.Address;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateFramebuffer);  -- vulkan_core.h:3222

   type PFN_vkDestroyFramebuffer is access procedure
        (arg1 : VkDevice;
         arg2 : VkFramebuffer;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkDestroyFramebuffer);  -- vulkan_core.h:3223

   type PFN_vkCreateRenderPass is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : System.Address;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateRenderPass);  -- vulkan_core.h:3224

   type PFN_vkDestroyRenderPass is access procedure
        (arg1 : VkDevice;
         arg2 : VkRenderPass;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkDestroyRenderPass);  -- vulkan_core.h:3225

   type PFN_vkGetRenderAreaGranularity is access procedure
        (arg1 : VkDevice;
         arg2 : VkRenderPass;
         arg3 : access VkExtent2D);
   pragma Convention (C, PFN_vkGetRenderAreaGranularity);  -- vulkan_core.h:3226

   type PFN_vkCreateCommandPool is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : System.Address;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateCommandPool);  -- vulkan_core.h:3227

   type PFN_vkDestroyCommandPool is access procedure
        (arg1 : VkDevice;
         arg2 : VkCommandPool;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkDestroyCommandPool);  -- vulkan_core.h:3228

   type PFN_vkResetCommandPool is access function
        (arg1 : VkDevice;
         arg2 : VkCommandPool;
         arg3 : VkCommandPoolResetFlags) return VkResult;
   pragma Convention (C, PFN_vkResetCommandPool);  -- vulkan_core.h:3229

   type PFN_vkAllocateCommandBuffers is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkAllocateCommandBuffers);  -- vulkan_core.h:3230

   type PFN_vkFreeCommandBuffers is access procedure
        (arg1 : VkDevice;
         arg2 : VkCommandPool;
         arg3 : stdint_h.uint32_t;
         arg4 : System.Address);
   pragma Convention (C, PFN_vkFreeCommandBuffers);  -- vulkan_core.h:3231

   type PFN_vkBeginCommandBuffer is access function (arg1 : VkCommandBuffer; arg2 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkBeginCommandBuffer);  -- vulkan_core.h:3232

   type PFN_vkEndCommandBuffer is access function (arg1 : VkCommandBuffer) return VkResult;
   pragma Convention (C, PFN_vkEndCommandBuffer);  -- vulkan_core.h:3233

   type PFN_vkResetCommandBuffer is access function (arg1 : VkCommandBuffer; arg2 : VkCommandBufferResetFlags) return VkResult;
   pragma Convention (C, PFN_vkResetCommandBuffer);  -- vulkan_core.h:3234

   type PFN_vkCmdBindPipeline is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkPipelineBindPoint;
         arg3 : VkPipeline);
   pragma Convention (C, PFN_vkCmdBindPipeline);  -- vulkan_core.h:3235

   type PFN_vkCmdSetViewport is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : stdint_h.uint32_t;
         arg3 : stdint_h.uint32_t;
         arg4 : System.Address);
   pragma Convention (C, PFN_vkCmdSetViewport);  -- vulkan_core.h:3236

   type PFN_vkCmdSetScissor is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : stdint_h.uint32_t;
         arg3 : stdint_h.uint32_t;
         arg4 : System.Address);
   pragma Convention (C, PFN_vkCmdSetScissor);  -- vulkan_core.h:3237

   type PFN_vkCmdSetLineWidth is access procedure (arg1 : VkCommandBuffer; arg2 : float);
   pragma Convention (C, PFN_vkCmdSetLineWidth);  -- vulkan_core.h:3238

   type PFN_vkCmdSetDepthBias is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : float;
         arg3 : float;
         arg4 : float);
   pragma Convention (C, PFN_vkCmdSetDepthBias);  -- vulkan_core.h:3239

   type PFN_vkCmdSetBlendConstants is access procedure (arg1 : VkCommandBuffer; arg2 : access float);
   pragma Convention (C, PFN_vkCmdSetBlendConstants);  -- vulkan_core.h:3240

   type PFN_vkCmdSetDepthBounds is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : float;
         arg3 : float);
   pragma Convention (C, PFN_vkCmdSetDepthBounds);  -- vulkan_core.h:3241

   type PFN_vkCmdSetStencilCompareMask is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkStencilFaceFlags;
         arg3 : stdint_h.uint32_t);
   pragma Convention (C, PFN_vkCmdSetStencilCompareMask);  -- vulkan_core.h:3242

   type PFN_vkCmdSetStencilWriteMask is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkStencilFaceFlags;
         arg3 : stdint_h.uint32_t);
   pragma Convention (C, PFN_vkCmdSetStencilWriteMask);  -- vulkan_core.h:3243

   type PFN_vkCmdSetStencilReference is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkStencilFaceFlags;
         arg3 : stdint_h.uint32_t);
   pragma Convention (C, PFN_vkCmdSetStencilReference);  -- vulkan_core.h:3244

   type PFN_vkCmdBindDescriptorSets is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkPipelineBindPoint;
         arg3 : VkPipelineLayout;
         arg4 : stdint_h.uint32_t;
         arg5 : stdint_h.uint32_t;
         arg6 : System.Address;
         arg7 : stdint_h.uint32_t;
         arg8 : access stdint_h.uint32_t);
   pragma Convention (C, PFN_vkCmdBindDescriptorSets);  -- vulkan_core.h:3245

   type PFN_vkCmdBindIndexBuffer is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkBuffer;
         arg3 : VkDeviceSize;
         arg4 : VkIndexType);
   pragma Convention (C, PFN_vkCmdBindIndexBuffer);  -- vulkan_core.h:3246

   type PFN_vkCmdBindVertexBuffers is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : stdint_h.uint32_t;
         arg3 : stdint_h.uint32_t;
         arg4 : System.Address;
         arg5 : access VkDeviceSize);
   pragma Convention (C, PFN_vkCmdBindVertexBuffers);  -- vulkan_core.h:3247

   type PFN_vkCmdDraw is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : stdint_h.uint32_t;
         arg3 : stdint_h.uint32_t;
         arg4 : stdint_h.uint32_t;
         arg5 : stdint_h.uint32_t);
   pragma Convention (C, PFN_vkCmdDraw);  -- vulkan_core.h:3248

   type PFN_vkCmdDrawIndexed is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : stdint_h.uint32_t;
         arg3 : stdint_h.uint32_t;
         arg4 : stdint_h.uint32_t;
         arg5 : stdint_h.int32_t;
         arg6 : stdint_h.uint32_t);
   pragma Convention (C, PFN_vkCmdDrawIndexed);  -- vulkan_core.h:3249

   type PFN_vkCmdDrawIndirect is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkBuffer;
         arg3 : VkDeviceSize;
         arg4 : stdint_h.uint32_t;
         arg5 : stdint_h.uint32_t);
   pragma Convention (C, PFN_vkCmdDrawIndirect);  -- vulkan_core.h:3250

   type PFN_vkCmdDrawIndexedIndirect is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkBuffer;
         arg3 : VkDeviceSize;
         arg4 : stdint_h.uint32_t;
         arg5 : stdint_h.uint32_t);
   pragma Convention (C, PFN_vkCmdDrawIndexedIndirect);  -- vulkan_core.h:3251

   type PFN_vkCmdDispatch is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : stdint_h.uint32_t;
         arg3 : stdint_h.uint32_t;
         arg4 : stdint_h.uint32_t);
   pragma Convention (C, PFN_vkCmdDispatch);  -- vulkan_core.h:3252

   type PFN_vkCmdDispatchIndirect is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkBuffer;
         arg3 : VkDeviceSize);
   pragma Convention (C, PFN_vkCmdDispatchIndirect);  -- vulkan_core.h:3253

   type PFN_vkCmdCopyBuffer is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkBuffer;
         arg3 : VkBuffer;
         arg4 : stdint_h.uint32_t;
         arg5 : System.Address);
   pragma Convention (C, PFN_vkCmdCopyBuffer);  -- vulkan_core.h:3254

   type PFN_vkCmdCopyImage is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkImage;
         arg3 : VkImageLayout;
         arg4 : VkImage;
         arg5 : VkImageLayout;
         arg6 : stdint_h.uint32_t;
         arg7 : System.Address);
   pragma Convention (C, PFN_vkCmdCopyImage);  -- vulkan_core.h:3255

   type PFN_vkCmdBlitImage is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkImage;
         arg3 : VkImageLayout;
         arg4 : VkImage;
         arg5 : VkImageLayout;
         arg6 : stdint_h.uint32_t;
         arg7 : System.Address;
         arg8 : VkFilter);
   pragma Convention (C, PFN_vkCmdBlitImage);  -- vulkan_core.h:3256

   type PFN_vkCmdCopyBufferToImage is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkBuffer;
         arg3 : VkImage;
         arg4 : VkImageLayout;
         arg5 : stdint_h.uint32_t;
         arg6 : System.Address);
   pragma Convention (C, PFN_vkCmdCopyBufferToImage);  -- vulkan_core.h:3257

   type PFN_vkCmdCopyImageToBuffer is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkImage;
         arg3 : VkImageLayout;
         arg4 : VkBuffer;
         arg5 : stdint_h.uint32_t;
         arg6 : System.Address);
   pragma Convention (C, PFN_vkCmdCopyImageToBuffer);  -- vulkan_core.h:3258

   type PFN_vkCmdUpdateBuffer is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkBuffer;
         arg3 : VkDeviceSize;
         arg4 : VkDeviceSize;
         arg5 : System.Address);
   pragma Convention (C, PFN_vkCmdUpdateBuffer);  -- vulkan_core.h:3259

   type PFN_vkCmdFillBuffer is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkBuffer;
         arg3 : VkDeviceSize;
         arg4 : VkDeviceSize;
         arg5 : stdint_h.uint32_t);
   pragma Convention (C, PFN_vkCmdFillBuffer);  -- vulkan_core.h:3260

   type PFN_vkCmdClearColorImage is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkImage;
         arg3 : VkImageLayout;
         arg4 : System.Address;
         arg5 : stdint_h.uint32_t;
         arg6 : System.Address);
   pragma Convention (C, PFN_vkCmdClearColorImage);  -- vulkan_core.h:3261

   type PFN_vkCmdClearDepthStencilImage is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkImage;
         arg3 : VkImageLayout;
         arg4 : System.Address;
         arg5 : stdint_h.uint32_t;
         arg6 : System.Address);
   pragma Convention (C, PFN_vkCmdClearDepthStencilImage);  -- vulkan_core.h:3262

   type PFN_vkCmdClearAttachments is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : stdint_h.uint32_t;
         arg3 : System.Address;
         arg4 : stdint_h.uint32_t;
         arg5 : System.Address);
   pragma Convention (C, PFN_vkCmdClearAttachments);  -- vulkan_core.h:3263

   type PFN_vkCmdResolveImage is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkImage;
         arg3 : VkImageLayout;
         arg4 : VkImage;
         arg5 : VkImageLayout;
         arg6 : stdint_h.uint32_t;
         arg7 : System.Address);
   pragma Convention (C, PFN_vkCmdResolveImage);  -- vulkan_core.h:3264

   type PFN_vkCmdSetEvent is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkEvent;
         arg3 : VkPipelineStageFlags);
   pragma Convention (C, PFN_vkCmdSetEvent);  -- vulkan_core.h:3265

   type PFN_vkCmdResetEvent is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkEvent;
         arg3 : VkPipelineStageFlags);
   pragma Convention (C, PFN_vkCmdResetEvent);  -- vulkan_core.h:3266

   type PFN_vkCmdWaitEvents is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : stdint_h.uint32_t;
         arg3 : System.Address;
         arg4 : VkPipelineStageFlags;
         arg5 : VkPipelineStageFlags;
         arg6 : stdint_h.uint32_t;
         arg7 : System.Address;
         arg8 : stdint_h.uint32_t;
         arg9 : System.Address;
         arg10 : stdint_h.uint32_t;
         arg11 : System.Address);
   pragma Convention (C, PFN_vkCmdWaitEvents);  -- vulkan_core.h:3267

   type PFN_vkCmdPipelineBarrier is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkPipelineStageFlags;
         arg3 : VkPipelineStageFlags;
         arg4 : VkDependencyFlags;
         arg5 : stdint_h.uint32_t;
         arg6 : System.Address;
         arg7 : stdint_h.uint32_t;
         arg8 : System.Address;
         arg9 : stdint_h.uint32_t;
         arg10 : System.Address);
   pragma Convention (C, PFN_vkCmdPipelineBarrier);  -- vulkan_core.h:3268

   type PFN_vkCmdBeginQuery is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkQueryPool;
         arg3 : stdint_h.uint32_t;
         arg4 : VkQueryControlFlags);
   pragma Convention (C, PFN_vkCmdBeginQuery);  -- vulkan_core.h:3269

   type PFN_vkCmdEndQuery is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkQueryPool;
         arg3 : stdint_h.uint32_t);
   pragma Convention (C, PFN_vkCmdEndQuery);  -- vulkan_core.h:3270

   type PFN_vkCmdResetQueryPool is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkQueryPool;
         arg3 : stdint_h.uint32_t;
         arg4 : stdint_h.uint32_t);
   pragma Convention (C, PFN_vkCmdResetQueryPool);  -- vulkan_core.h:3271

   type PFN_vkCmdWriteTimestamp is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkPipelineStageFlagBits;
         arg3 : VkQueryPool;
         arg4 : stdint_h.uint32_t);
   pragma Convention (C, PFN_vkCmdWriteTimestamp);  -- vulkan_core.h:3272

   type PFN_vkCmdCopyQueryPoolResults is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkQueryPool;
         arg3 : stdint_h.uint32_t;
         arg4 : stdint_h.uint32_t;
         arg5 : VkBuffer;
         arg6 : VkDeviceSize;
         arg7 : VkDeviceSize;
         arg8 : VkQueryResultFlags);
   pragma Convention (C, PFN_vkCmdCopyQueryPoolResults);  -- vulkan_core.h:3273

   type PFN_vkCmdPushConstants is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkPipelineLayout;
         arg3 : VkShaderStageFlags;
         arg4 : stdint_h.uint32_t;
         arg5 : stdint_h.uint32_t;
         arg6 : System.Address);
   pragma Convention (C, PFN_vkCmdPushConstants);  -- vulkan_core.h:3274

   type PFN_vkCmdBeginRenderPass is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : System.Address;
         arg3 : VkSubpassContents);
   pragma Convention (C, PFN_vkCmdBeginRenderPass);  -- vulkan_core.h:3275

   type PFN_vkCmdNextSubpass is access procedure (arg1 : VkCommandBuffer; arg2 : VkSubpassContents);
   pragma Convention (C, PFN_vkCmdNextSubpass);  -- vulkan_core.h:3276

   type PFN_vkCmdEndRenderPass is access procedure (arg1 : VkCommandBuffer);
   pragma Convention (C, PFN_vkCmdEndRenderPass);  -- vulkan_core.h:3277

   type PFN_vkCmdExecuteCommands is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : stdint_h.uint32_t;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkCmdExecuteCommands);  -- vulkan_core.h:3278

   function vkCreateInstance
     (pCreateInfo : System.Address;
      pAllocator : System.Address := System.Null_Address;
      pInstance : System.Address) return VkResult;  -- vulkan_core.h:3281
   pragma Import (C, vkCreateInstance, "vkCreateInstance");

   procedure vkDestroyInstance (instance : VkInstance; pAllocator : System.Address := System.Null_Address);  -- vulkan_core.h:3286
   pragma Import (C, vkDestroyInstance, "vkDestroyInstance");

   function vkEnumeratePhysicalDevices
     (instance : VkInstance;
      pPhysicalDeviceCount : access stdint_h.uint32_t;
      pPhysicalDevices : System.Address) return VkResult;  -- vulkan_core.h:3290
   pragma Import (C, vkEnumeratePhysicalDevices, "vkEnumeratePhysicalDevices");

   procedure vkGetPhysicalDeviceFeatures (physicalDevice : VkPhysicalDevice; pFeatures : access VkPhysicalDeviceFeatures);  -- vulkan_core.h:3295
   pragma Import (C, vkGetPhysicalDeviceFeatures, "vkGetPhysicalDeviceFeatures");

   procedure vkGetPhysicalDeviceFormatProperties
     (physicalDevice : VkPhysicalDevice;
      format : VkFormat;
      pFormatProperties : access VkFormatProperties);  -- vulkan_core.h:3299
   pragma Import (C, vkGetPhysicalDeviceFormatProperties, "vkGetPhysicalDeviceFormatProperties");

   function vkGetPhysicalDeviceImageFormatProperties
     (physicalDevice : VkPhysicalDevice;
      format : VkFormat;
      c_type : VkImageType;
      tiling : VkImageTiling;
      usage : VkImageUsageFlags;
      flags : VkImageCreateFlags;
      pImageFormatProperties : access VkImageFormatProperties) return VkResult;  -- vulkan_core.h:3304
   pragma Import (C, vkGetPhysicalDeviceImageFormatProperties, "vkGetPhysicalDeviceImageFormatProperties");

   procedure vkGetPhysicalDeviceProperties (physicalDevice : VkPhysicalDevice; pProperties : access VkPhysicalDeviceProperties);  -- vulkan_core.h:3313
   pragma Import (C, vkGetPhysicalDeviceProperties, "vkGetPhysicalDeviceProperties");

   procedure vkGetPhysicalDeviceQueueFamilyProperties
     (physicalDevice : VkPhysicalDevice;
      pQueueFamilyPropertyCount : access stdint_h.uint32_t;
      pQueueFamilyProperties : access VkQueueFamilyProperties);  -- vulkan_core.h:3317
   pragma Import (C, vkGetPhysicalDeviceQueueFamilyProperties, "vkGetPhysicalDeviceQueueFamilyProperties");

   procedure vkGetPhysicalDeviceMemoryProperties (physicalDevice : VkPhysicalDevice; pMemoryProperties : access VkPhysicalDeviceMemoryProperties);  -- vulkan_core.h:3322
   pragma Import (C, vkGetPhysicalDeviceMemoryProperties, "vkGetPhysicalDeviceMemoryProperties");

   function vkGetInstanceProcAddr (instance : VkInstance; pName : Interfaces.C.Strings.chars_ptr) return PFN_vkVoidFunction;  -- vulkan_core.h:3326
   pragma Import (C, vkGetInstanceProcAddr, "vkGetInstanceProcAddr");

   function vkGetDeviceProcAddr (device : VkDevice; pName : Interfaces.C.Strings.chars_ptr) return PFN_vkVoidFunction;  -- vulkan_core.h:3330
   pragma Import (C, vkGetDeviceProcAddr, "vkGetDeviceProcAddr");

   function vkCreateDevice
     (physicalDevice : VkPhysicalDevice;
      pCreateInfo : System.Address;
      pAllocator : System.Address;
      pDevice : System.Address) return VkResult;  -- vulkan_core.h:3334
   pragma Import (C, vkCreateDevice, "vkCreateDevice");

   procedure vkDestroyDevice (device : VkDevice; pAllocator : System.Address);  -- vulkan_core.h:3340
   pragma Import (C, vkDestroyDevice, "vkDestroyDevice");

   function vkEnumerateInstanceExtensionProperties
     (pLayerName : Interfaces.C.Strings.chars_ptr;
      pPropertyCount : access stdint_h.uint32_t;
      pProperties : access VkExtensionProperties) return VkResult;  -- vulkan_core.h:3344
   pragma Import (C, vkEnumerateInstanceExtensionProperties, "vkEnumerateInstanceExtensionProperties");

   function vkEnumerateDeviceExtensionProperties
     (physicalDevice : VkPhysicalDevice;
      pLayerName : Interfaces.C.Strings.chars_ptr;
      pPropertyCount : access stdint_h.uint32_t;
      pProperties : access VkExtensionProperties) return VkResult;  -- vulkan_core.h:3349
   pragma Import (C, vkEnumerateDeviceExtensionProperties, "vkEnumerateDeviceExtensionProperties");

   function vkEnumerateInstanceLayerProperties (pPropertyCount : access stdint_h.uint32_t; pProperties : access VkLayerProperties) return VkResult;  -- vulkan_core.h:3355
   pragma Import (C, vkEnumerateInstanceLayerProperties, "vkEnumerateInstanceLayerProperties");

   function vkEnumerateDeviceLayerProperties
     (physicalDevice : VkPhysicalDevice;
      pPropertyCount : access stdint_h.uint32_t;
      pProperties : access VkLayerProperties) return VkResult;  -- vulkan_core.h:3359
   pragma Import (C, vkEnumerateDeviceLayerProperties, "vkEnumerateDeviceLayerProperties");

   procedure vkGetDeviceQueue
     (device : VkDevice;
      queueFamilyIndex : stdint_h.uint32_t;
      queueIndex : stdint_h.uint32_t;
      pQueue : System.Address);  -- vulkan_core.h:3364
   pragma Import (C, vkGetDeviceQueue, "vkGetDeviceQueue");

   function vkQueueSubmit
     (queue : VkQueue;
      submitCount : stdint_h.uint32_t;
      pSubmits : System.Address;
      fence : VkFence) return VkResult;  -- vulkan_core.h:3370
   pragma Import (C, vkQueueSubmit, "vkQueueSubmit");

   function vkQueueWaitIdle (queue : VkQueue) return VkResult;  -- vulkan_core.h:3376
   pragma Import (C, vkQueueWaitIdle, "vkQueueWaitIdle");

   function vkDeviceWaitIdle (device : VkDevice) return VkResult;  -- vulkan_core.h:3379
   pragma Import (C, vkDeviceWaitIdle, "vkDeviceWaitIdle");

   function vkAllocateMemory
     (device : VkDevice;
      pAllocateInfo : System.Address;
      pAllocator : System.Address;
      pMemory : System.Address) return VkResult;  -- vulkan_core.h:3382
   pragma Import (C, vkAllocateMemory, "vkAllocateMemory");

   procedure vkFreeMemory
     (device : VkDevice;
      memory : VkDeviceMemory;
      pAllocator : System.Address);  -- vulkan_core.h:3388
   pragma Import (C, vkFreeMemory, "vkFreeMemory");

   function vkMapMemory
     (device : VkDevice;
      memory : VkDeviceMemory;
      offset : VkDeviceSize;
      size : VkDeviceSize;
      flags : VkMemoryMapFlags;
      ppData : System.Address) return VkResult;  -- vulkan_core.h:3393
   pragma Import (C, vkMapMemory, "vkMapMemory");

   procedure vkUnmapMemory (device : VkDevice; memory : VkDeviceMemory);  -- vulkan_core.h:3401
   pragma Import (C, vkUnmapMemory, "vkUnmapMemory");

   function vkFlushMappedMemoryRanges
     (device : VkDevice;
      memoryRangeCount : stdint_h.uint32_t;
      pMemoryRanges : System.Address) return VkResult;  -- vulkan_core.h:3405
   pragma Import (C, vkFlushMappedMemoryRanges, "vkFlushMappedMemoryRanges");

   function vkInvalidateMappedMemoryRanges
     (device : VkDevice;
      memoryRangeCount : stdint_h.uint32_t;
      pMemoryRanges : System.Address) return VkResult;  -- vulkan_core.h:3410
   pragma Import (C, vkInvalidateMappedMemoryRanges, "vkInvalidateMappedMemoryRanges");

   procedure vkGetDeviceMemoryCommitment
     (device : VkDevice;
      memory : VkDeviceMemory;
      pCommittedMemoryInBytes : access VkDeviceSize);  -- vulkan_core.h:3415
   pragma Import (C, vkGetDeviceMemoryCommitment, "vkGetDeviceMemoryCommitment");

   function vkBindBufferMemory
     (device : VkDevice;
      buffer : VkBuffer;
      memory : VkDeviceMemory;
      memoryOffset : VkDeviceSize) return VkResult;  -- vulkan_core.h:3420
   pragma Import (C, vkBindBufferMemory, "vkBindBufferMemory");

   function vkBindImageMemory
     (device : VkDevice;
      image : VkImage;
      memory : VkDeviceMemory;
      memoryOffset : VkDeviceSize) return VkResult;  -- vulkan_core.h:3426
   pragma Import (C, vkBindImageMemory, "vkBindImageMemory");

   procedure vkGetBufferMemoryRequirements
     (device : VkDevice;
      buffer : VkBuffer;
      pMemoryRequirements : access VkMemoryRequirements);  -- vulkan_core.h:3432
   pragma Import (C, vkGetBufferMemoryRequirements, "vkGetBufferMemoryRequirements");

   procedure vkGetImageMemoryRequirements
     (device : VkDevice;
      image : VkImage;
      pMemoryRequirements : access VkMemoryRequirements);  -- vulkan_core.h:3437
   pragma Import (C, vkGetImageMemoryRequirements, "vkGetImageMemoryRequirements");

   procedure vkGetImageSparseMemoryRequirements
     (device : VkDevice;
      image : VkImage;
      pSparseMemoryRequirementCount : access stdint_h.uint32_t;
      pSparseMemoryRequirements : access VkSparseImageMemoryRequirements);  -- vulkan_core.h:3442
   pragma Import (C, vkGetImageSparseMemoryRequirements, "vkGetImageSparseMemoryRequirements");

   procedure vkGetPhysicalDeviceSparseImageFormatProperties
     (physicalDevice : VkPhysicalDevice;
      format : VkFormat;
      c_type : VkImageType;
      samples : VkSampleCountFlagBits;
      usage : VkImageUsageFlags;
      tiling : VkImageTiling;
      pPropertyCount : access stdint_h.uint32_t;
      pProperties : access VkSparseImageFormatProperties);  -- vulkan_core.h:3448
   pragma Import (C, vkGetPhysicalDeviceSparseImageFormatProperties, "vkGetPhysicalDeviceSparseImageFormatProperties");

   function vkQueueBindSparse
     (queue : VkQueue;
      bindInfoCount : stdint_h.uint32_t;
      pBindInfo : System.Address;
      fence : VkFence) return VkResult;  -- vulkan_core.h:3458
   pragma Import (C, vkQueueBindSparse, "vkQueueBindSparse");

   function vkCreateFence
     (device : VkDevice;
      pCreateInfo : System.Address;
      pAllocator : System.Address;
      pFence : System.Address) return VkResult;  -- vulkan_core.h:3464
   pragma Import (C, vkCreateFence, "vkCreateFence");

   procedure vkDestroyFence
     (device : VkDevice;
      fence : VkFence;
      pAllocator : System.Address);  -- vulkan_core.h:3470
   pragma Import (C, vkDestroyFence, "vkDestroyFence");

   function vkResetFences
     (device : VkDevice;
      fenceCount : stdint_h.uint32_t;
      pFences : System.Address) return VkResult;  -- vulkan_core.h:3475
   pragma Import (C, vkResetFences, "vkResetFences");

   function vkGetFenceStatus (device : VkDevice; fence : VkFence) return VkResult;  -- vulkan_core.h:3480
   pragma Import (C, vkGetFenceStatus, "vkGetFenceStatus");

   function vkWaitForFences
     (device : VkDevice;
      fenceCount : stdint_h.uint32_t;
      pFences : System.Address;
      waitAll : VkBool32;
      timeout : stdint_h.uint64_t) return VkResult;  -- vulkan_core.h:3484
   pragma Import (C, vkWaitForFences, "vkWaitForFences");

   function vkCreateSemaphore
     (device : VkDevice;
      pCreateInfo : System.Address;
      pAllocator : System.Address;
      pSemaphore : System.Address) return VkResult;  -- vulkan_core.h:3491
   pragma Import (C, vkCreateSemaphore, "vkCreateSemaphore");

   procedure vkDestroySemaphore
     (device : VkDevice;
      semaphore : VkSemaphore;
      pAllocator : System.Address);  -- vulkan_core.h:3497
   pragma Import (C, vkDestroySemaphore, "vkDestroySemaphore");

   function vkCreateEvent
     (device : VkDevice;
      pCreateInfo : System.Address;
      pAllocator : System.Address;
      pEvent : System.Address) return VkResult;  -- vulkan_core.h:3502
   pragma Import (C, vkCreateEvent, "vkCreateEvent");

   procedure vkDestroyEvent
     (device : VkDevice;
      event : VkEvent;
      pAllocator : System.Address);  -- vulkan_core.h:3508
   pragma Import (C, vkDestroyEvent, "vkDestroyEvent");

   function vkGetEventStatus (device : VkDevice; event : VkEvent) return VkResult;  -- vulkan_core.h:3513
   pragma Import (C, vkGetEventStatus, "vkGetEventStatus");

   function vkSetEvent (device : VkDevice; event : VkEvent) return VkResult;  -- vulkan_core.h:3517
   pragma Import (C, vkSetEvent, "vkSetEvent");

   function vkResetEvent (device : VkDevice; event : VkEvent) return VkResult;  -- vulkan_core.h:3521
   pragma Import (C, vkResetEvent, "vkResetEvent");

   function vkCreateQueryPool
     (device : VkDevice;
      pCreateInfo : System.Address;
      pAllocator : System.Address;
      pQueryPool : System.Address) return VkResult;  -- vulkan_core.h:3525
   pragma Import (C, vkCreateQueryPool, "vkCreateQueryPool");

   procedure vkDestroyQueryPool
     (device : VkDevice;
      queryPool : VkQueryPool;
      pAllocator : System.Address);  -- vulkan_core.h:3531
   pragma Import (C, vkDestroyQueryPool, "vkDestroyQueryPool");

   function vkGetQueryPoolResults
     (device : VkDevice;
      queryPool : VkQueryPool;
      firstQuery : stdint_h.uint32_t;
      queryCount : stdint_h.uint32_t;
      dataSize : crtdefs_h.size_t;
      pData : System.Address;
      stride : VkDeviceSize;
      flags : VkQueryResultFlags) return VkResult;  -- vulkan_core.h:3536
   pragma Import (C, vkGetQueryPoolResults, "vkGetQueryPoolResults");

   function vkCreateBuffer
     (device : VkDevice;
      pCreateInfo : System.Address;
      pAllocator : System.Address;
      pBuffer : System.Address) return VkResult;  -- vulkan_core.h:3546
   pragma Import (C, vkCreateBuffer, "vkCreateBuffer");

   procedure vkDestroyBuffer
     (device : VkDevice;
      buffer : VkBuffer;
      pAllocator : System.Address);  -- vulkan_core.h:3552
   pragma Import (C, vkDestroyBuffer, "vkDestroyBuffer");

   function vkCreateBufferView
     (device : VkDevice;
      pCreateInfo : System.Address;
      pAllocator : System.Address;
      pView : System.Address) return VkResult;  -- vulkan_core.h:3557
   pragma Import (C, vkCreateBufferView, "vkCreateBufferView");

   procedure vkDestroyBufferView
     (device : VkDevice;
      bufferView : VkBufferView;
      pAllocator : System.Address);  -- vulkan_core.h:3563
   pragma Import (C, vkDestroyBufferView, "vkDestroyBufferView");

   function vkCreateImage
     (device : VkDevice;
      pCreateInfo : System.Address;
      pAllocator : System.Address;
      pImage : System.Address) return VkResult;  -- vulkan_core.h:3568
   pragma Import (C, vkCreateImage, "vkCreateImage");

   procedure vkDestroyImage
     (device : VkDevice;
      image : VkImage;
      pAllocator : System.Address);  -- vulkan_core.h:3574
   pragma Import (C, vkDestroyImage, "vkDestroyImage");

   procedure vkGetImageSubresourceLayout
     (device : VkDevice;
      image : VkImage;
      pSubresource : System.Address;
      pLayout : access VkSubresourceLayout);  -- vulkan_core.h:3579
   pragma Import (C, vkGetImageSubresourceLayout, "vkGetImageSubresourceLayout");

   function vkCreateImageView
     (device : VkDevice;
      pCreateInfo : System.Address;
      pAllocator : System.Address;
      pView : System.Address) return VkResult;  -- vulkan_core.h:3585
   pragma Import (C, vkCreateImageView, "vkCreateImageView");

   procedure vkDestroyImageView
     (device : VkDevice;
      imageView : VkImageView;
      pAllocator : System.Address);  -- vulkan_core.h:3591
   pragma Import (C, vkDestroyImageView, "vkDestroyImageView");

   function vkCreateShaderModule
     (device : VkDevice;
      pCreateInfo : System.Address;
      pAllocator : System.Address;
      pShaderModule : System.Address) return VkResult;  -- vulkan_core.h:3596
   pragma Import (C, vkCreateShaderModule, "vkCreateShaderModule");

   procedure vkDestroyShaderModule
     (device : VkDevice;
      shaderModule : VkShaderModule;
      pAllocator : System.Address);  -- vulkan_core.h:3602
   pragma Import (C, vkDestroyShaderModule, "vkDestroyShaderModule");

   function vkCreatePipelineCache
     (device : VkDevice;
      pCreateInfo : System.Address;
      pAllocator : System.Address;
      pPipelineCache : System.Address) return VkResult;  -- vulkan_core.h:3607
   pragma Import (C, vkCreatePipelineCache, "vkCreatePipelineCache");

   procedure vkDestroyPipelineCache
     (device : VkDevice;
      pipelineCache : VkPipelineCache;
      pAllocator : System.Address);  -- vulkan_core.h:3613
   pragma Import (C, vkDestroyPipelineCache, "vkDestroyPipelineCache");

   function vkGetPipelineCacheData
     (device : VkDevice;
      pipelineCache : VkPipelineCache;
      pDataSize : access crtdefs_h.size_t;
      pData : System.Address) return VkResult;  -- vulkan_core.h:3618
   pragma Import (C, vkGetPipelineCacheData, "vkGetPipelineCacheData");

   function vkMergePipelineCaches
     (device : VkDevice;
      dstCache : VkPipelineCache;
      srcCacheCount : stdint_h.uint32_t;
      pSrcCaches : System.Address) return VkResult;  -- vulkan_core.h:3624
   pragma Import (C, vkMergePipelineCaches, "vkMergePipelineCaches");

   function vkCreateGraphicsPipelines
     (device : VkDevice;
      pipelineCache : VkPipelineCache;
      createInfoCount : stdint_h.uint32_t;
      pCreateInfos : System.Address;
      pAllocator : System.Address;
      pPipelines : System.Address) return VkResult;  -- vulkan_core.h:3630
   pragma Import (C, vkCreateGraphicsPipelines, "vkCreateGraphicsPipelines");

   function vkCreateComputePipelines
     (device : VkDevice;
      pipelineCache : VkPipelineCache;
      createInfoCount : stdint_h.uint32_t;
      pCreateInfos : System.Address;
      pAllocator : System.Address;
      pPipelines : System.Address) return VkResult;  -- vulkan_core.h:3638
   pragma Import (C, vkCreateComputePipelines, "vkCreateComputePipelines");

   procedure vkDestroyPipeline
     (device : VkDevice;
      pipeline : VkPipeline;
      pAllocator : System.Address);  -- vulkan_core.h:3646
   pragma Import (C, vkDestroyPipeline, "vkDestroyPipeline");

   function vkCreatePipelineLayout
     (device : VkDevice;
      pCreateInfo : System.Address;
      pAllocator : System.Address;
      pPipelineLayout : System.Address) return VkResult;  -- vulkan_core.h:3651
   pragma Import (C, vkCreatePipelineLayout, "vkCreatePipelineLayout");

   procedure vkDestroyPipelineLayout
     (device : VkDevice;
      pipelineLayout : VkPipelineLayout;
      pAllocator : System.Address);  -- vulkan_core.h:3657
   pragma Import (C, vkDestroyPipelineLayout, "vkDestroyPipelineLayout");

   function vkCreateSampler
     (device : VkDevice;
      pCreateInfo : System.Address;
      pAllocator : System.Address;
      pSampler : System.Address) return VkResult;  -- vulkan_core.h:3662
   pragma Import (C, vkCreateSampler, "vkCreateSampler");

   procedure vkDestroySampler
     (device : VkDevice;
      sampler : VkSampler;
      pAllocator : System.Address);  -- vulkan_core.h:3668
   pragma Import (C, vkDestroySampler, "vkDestroySampler");

   function vkCreateDescriptorSetLayout
     (device : VkDevice;
      pCreateInfo : System.Address;
      pAllocator : System.Address;
      pSetLayout : System.Address) return VkResult;  -- vulkan_core.h:3673
   pragma Import (C, vkCreateDescriptorSetLayout, "vkCreateDescriptorSetLayout");

   procedure vkDestroyDescriptorSetLayout
     (device : VkDevice;
      descriptorSetLayout : VkDescriptorSetLayout;
      pAllocator : System.Address);  -- vulkan_core.h:3679
   pragma Import (C, vkDestroyDescriptorSetLayout, "vkDestroyDescriptorSetLayout");

   function vkCreateDescriptorPool
     (device : VkDevice;
      pCreateInfo : System.Address;
      pAllocator : System.Address;
      pDescriptorPool : System.Address) return VkResult;  -- vulkan_core.h:3684
   pragma Import (C, vkCreateDescriptorPool, "vkCreateDescriptorPool");

   procedure vkDestroyDescriptorPool
     (device : VkDevice;
      descriptorPool : VkDescriptorPool;
      pAllocator : System.Address);  -- vulkan_core.h:3690
   pragma Import (C, vkDestroyDescriptorPool, "vkDestroyDescriptorPool");

   function vkResetDescriptorPool
     (device : VkDevice;
      descriptorPool : VkDescriptorPool;
      flags : VkDescriptorPoolResetFlags) return VkResult;  -- vulkan_core.h:3695
   pragma Import (C, vkResetDescriptorPool, "vkResetDescriptorPool");

   function vkAllocateDescriptorSets
     (device : VkDevice;
      pAllocateInfo : System.Address;
      pDescriptorSets : System.Address) return VkResult;  -- vulkan_core.h:3700
   pragma Import (C, vkAllocateDescriptorSets, "vkAllocateDescriptorSets");

   function vkFreeDescriptorSets
     (device : VkDevice;
      descriptorPool : VkDescriptorPool;
      descriptorSetCount : stdint_h.uint32_t;
      pDescriptorSets : System.Address) return VkResult;  -- vulkan_core.h:3705
   pragma Import (C, vkFreeDescriptorSets, "vkFreeDescriptorSets");

   procedure vkUpdateDescriptorSets
     (device : VkDevice;
      descriptorWriteCount : stdint_h.uint32_t;
      pDescriptorWrites : System.Address;
      descriptorCopyCount : stdint_h.uint32_t;
      pDescriptorCopies : System.Address);  -- vulkan_core.h:3711
   pragma Import (C, vkUpdateDescriptorSets, "vkUpdateDescriptorSets");

   function vkCreateFramebuffer
     (device : VkDevice;
      pCreateInfo : System.Address;
      pAllocator : System.Address;
      pFramebuffer : System.Address) return VkResult;  -- vulkan_core.h:3718
   pragma Import (C, vkCreateFramebuffer, "vkCreateFramebuffer");

   procedure vkDestroyFramebuffer
     (device : VkDevice;
      framebuffer : VkFramebuffer;
      pAllocator : System.Address);  -- vulkan_core.h:3724
   pragma Import (C, vkDestroyFramebuffer, "vkDestroyFramebuffer");

   function vkCreateRenderPass
     (device : VkDevice;
      pCreateInfo : System.Address;
      pAllocator : System.Address;
      pRenderPass : System.Address) return VkResult;  -- vulkan_core.h:3729
   pragma Import (C, vkCreateRenderPass, "vkCreateRenderPass");

   procedure vkDestroyRenderPass
     (device : VkDevice;
      renderPass : VkRenderPass;
      pAllocator : System.Address);  -- vulkan_core.h:3735
   pragma Import (C, vkDestroyRenderPass, "vkDestroyRenderPass");

   procedure vkGetRenderAreaGranularity
     (device : VkDevice;
      renderPass : VkRenderPass;
      pGranularity : access VkExtent2D);  -- vulkan_core.h:3740
   pragma Import (C, vkGetRenderAreaGranularity, "vkGetRenderAreaGranularity");

   function vkCreateCommandPool
     (device : VkDevice;
      pCreateInfo : System.Address;
      pAllocator : System.Address;
      pCommandPool : System.Address) return VkResult;  -- vulkan_core.h:3745
   pragma Import (C, vkCreateCommandPool, "vkCreateCommandPool");

   procedure vkDestroyCommandPool
     (device : VkDevice;
      commandPool : VkCommandPool;
      pAllocator : System.Address);  -- vulkan_core.h:3751
   pragma Import (C, vkDestroyCommandPool, "vkDestroyCommandPool");

   function vkResetCommandPool
     (device : VkDevice;
      commandPool : VkCommandPool;
      flags : VkCommandPoolResetFlags) return VkResult;  -- vulkan_core.h:3756
   pragma Import (C, vkResetCommandPool, "vkResetCommandPool");

   function vkAllocateCommandBuffers
     (device : VkDevice;
      pAllocateInfo : System.Address;
      pCommandBuffers : System.Address) return VkResult;  -- vulkan_core.h:3761
   pragma Import (C, vkAllocateCommandBuffers, "vkAllocateCommandBuffers");

   procedure vkFreeCommandBuffers
     (device : VkDevice;
      commandPool : VkCommandPool;
      commandBufferCount : stdint_h.uint32_t;
      pCommandBuffers : System.Address);  -- vulkan_core.h:3766
   pragma Import (C, vkFreeCommandBuffers, "vkFreeCommandBuffers");

   function vkBeginCommandBuffer (commandBuffer : VkCommandBuffer; pBeginInfo : System.Address) return VkResult;  -- vulkan_core.h:3772
   pragma Import (C, vkBeginCommandBuffer, "vkBeginCommandBuffer");

   function vkEndCommandBuffer (commandBuffer : VkCommandBuffer) return VkResult;  -- vulkan_core.h:3776
   pragma Import (C, vkEndCommandBuffer, "vkEndCommandBuffer");

   function vkResetCommandBuffer (commandBuffer : VkCommandBuffer; flags : VkCommandBufferResetFlags) return VkResult;  -- vulkan_core.h:3779
   pragma Import (C, vkResetCommandBuffer, "vkResetCommandBuffer");

   procedure vkCmdBindPipeline
     (commandBuffer : VkCommandBuffer;
      pipelineBindPoint : VkPipelineBindPoint;
      pipeline : VkPipeline);  -- vulkan_core.h:3783
   pragma Import (C, vkCmdBindPipeline, "vkCmdBindPipeline");

   procedure vkCmdSetViewport
     (commandBuffer : VkCommandBuffer;
      firstViewport : stdint_h.uint32_t;
      viewportCount : stdint_h.uint32_t;
      pViewports : System.Address);  -- vulkan_core.h:3788
   pragma Import (C, vkCmdSetViewport, "vkCmdSetViewport");

   procedure vkCmdSetScissor
     (commandBuffer : VkCommandBuffer;
      firstScissor : stdint_h.uint32_t;
      scissorCount : stdint_h.uint32_t;
      pScissors : System.Address);  -- vulkan_core.h:3794
   pragma Import (C, vkCmdSetScissor, "vkCmdSetScissor");

   procedure vkCmdSetLineWidth (commandBuffer : VkCommandBuffer; lineWidth : float);  -- vulkan_core.h:3800
   pragma Import (C, vkCmdSetLineWidth, "vkCmdSetLineWidth");

   procedure vkCmdSetDepthBias
     (commandBuffer : VkCommandBuffer;
      depthBiasConstantFactor : float;
      depthBiasClamp : float;
      depthBiasSlopeFactor : float);  -- vulkan_core.h:3804
   pragma Import (C, vkCmdSetDepthBias, "vkCmdSetDepthBias");

   procedure vkCmdSetBlendConstants (commandBuffer : VkCommandBuffer; blendConstants : access float);  -- vulkan_core.h:3810
   pragma Import (C, vkCmdSetBlendConstants, "vkCmdSetBlendConstants");

   procedure vkCmdSetDepthBounds
     (commandBuffer : VkCommandBuffer;
      minDepthBounds : float;
      maxDepthBounds : float);  -- vulkan_core.h:3814
   pragma Import (C, vkCmdSetDepthBounds, "vkCmdSetDepthBounds");

   procedure vkCmdSetStencilCompareMask
     (commandBuffer : VkCommandBuffer;
      faceMask : VkStencilFaceFlags;
      compareMask : stdint_h.uint32_t);  -- vulkan_core.h:3819
   pragma Import (C, vkCmdSetStencilCompareMask, "vkCmdSetStencilCompareMask");

   procedure vkCmdSetStencilWriteMask
     (commandBuffer : VkCommandBuffer;
      faceMask : VkStencilFaceFlags;
      writeMask : stdint_h.uint32_t);  -- vulkan_core.h:3824
   pragma Import (C, vkCmdSetStencilWriteMask, "vkCmdSetStencilWriteMask");

   procedure vkCmdSetStencilReference
     (commandBuffer : VkCommandBuffer;
      faceMask : VkStencilFaceFlags;
      reference : stdint_h.uint32_t);  -- vulkan_core.h:3829
   pragma Import (C, vkCmdSetStencilReference, "vkCmdSetStencilReference");

   procedure vkCmdBindDescriptorSets
     (commandBuffer : VkCommandBuffer;
      pipelineBindPoint : VkPipelineBindPoint;
      layout : VkPipelineLayout;
      firstSet : stdint_h.uint32_t;
      descriptorSetCount : stdint_h.uint32_t;
      pDescriptorSets : System.Address;
      dynamicOffsetCount : stdint_h.uint32_t;
      pDynamicOffsets : access stdint_h.uint32_t);  -- vulkan_core.h:3834
   pragma Import (C, vkCmdBindDescriptorSets, "vkCmdBindDescriptorSets");

   procedure vkCmdBindIndexBuffer
     (commandBuffer : VkCommandBuffer;
      buffer : VkBuffer;
      offset : VkDeviceSize;
      indexType : VkIndexType);  -- vulkan_core.h:3844
   pragma Import (C, vkCmdBindIndexBuffer, "vkCmdBindIndexBuffer");

   procedure vkCmdBindVertexBuffers
     (commandBuffer : VkCommandBuffer;
      firstBinding : stdint_h.uint32_t;
      bindingCount : stdint_h.uint32_t;
      pBuffers : System.Address;
      pOffsets : access VkDeviceSize);  -- vulkan_core.h:3850
   pragma Import (C, vkCmdBindVertexBuffers, "vkCmdBindVertexBuffers");

   procedure vkCmdDraw
     (commandBuffer : VkCommandBuffer;
      vertexCount : stdint_h.uint32_t;
      instanceCount : stdint_h.uint32_t;
      firstVertex : stdint_h.uint32_t;
      firstInstance : stdint_h.uint32_t);  -- vulkan_core.h:3857
   pragma Import (C, vkCmdDraw, "vkCmdDraw");

   procedure vkCmdDrawIndexed
     (commandBuffer : VkCommandBuffer;
      indexCount : stdint_h.uint32_t;
      instanceCount : stdint_h.uint32_t;
      firstIndex : stdint_h.uint32_t;
      vertexOffset : stdint_h.int32_t;
      firstInstance : stdint_h.uint32_t);  -- vulkan_core.h:3864
   pragma Import (C, vkCmdDrawIndexed, "vkCmdDrawIndexed");

   procedure vkCmdDrawIndirect
     (commandBuffer : VkCommandBuffer;
      buffer : VkBuffer;
      offset : VkDeviceSize;
      drawCount : stdint_h.uint32_t;
      stride : stdint_h.uint32_t);  -- vulkan_core.h:3872
   pragma Import (C, vkCmdDrawIndirect, "vkCmdDrawIndirect");

   procedure vkCmdDrawIndexedIndirect
     (commandBuffer : VkCommandBuffer;
      buffer : VkBuffer;
      offset : VkDeviceSize;
      drawCount : stdint_h.uint32_t;
      stride : stdint_h.uint32_t);  -- vulkan_core.h:3879
   pragma Import (C, vkCmdDrawIndexedIndirect, "vkCmdDrawIndexedIndirect");

   procedure vkCmdDispatch
     (commandBuffer : VkCommandBuffer;
      groupCountX : stdint_h.uint32_t;
      groupCountY : stdint_h.uint32_t;
      groupCountZ : stdint_h.uint32_t);  -- vulkan_core.h:3886
   pragma Import (C, vkCmdDispatch, "vkCmdDispatch");

   procedure vkCmdDispatchIndirect
     (commandBuffer : VkCommandBuffer;
      buffer : VkBuffer;
      offset : VkDeviceSize);  -- vulkan_core.h:3892
   pragma Import (C, vkCmdDispatchIndirect, "vkCmdDispatchIndirect");

   procedure vkCmdCopyBuffer
     (commandBuffer : VkCommandBuffer;
      srcBuffer : VkBuffer;
      dstBuffer : VkBuffer;
      regionCount : stdint_h.uint32_t;
      pRegions : System.Address);  -- vulkan_core.h:3897
   pragma Import (C, vkCmdCopyBuffer, "vkCmdCopyBuffer");

   procedure vkCmdCopyImage
     (commandBuffer : VkCommandBuffer;
      srcImage : VkImage;
      srcImageLayout : VkImageLayout;
      dstImage : VkImage;
      dstImageLayout : VkImageLayout;
      regionCount : stdint_h.uint32_t;
      pRegions : System.Address);  -- vulkan_core.h:3904
   pragma Import (C, vkCmdCopyImage, "vkCmdCopyImage");

   procedure vkCmdBlitImage
     (commandBuffer : VkCommandBuffer;
      srcImage : VkImage;
      srcImageLayout : VkImageLayout;
      dstImage : VkImage;
      dstImageLayout : VkImageLayout;
      regionCount : stdint_h.uint32_t;
      pRegions : System.Address;
      filter : VkFilter);  -- vulkan_core.h:3913
   pragma Import (C, vkCmdBlitImage, "vkCmdBlitImage");

   procedure vkCmdCopyBufferToImage
     (commandBuffer : VkCommandBuffer;
      srcBuffer : VkBuffer;
      dstImage : VkImage;
      dstImageLayout : VkImageLayout;
      regionCount : stdint_h.uint32_t;
      pRegions : System.Address);  -- vulkan_core.h:3923
   pragma Import (C, vkCmdCopyBufferToImage, "vkCmdCopyBufferToImage");

   procedure vkCmdCopyImageToBuffer
     (commandBuffer : VkCommandBuffer;
      srcImage : VkImage;
      srcImageLayout : VkImageLayout;
      dstBuffer : VkBuffer;
      regionCount : stdint_h.uint32_t;
      pRegions : System.Address);  -- vulkan_core.h:3931
   pragma Import (C, vkCmdCopyImageToBuffer, "vkCmdCopyImageToBuffer");

   procedure vkCmdUpdateBuffer
     (commandBuffer : VkCommandBuffer;
      dstBuffer : VkBuffer;
      dstOffset : VkDeviceSize;
      dataSize : VkDeviceSize;
      pData : System.Address);  -- vulkan_core.h:3939
   pragma Import (C, vkCmdUpdateBuffer, "vkCmdUpdateBuffer");

   procedure vkCmdFillBuffer
     (commandBuffer : VkCommandBuffer;
      dstBuffer : VkBuffer;
      dstOffset : VkDeviceSize;
      size : VkDeviceSize;
      data : stdint_h.uint32_t);  -- vulkan_core.h:3946
   pragma Import (C, vkCmdFillBuffer, "vkCmdFillBuffer");

   procedure vkCmdClearColorImage
     (commandBuffer : VkCommandBuffer;
      image : VkImage;
      imageLayout : VkImageLayout;
      pColor : System.Address;
      rangeCount : stdint_h.uint32_t;
      pRanges : System.Address);  -- vulkan_core.h:3953
   pragma Import (C, vkCmdClearColorImage, "vkCmdClearColorImage");

   procedure vkCmdClearDepthStencilImage
     (commandBuffer : VkCommandBuffer;
      image : VkImage;
      imageLayout : VkImageLayout;
      pDepthStencil : System.Address;
      rangeCount : stdint_h.uint32_t;
      pRanges : System.Address);  -- vulkan_core.h:3961
   pragma Import (C, vkCmdClearDepthStencilImage, "vkCmdClearDepthStencilImage");

   procedure vkCmdClearAttachments
     (commandBuffer : VkCommandBuffer;
      attachmentCount : stdint_h.uint32_t;
      pAttachments : System.Address;
      rectCount : stdint_h.uint32_t;
      pRects : System.Address);  -- vulkan_core.h:3969
   pragma Import (C, vkCmdClearAttachments, "vkCmdClearAttachments");

   procedure vkCmdResolveImage
     (commandBuffer : VkCommandBuffer;
      srcImage : VkImage;
      srcImageLayout : VkImageLayout;
      dstImage : VkImage;
      dstImageLayout : VkImageLayout;
      regionCount : stdint_h.uint32_t;
      pRegions : System.Address);  -- vulkan_core.h:3976
   pragma Import (C, vkCmdResolveImage, "vkCmdResolveImage");

   procedure vkCmdSetEvent
     (commandBuffer : VkCommandBuffer;
      event : VkEvent;
      stageMask : VkPipelineStageFlags);  -- vulkan_core.h:3985
   pragma Import (C, vkCmdSetEvent, "vkCmdSetEvent");

   procedure vkCmdResetEvent
     (commandBuffer : VkCommandBuffer;
      event : VkEvent;
      stageMask : VkPipelineStageFlags);  -- vulkan_core.h:3990
   pragma Import (C, vkCmdResetEvent, "vkCmdResetEvent");

   procedure vkCmdWaitEvents
     (commandBuffer : VkCommandBuffer;
      eventCount : stdint_h.uint32_t;
      pEvents : System.Address;
      srcStageMask : VkPipelineStageFlags;
      dstStageMask : VkPipelineStageFlags;
      memoryBarrierCount : stdint_h.uint32_t;
      pMemoryBarriers : System.Address;
      bufferMemoryBarrierCount : stdint_h.uint32_t;
      pBufferMemoryBarriers : System.Address;
      imageMemoryBarrierCount : stdint_h.uint32_t;
      pImageMemoryBarriers : System.Address);  -- vulkan_core.h:3995
   pragma Import (C, vkCmdWaitEvents, "vkCmdWaitEvents");

   procedure vkCmdPipelineBarrier
     (commandBuffer : VkCommandBuffer;
      srcStageMask : VkPipelineStageFlags;
      dstStageMask : VkPipelineStageFlags;
      dependencyFlags : VkDependencyFlags;
      memoryBarrierCount : stdint_h.uint32_t;
      pMemoryBarriers : System.Address;
      bufferMemoryBarrierCount : stdint_h.uint32_t;
      pBufferMemoryBarriers : System.Address;
      imageMemoryBarrierCount : stdint_h.uint32_t;
      pImageMemoryBarriers : System.Address);  -- vulkan_core.h:4008
   pragma Import (C, vkCmdPipelineBarrier, "vkCmdPipelineBarrier");

   procedure vkCmdBeginQuery
     (commandBuffer : VkCommandBuffer;
      queryPool : VkQueryPool;
      query : stdint_h.uint32_t;
      flags : VkQueryControlFlags);  -- vulkan_core.h:4020
   pragma Import (C, vkCmdBeginQuery, "vkCmdBeginQuery");

   procedure vkCmdEndQuery
     (commandBuffer : VkCommandBuffer;
      queryPool : VkQueryPool;
      query : stdint_h.uint32_t);  -- vulkan_core.h:4026
   pragma Import (C, vkCmdEndQuery, "vkCmdEndQuery");

   procedure vkCmdResetQueryPool
     (commandBuffer : VkCommandBuffer;
      queryPool : VkQueryPool;
      firstQuery : stdint_h.uint32_t;
      queryCount : stdint_h.uint32_t);  -- vulkan_core.h:4031
   pragma Import (C, vkCmdResetQueryPool, "vkCmdResetQueryPool");

   procedure vkCmdWriteTimestamp
     (commandBuffer : VkCommandBuffer;
      pipelineStage : VkPipelineStageFlagBits;
      queryPool : VkQueryPool;
      query : stdint_h.uint32_t);  -- vulkan_core.h:4037
   pragma Import (C, vkCmdWriteTimestamp, "vkCmdWriteTimestamp");

   procedure vkCmdCopyQueryPoolResults
     (commandBuffer : VkCommandBuffer;
      queryPool : VkQueryPool;
      firstQuery : stdint_h.uint32_t;
      queryCount : stdint_h.uint32_t;
      dstBuffer : VkBuffer;
      dstOffset : VkDeviceSize;
      stride : VkDeviceSize;
      flags : VkQueryResultFlags);  -- vulkan_core.h:4043
   pragma Import (C, vkCmdCopyQueryPoolResults, "vkCmdCopyQueryPoolResults");

   procedure vkCmdPushConstants
     (commandBuffer : VkCommandBuffer;
      layout : VkPipelineLayout;
      stageFlags : VkShaderStageFlags;
      offset : stdint_h.uint32_t;
      size : stdint_h.uint32_t;
      pValues : System.Address);  -- vulkan_core.h:4053
   pragma Import (C, vkCmdPushConstants, "vkCmdPushConstants");

   procedure vkCmdBeginRenderPass
     (commandBuffer : VkCommandBuffer;
      pRenderPassBegin : System.Address;
      contents : VkSubpassContents);  -- vulkan_core.h:4061
   pragma Import (C, vkCmdBeginRenderPass, "vkCmdBeginRenderPass");

   procedure vkCmdNextSubpass (commandBuffer : VkCommandBuffer; contents : VkSubpassContents);  -- vulkan_core.h:4066
   pragma Import (C, vkCmdNextSubpass, "vkCmdNextSubpass");

   procedure vkCmdEndRenderPass (commandBuffer : VkCommandBuffer);  -- vulkan_core.h:4070
   pragma Import (C, vkCmdEndRenderPass, "vkCmdEndRenderPass");

   procedure vkCmdExecuteCommands
     (commandBuffer : VkCommandBuffer;
      commandBufferCount : stdint_h.uint32_t;
      pCommandBuffers : System.Address);  -- vulkan_core.h:4073
   pragma Import (C, vkCmdExecuteCommands, "vkCmdExecuteCommands");

  -- Vulkan 1.1 version number
   --  skipped empty struct VkSamplerYcbcrConversion_T

   type VkSamplerYcbcrConversion is new System.Address;  -- vulkan_core.h:4084

   type VkDescriptorUpdateTemplate is new System.Address;  -- vulkan_core.h:4085

   --  skipped empty struct VkDescriptorUpdateTemplate_T

   subtype VkPointClippingBehavior is unsigned;
   VK_POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES : constant VkPointClippingBehavior := 0;
   VK_POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY : constant VkPointClippingBehavior := 1;
   VK_POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES_KHR : constant VkPointClippingBehavior := 0;
   VK_POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY_KHR : constant VkPointClippingBehavior := 1;
   VK_POINT_CLIPPING_BEHAVIOR_MAX_ENUM : constant VkPointClippingBehavior := 2147483647;  -- vulkan_core.h:4090

   subtype VkTessellationDomainOrigin is unsigned;
   VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT : constant VkTessellationDomainOrigin := 0;
   VK_TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT : constant VkTessellationDomainOrigin := 1;
   VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT_KHR : constant VkTessellationDomainOrigin := 0;
   VK_TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT_KHR : constant VkTessellationDomainOrigin := 1;
   VK_TESSELLATION_DOMAIN_ORIGIN_MAX_ENUM : constant VkTessellationDomainOrigin := 2147483647;  -- vulkan_core.h:4098

   subtype VkSamplerYcbcrModelConversion is unsigned;
   VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY : constant VkSamplerYcbcrModelConversion := 0;
   VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY : constant VkSamplerYcbcrModelConversion := 1;
   VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709 : constant VkSamplerYcbcrModelConversion := 2;
   VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601 : constant VkSamplerYcbcrModelConversion := 3;
   VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020 : constant VkSamplerYcbcrModelConversion := 4;
   VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY_KHR : constant VkSamplerYcbcrModelConversion := 0;
   VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY_KHR : constant VkSamplerYcbcrModelConversion := 1;
   VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709_KHR : constant VkSamplerYcbcrModelConversion := 2;
   VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601_KHR : constant VkSamplerYcbcrModelConversion := 3;
   VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020_KHR : constant VkSamplerYcbcrModelConversion := 4;
   VK_SAMPLER_YCBCR_MODEL_CONVERSION_MAX_ENUM : constant VkSamplerYcbcrModelConversion := 2147483647;  -- vulkan_core.h:4106

   subtype VkSamplerYcbcrRange is unsigned;
   VK_SAMPLER_YCBCR_RANGE_ITU_FULL : constant VkSamplerYcbcrRange := 0;
   VK_SAMPLER_YCBCR_RANGE_ITU_NARROW : constant VkSamplerYcbcrRange := 1;
   VK_SAMPLER_YCBCR_RANGE_ITU_FULL_KHR : constant VkSamplerYcbcrRange := 0;
   VK_SAMPLER_YCBCR_RANGE_ITU_NARROW_KHR : constant VkSamplerYcbcrRange := 1;
   VK_SAMPLER_YCBCR_RANGE_MAX_ENUM : constant VkSamplerYcbcrRange := 2147483647;  -- vulkan_core.h:4120

   subtype VkChromaLocation is unsigned;
   VK_CHROMA_LOCATION_COSITED_EVEN : constant VkChromaLocation := 0;
   VK_CHROMA_LOCATION_MIDPOINT : constant VkChromaLocation := 1;
   VK_CHROMA_LOCATION_COSITED_EVEN_KHR : constant VkChromaLocation := 0;
   VK_CHROMA_LOCATION_MIDPOINT_KHR : constant VkChromaLocation := 1;
   VK_CHROMA_LOCATION_MAX_ENUM : constant VkChromaLocation := 2147483647;  -- vulkan_core.h:4128

   subtype VkDescriptorUpdateTemplateType is unsigned;
   VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET : constant VkDescriptorUpdateTemplateType := 0;
   VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR : constant VkDescriptorUpdateTemplateType := 1;
   VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET_KHR : constant VkDescriptorUpdateTemplateType := 0;
   VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_MAX_ENUM : constant VkDescriptorUpdateTemplateType := 2147483647;  -- vulkan_core.h:4136

   subtype VkSubgroupFeatureFlagBits is unsigned;
   VK_SUBGROUP_FEATURE_BASIC_BIT : constant VkSubgroupFeatureFlagBits := 1;
   VK_SUBGROUP_FEATURE_VOTE_BIT : constant VkSubgroupFeatureFlagBits := 2;
   VK_SUBGROUP_FEATURE_ARITHMETIC_BIT : constant VkSubgroupFeatureFlagBits := 4;
   VK_SUBGROUP_FEATURE_BALLOT_BIT : constant VkSubgroupFeatureFlagBits := 8;
   VK_SUBGROUP_FEATURE_SHUFFLE_BIT : constant VkSubgroupFeatureFlagBits := 16;
   VK_SUBGROUP_FEATURE_SHUFFLE_RELATIVE_BIT : constant VkSubgroupFeatureFlagBits := 32;
   VK_SUBGROUP_FEATURE_CLUSTERED_BIT : constant VkSubgroupFeatureFlagBits := 64;
   VK_SUBGROUP_FEATURE_QUAD_BIT : constant VkSubgroupFeatureFlagBits := 128;
   VK_SUBGROUP_FEATURE_PARTITIONED_BIT_NV : constant VkSubgroupFeatureFlagBits := 256;
   VK_SUBGROUP_FEATURE_FLAG_BITS_MAX_ENUM : constant VkSubgroupFeatureFlagBits := 2147483647;  -- vulkan_core.h:4143

   subtype VkSubgroupFeatureFlags is VkFlags;  -- vulkan_core.h:4155

   subtype VkPeerMemoryFeatureFlagBits is unsigned;
   VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT : constant VkPeerMemoryFeatureFlagBits := 1;
   VK_PEER_MEMORY_FEATURE_COPY_DST_BIT : constant VkPeerMemoryFeatureFlagBits := 2;
   VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT : constant VkPeerMemoryFeatureFlagBits := 4;
   VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT : constant VkPeerMemoryFeatureFlagBits := 8;
   VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT_KHR : constant VkPeerMemoryFeatureFlagBits := 1;
   VK_PEER_MEMORY_FEATURE_COPY_DST_BIT_KHR : constant VkPeerMemoryFeatureFlagBits := 2;
   VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT_KHR : constant VkPeerMemoryFeatureFlagBits := 4;
   VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT_KHR : constant VkPeerMemoryFeatureFlagBits := 8;
   VK_PEER_MEMORY_FEATURE_FLAG_BITS_MAX_ENUM : constant VkPeerMemoryFeatureFlagBits := 2147483647;  -- vulkan_core.h:4157

   subtype VkPeerMemoryFeatureFlags is VkFlags;  -- vulkan_core.h:4168

   subtype VkMemoryAllocateFlagBits is unsigned;
   VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT : constant VkMemoryAllocateFlagBits := 1;
   VK_MEMORY_ALLOCATE_DEVICE_ADDRESS_BIT : constant VkMemoryAllocateFlagBits := 2;
   VK_MEMORY_ALLOCATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT : constant VkMemoryAllocateFlagBits := 4;
   VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT_KHR : constant VkMemoryAllocateFlagBits := 1;
   VK_MEMORY_ALLOCATE_DEVICE_ADDRESS_BIT_KHR : constant VkMemoryAllocateFlagBits := 2;
   VK_MEMORY_ALLOCATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_KHR : constant VkMemoryAllocateFlagBits := 4;
   VK_MEMORY_ALLOCATE_FLAG_BITS_MAX_ENUM : constant VkMemoryAllocateFlagBits := 2147483647;  -- vulkan_core.h:4170

   subtype VkMemoryAllocateFlags is VkFlags;  -- vulkan_core.h:4179

   subtype VkCommandPoolTrimFlags is VkFlags;  -- vulkan_core.h:4180

   subtype VkDescriptorUpdateTemplateCreateFlags is VkFlags;  -- vulkan_core.h:4181

   subtype VkExternalMemoryHandleTypeFlagBits is unsigned;
   VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT : constant VkExternalMemoryHandleTypeFlagBits := 1;
   VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT : constant VkExternalMemoryHandleTypeFlagBits := 2;
   VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT : constant VkExternalMemoryHandleTypeFlagBits := 4;
   VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT : constant VkExternalMemoryHandleTypeFlagBits := 8;
   VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT : constant VkExternalMemoryHandleTypeFlagBits := 16;
   VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT : constant VkExternalMemoryHandleTypeFlagBits := 32;
   VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT : constant VkExternalMemoryHandleTypeFlagBits := 64;
   VK_EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT : constant VkExternalMemoryHandleTypeFlagBits := 512;
   VK_EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID : constant VkExternalMemoryHandleTypeFlagBits := 1024;
   VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT : constant VkExternalMemoryHandleTypeFlagBits := 128;
   VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT : constant VkExternalMemoryHandleTypeFlagBits := 256;
   VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT_KHR : constant VkExternalMemoryHandleTypeFlagBits := 1;
   VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR : constant VkExternalMemoryHandleTypeFlagBits := 2;
   VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR : constant VkExternalMemoryHandleTypeFlagBits := 4;
   VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT_KHR : constant VkExternalMemoryHandleTypeFlagBits := 8;
   VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT_KHR : constant VkExternalMemoryHandleTypeFlagBits := 16;
   VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT_KHR : constant VkExternalMemoryHandleTypeFlagBits := 32;
   VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT_KHR : constant VkExternalMemoryHandleTypeFlagBits := 64;
   VK_EXTERNAL_MEMORY_HANDLE_TYPE_FLAG_BITS_MAX_ENUM : constant VkExternalMemoryHandleTypeFlagBits := 2147483647;  -- vulkan_core.h:4183

   subtype VkExternalMemoryHandleTypeFlags is VkFlags;  -- vulkan_core.h:4204

   subtype VkExternalMemoryFeatureFlagBits is unsigned;
   VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT : constant VkExternalMemoryFeatureFlagBits := 1;
   VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT : constant VkExternalMemoryFeatureFlagBits := 2;
   VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT : constant VkExternalMemoryFeatureFlagBits := 4;
   VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_KHR : constant VkExternalMemoryFeatureFlagBits := 1;
   VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_KHR : constant VkExternalMemoryFeatureFlagBits := 2;
   VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_KHR : constant VkExternalMemoryFeatureFlagBits := 4;
   VK_EXTERNAL_MEMORY_FEATURE_FLAG_BITS_MAX_ENUM : constant VkExternalMemoryFeatureFlagBits := 2147483647;  -- vulkan_core.h:4206

   subtype VkExternalMemoryFeatureFlags is VkFlags;  -- vulkan_core.h:4215

   subtype VkExternalFenceHandleTypeFlagBits is unsigned;
   VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT : constant VkExternalFenceHandleTypeFlagBits := 1;
   VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT : constant VkExternalFenceHandleTypeFlagBits := 2;
   VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT : constant VkExternalFenceHandleTypeFlagBits := 4;
   VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT : constant VkExternalFenceHandleTypeFlagBits := 8;
   VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR : constant VkExternalFenceHandleTypeFlagBits := 1;
   VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR : constant VkExternalFenceHandleTypeFlagBits := 2;
   VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR : constant VkExternalFenceHandleTypeFlagBits := 4;
   VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT_KHR : constant VkExternalFenceHandleTypeFlagBits := 8;
   VK_EXTERNAL_FENCE_HANDLE_TYPE_FLAG_BITS_MAX_ENUM : constant VkExternalFenceHandleTypeFlagBits := 2147483647;  -- vulkan_core.h:4217

   subtype VkExternalFenceHandleTypeFlags is VkFlags;  -- vulkan_core.h:4228

   subtype VkExternalFenceFeatureFlagBits is unsigned;
   VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT : constant VkExternalFenceFeatureFlagBits := 1;
   VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT : constant VkExternalFenceFeatureFlagBits := 2;
   VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT_KHR : constant VkExternalFenceFeatureFlagBits := 1;
   VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT_KHR : constant VkExternalFenceFeatureFlagBits := 2;
   VK_EXTERNAL_FENCE_FEATURE_FLAG_BITS_MAX_ENUM : constant VkExternalFenceFeatureFlagBits := 2147483647;  -- vulkan_core.h:4230

   subtype VkExternalFenceFeatureFlags is VkFlags;  -- vulkan_core.h:4237

   subtype VkFenceImportFlagBits is unsigned;
   VK_FENCE_IMPORT_TEMPORARY_BIT : constant VkFenceImportFlagBits := 1;
   VK_FENCE_IMPORT_TEMPORARY_BIT_KHR : constant VkFenceImportFlagBits := 1;
   VK_FENCE_IMPORT_FLAG_BITS_MAX_ENUM : constant VkFenceImportFlagBits := 2147483647;  -- vulkan_core.h:4239

   subtype VkFenceImportFlags is VkFlags;  -- vulkan_core.h:4244

   subtype VkSemaphoreImportFlagBits is unsigned;
   VK_SEMAPHORE_IMPORT_TEMPORARY_BIT : constant VkSemaphoreImportFlagBits := 1;
   VK_SEMAPHORE_IMPORT_TEMPORARY_BIT_KHR : constant VkSemaphoreImportFlagBits := 1;
   VK_SEMAPHORE_IMPORT_FLAG_BITS_MAX_ENUM : constant VkSemaphoreImportFlagBits := 2147483647;  -- vulkan_core.h:4246

   subtype VkSemaphoreImportFlags is VkFlags;  -- vulkan_core.h:4251

   subtype VkExternalSemaphoreHandleTypeFlagBits is unsigned;
   VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT : constant VkExternalSemaphoreHandleTypeFlagBits := 1;
   VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT : constant VkExternalSemaphoreHandleTypeFlagBits := 2;
   VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT : constant VkExternalSemaphoreHandleTypeFlagBits := 4;
   VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT : constant VkExternalSemaphoreHandleTypeFlagBits := 8;
   VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT : constant VkExternalSemaphoreHandleTypeFlagBits := 16;
   VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D11_FENCE_BIT : constant VkExternalSemaphoreHandleTypeFlagBits := 8;
   VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR : constant VkExternalSemaphoreHandleTypeFlagBits := 1;
   VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR : constant VkExternalSemaphoreHandleTypeFlagBits := 2;
   VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR : constant VkExternalSemaphoreHandleTypeFlagBits := 4;
   VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT_KHR : constant VkExternalSemaphoreHandleTypeFlagBits := 8;
   VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT_KHR : constant VkExternalSemaphoreHandleTypeFlagBits := 16;
   VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_FLAG_BITS_MAX_ENUM : constant VkExternalSemaphoreHandleTypeFlagBits := 2147483647;  -- vulkan_core.h:4253

   subtype VkExternalSemaphoreHandleTypeFlags is VkFlags;  -- vulkan_core.h:4267

   subtype VkExternalSemaphoreFeatureFlagBits is unsigned;
   VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT : constant VkExternalSemaphoreFeatureFlagBits := 1;
   VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT : constant VkExternalSemaphoreFeatureFlagBits := 2;
   VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT_KHR : constant VkExternalSemaphoreFeatureFlagBits := 1;
   VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT_KHR : constant VkExternalSemaphoreFeatureFlagBits := 2;
   VK_EXTERNAL_SEMAPHORE_FEATURE_FLAG_BITS_MAX_ENUM : constant VkExternalSemaphoreFeatureFlagBits := 2147483647;  -- vulkan_core.h:4269

   subtype VkExternalSemaphoreFeatureFlags is VkFlags;  -- vulkan_core.h:4276

   type VkPhysicalDeviceSubgroupProperties is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4278
      pNext : System.Address;  -- vulkan_core.h:4279
      subgroupSize : aliased stdint_h.uint32_t;  -- vulkan_core.h:4280
      supportedStages : aliased VkShaderStageFlags;  -- vulkan_core.h:4281
      supportedOperations : aliased VkSubgroupFeatureFlags;  -- vulkan_core.h:4282
      quadOperationsInAllStages : aliased VkBool32;  -- vulkan_core.h:4283
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceSubgroupProperties);  -- vulkan_core.h:4277

   type VkBindBufferMemoryInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4287
      pNext : System.Address;  -- vulkan_core.h:4288
      buffer : VkBuffer;  -- vulkan_core.h:4289
      memory : VkDeviceMemory;  -- vulkan_core.h:4290
      memoryOffset : aliased VkDeviceSize;  -- vulkan_core.h:4291
   end record;
   pragma Convention (C_Pass_By_Copy, VkBindBufferMemoryInfo);  -- vulkan_core.h:4286

   type VkBindImageMemoryInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4295
      pNext : System.Address;  -- vulkan_core.h:4296
      image : VkImage;  -- vulkan_core.h:4297
      memory : VkDeviceMemory;  -- vulkan_core.h:4298
      memoryOffset : aliased VkDeviceSize;  -- vulkan_core.h:4299
   end record;
   pragma Convention (C_Pass_By_Copy, VkBindImageMemoryInfo);  -- vulkan_core.h:4294

   type VkPhysicalDevice16BitStorageFeatures is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4303
      pNext : System.Address;  -- vulkan_core.h:4304
      storageBuffer16BitAccess : aliased VkBool32;  -- vulkan_core.h:4305
      uniformAndStorageBuffer16BitAccess : aliased VkBool32;  -- vulkan_core.h:4306
      storagePushConstant16 : aliased VkBool32;  -- vulkan_core.h:4307
      storageInputOutput16 : aliased VkBool32;  -- vulkan_core.h:4308
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDevice16BitStorageFeatures);  -- vulkan_core.h:4302

   type VkMemoryDedicatedRequirements is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4312
      pNext : System.Address;  -- vulkan_core.h:4313
      prefersDedicatedAllocation : aliased VkBool32;  -- vulkan_core.h:4314
      requiresDedicatedAllocation : aliased VkBool32;  -- vulkan_core.h:4315
   end record;
   pragma Convention (C_Pass_By_Copy, VkMemoryDedicatedRequirements);  -- vulkan_core.h:4311

   type VkMemoryDedicatedAllocateInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4319
      pNext : System.Address;  -- vulkan_core.h:4320
      image : VkImage;  -- vulkan_core.h:4321
      buffer : VkBuffer;  -- vulkan_core.h:4322
   end record;
   pragma Convention (C_Pass_By_Copy, VkMemoryDedicatedAllocateInfo);  -- vulkan_core.h:4318

   type VkMemoryAllocateFlagsInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4326
      pNext : System.Address;  -- vulkan_core.h:4327
      flags : aliased VkMemoryAllocateFlags;  -- vulkan_core.h:4328
      deviceMask : aliased stdint_h.uint32_t;  -- vulkan_core.h:4329
   end record;
   pragma Convention (C_Pass_By_Copy, VkMemoryAllocateFlagsInfo);  -- vulkan_core.h:4325

   type VkDeviceGroupRenderPassBeginInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4333
      pNext : System.Address;  -- vulkan_core.h:4334
      deviceMask : aliased stdint_h.uint32_t;  -- vulkan_core.h:4335
      deviceRenderAreaCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:4336
      pDeviceRenderAreas : System.Address;  -- vulkan_core.h:4337
   end record;
   pragma Convention (C_Pass_By_Copy, VkDeviceGroupRenderPassBeginInfo);  -- vulkan_core.h:4332

   type VkDeviceGroupCommandBufferBeginInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4341
      pNext : System.Address;  -- vulkan_core.h:4342
      deviceMask : aliased stdint_h.uint32_t;  -- vulkan_core.h:4343
   end record;
   pragma Convention (C_Pass_By_Copy, VkDeviceGroupCommandBufferBeginInfo);  -- vulkan_core.h:4340

   type VkDeviceGroupSubmitInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4347
      pNext : System.Address;  -- vulkan_core.h:4348
      waitSemaphoreCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:4349
      pWaitSemaphoreDeviceIndices : access stdint_h.uint32_t;  -- vulkan_core.h:4350
      commandBufferCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:4351
      pCommandBufferDeviceMasks : access stdint_h.uint32_t;  -- vulkan_core.h:4352
      signalSemaphoreCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:4353
      pSignalSemaphoreDeviceIndices : access stdint_h.uint32_t;  -- vulkan_core.h:4354
   end record;
   pragma Convention (C_Pass_By_Copy, VkDeviceGroupSubmitInfo);  -- vulkan_core.h:4346

   type VkDeviceGroupBindSparseInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4358
      pNext : System.Address;  -- vulkan_core.h:4359
      resourceDeviceIndex : aliased stdint_h.uint32_t;  -- vulkan_core.h:4360
      memoryDeviceIndex : aliased stdint_h.uint32_t;  -- vulkan_core.h:4361
   end record;
   pragma Convention (C_Pass_By_Copy, VkDeviceGroupBindSparseInfo);  -- vulkan_core.h:4357

   type VkBindBufferMemoryDeviceGroupInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4365
      pNext : System.Address;  -- vulkan_core.h:4366
      deviceIndexCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:4367
      pDeviceIndices : access stdint_h.uint32_t;  -- vulkan_core.h:4368
   end record;
   pragma Convention (C_Pass_By_Copy, VkBindBufferMemoryDeviceGroupInfo);  -- vulkan_core.h:4364

   type VkBindImageMemoryDeviceGroupInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4372
      pNext : System.Address;  -- vulkan_core.h:4373
      deviceIndexCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:4374
      pDeviceIndices : access stdint_h.uint32_t;  -- vulkan_core.h:4375
      splitInstanceBindRegionCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:4376
      pSplitInstanceBindRegions : System.Address;  -- vulkan_core.h:4377
   end record;
   pragma Convention (C_Pass_By_Copy, VkBindImageMemoryDeviceGroupInfo);  -- vulkan_core.h:4371

   type VkPhysicalDeviceGroupProperties_physicalDevices_array is array (0 .. 31) of VkPhysicalDevice;
   type VkPhysicalDeviceGroupProperties is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4381
      pNext : System.Address;  -- vulkan_core.h:4382
      physicalDeviceCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:4383
      physicalDevices : aliased VkPhysicalDeviceGroupProperties_physicalDevices_array;  -- vulkan_core.h:4384
      subsetAllocation : aliased VkBool32;  -- vulkan_core.h:4385
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceGroupProperties);  -- vulkan_core.h:4380

   type VkDeviceGroupDeviceCreateInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4389
      pNext : System.Address;  -- vulkan_core.h:4390
      physicalDeviceCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:4391
      pPhysicalDevices : System.Address;  -- vulkan_core.h:4392
   end record;
   pragma Convention (C_Pass_By_Copy, VkDeviceGroupDeviceCreateInfo);  -- vulkan_core.h:4388

   type VkBufferMemoryRequirementsInfo2 is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4396
      pNext : System.Address;  -- vulkan_core.h:4397
      buffer : VkBuffer;  -- vulkan_core.h:4398
   end record;
   pragma Convention (C_Pass_By_Copy, VkBufferMemoryRequirementsInfo2);  -- vulkan_core.h:4395

   type VkImageMemoryRequirementsInfo2 is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4402
      pNext : System.Address;  -- vulkan_core.h:4403
      image : VkImage;  -- vulkan_core.h:4404
   end record;
   pragma Convention (C_Pass_By_Copy, VkImageMemoryRequirementsInfo2);  -- vulkan_core.h:4401

   type VkImageSparseMemoryRequirementsInfo2 is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4408
      pNext : System.Address;  -- vulkan_core.h:4409
      image : VkImage;  -- vulkan_core.h:4410
   end record;
   pragma Convention (C_Pass_By_Copy, VkImageSparseMemoryRequirementsInfo2);  -- vulkan_core.h:4407

   type VkMemoryRequirements2 is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4414
      pNext : System.Address;  -- vulkan_core.h:4415
      memoryRequirements : aliased VkMemoryRequirements;  -- vulkan_core.h:4416
   end record;
   pragma Convention (C_Pass_By_Copy, VkMemoryRequirements2);  -- vulkan_core.h:4413

   type VkSparseImageMemoryRequirements2 is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4420
      pNext : System.Address;  -- vulkan_core.h:4421
      memoryRequirements : aliased VkSparseImageMemoryRequirements;  -- vulkan_core.h:4422
   end record;
   pragma Convention (C_Pass_By_Copy, VkSparseImageMemoryRequirements2);  -- vulkan_core.h:4419

   type VkPhysicalDeviceFeatures2 is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4426
      pNext : System.Address;  -- vulkan_core.h:4427
      features : aliased VkPhysicalDeviceFeatures;  -- vulkan_core.h:4428
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceFeatures2);  -- vulkan_core.h:4425

   type VkPhysicalDeviceProperties2 is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4432
      pNext : System.Address;  -- vulkan_core.h:4433
      properties : aliased VkPhysicalDeviceProperties;  -- vulkan_core.h:4434
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceProperties2);  -- vulkan_core.h:4431

   type VkFormatProperties2 is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4438
      pNext : System.Address;  -- vulkan_core.h:4439
      formatProperties : aliased VkFormatProperties;  -- vulkan_core.h:4440
   end record;
   pragma Convention (C_Pass_By_Copy, VkFormatProperties2);  -- vulkan_core.h:4437

   type VkImageFormatProperties2 is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4444
      pNext : System.Address;  -- vulkan_core.h:4445
      imageFormatProperties : aliased VkImageFormatProperties;  -- vulkan_core.h:4446
   end record;
   pragma Convention (C_Pass_By_Copy, VkImageFormatProperties2);  -- vulkan_core.h:4443

   type VkPhysicalDeviceImageFormatInfo2 is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4450
      pNext : System.Address;  -- vulkan_core.h:4451
      format : aliased VkFormat;  -- vulkan_core.h:4452
      c_type : aliased VkImageType;  -- vulkan_core.h:4453
      tiling : aliased VkImageTiling;  -- vulkan_core.h:4454
      usage : aliased VkImageUsageFlags;  -- vulkan_core.h:4455
      flags : aliased VkImageCreateFlags;  -- vulkan_core.h:4456
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceImageFormatInfo2);  -- vulkan_core.h:4449

   type VkQueueFamilyProperties2 is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4460
      pNext : System.Address;  -- vulkan_core.h:4461
      queueFamilyProperties : aliased VkQueueFamilyProperties;  -- vulkan_core.h:4462
   end record;
   pragma Convention (C_Pass_By_Copy, VkQueueFamilyProperties2);  -- vulkan_core.h:4459

   type VkPhysicalDeviceMemoryProperties2 is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4466
      pNext : System.Address;  -- vulkan_core.h:4467
      memoryProperties : aliased VkPhysicalDeviceMemoryProperties;  -- vulkan_core.h:4468
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceMemoryProperties2);  -- vulkan_core.h:4465

   type VkSparseImageFormatProperties2 is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4472
      pNext : System.Address;  -- vulkan_core.h:4473
      properties : aliased VkSparseImageFormatProperties;  -- vulkan_core.h:4474
   end record;
   pragma Convention (C_Pass_By_Copy, VkSparseImageFormatProperties2);  -- vulkan_core.h:4471

   type VkPhysicalDeviceSparseImageFormatInfo2 is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4478
      pNext : System.Address;  -- vulkan_core.h:4479
      format : aliased VkFormat;  -- vulkan_core.h:4480
      c_type : aliased VkImageType;  -- vulkan_core.h:4481
      samples : aliased VkSampleCountFlagBits;  -- vulkan_core.h:4482
      usage : aliased VkImageUsageFlags;  -- vulkan_core.h:4483
      tiling : aliased VkImageTiling;  -- vulkan_core.h:4484
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceSparseImageFormatInfo2);  -- vulkan_core.h:4477

   type VkPhysicalDevicePointClippingProperties is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4488
      pNext : System.Address;  -- vulkan_core.h:4489
      pointClippingBehavior : aliased VkPointClippingBehavior;  -- vulkan_core.h:4490
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDevicePointClippingProperties);  -- vulkan_core.h:4487

   type VkInputAttachmentAspectReference is record
      subpass : aliased stdint_h.uint32_t;  -- vulkan_core.h:4494
      inputAttachmentIndex : aliased stdint_h.uint32_t;  -- vulkan_core.h:4495
      aspectMask : aliased VkImageAspectFlags;  -- vulkan_core.h:4496
   end record;
   pragma Convention (C_Pass_By_Copy, VkInputAttachmentAspectReference);  -- vulkan_core.h:4493

   type VkRenderPassInputAttachmentAspectCreateInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4500
      pNext : System.Address;  -- vulkan_core.h:4501
      aspectReferenceCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:4502
      pAspectReferences : System.Address;  -- vulkan_core.h:4503
   end record;
   pragma Convention (C_Pass_By_Copy, VkRenderPassInputAttachmentAspectCreateInfo);  -- vulkan_core.h:4499

   type VkImageViewUsageCreateInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4507
      pNext : System.Address;  -- vulkan_core.h:4508
      usage : aliased VkImageUsageFlags;  -- vulkan_core.h:4509
   end record;
   pragma Convention (C_Pass_By_Copy, VkImageViewUsageCreateInfo);  -- vulkan_core.h:4506

   type VkPipelineTessellationDomainOriginStateCreateInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4513
      pNext : System.Address;  -- vulkan_core.h:4514
      domainOrigin : aliased VkTessellationDomainOrigin;  -- vulkan_core.h:4515
   end record;
   pragma Convention (C_Pass_By_Copy, VkPipelineTessellationDomainOriginStateCreateInfo);  -- vulkan_core.h:4512

   type VkRenderPassMultiviewCreateInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4519
      pNext : System.Address;  -- vulkan_core.h:4520
      subpassCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:4521
      pViewMasks : access stdint_h.uint32_t;  -- vulkan_core.h:4522
      dependencyCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:4523
      pViewOffsets : access stdint_h.int32_t;  -- vulkan_core.h:4524
      correlationMaskCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:4525
      pCorrelationMasks : access stdint_h.uint32_t;  -- vulkan_core.h:4526
   end record;
   pragma Convention (C_Pass_By_Copy, VkRenderPassMultiviewCreateInfo);  -- vulkan_core.h:4518

   type VkPhysicalDeviceMultiviewFeatures is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4530
      pNext : System.Address;  -- vulkan_core.h:4531
      multiview : aliased VkBool32;  -- vulkan_core.h:4532
      multiviewGeometryShader : aliased VkBool32;  -- vulkan_core.h:4533
      multiviewTessellationShader : aliased VkBool32;  -- vulkan_core.h:4534
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceMultiviewFeatures);  -- vulkan_core.h:4529

   type VkPhysicalDeviceMultiviewProperties is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4538
      pNext : System.Address;  -- vulkan_core.h:4539
      maxMultiviewViewCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:4540
      maxMultiviewInstanceIndex : aliased stdint_h.uint32_t;  -- vulkan_core.h:4541
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceMultiviewProperties);  -- vulkan_core.h:4537

   type VkPhysicalDeviceVariablePointersFeatures is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4545
      pNext : System.Address;  -- vulkan_core.h:4546
      variablePointersStorageBuffer : aliased VkBool32;  -- vulkan_core.h:4547
      variablePointers : aliased VkBool32;  -- vulkan_core.h:4548
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceVariablePointersFeatures);  -- vulkan_core.h:4544

   subtype VkPhysicalDeviceVariablePointerFeatures is VkPhysicalDeviceVariablePointersFeatures;

   type VkPhysicalDeviceProtectedMemoryFeatures is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4554
      pNext : System.Address;  -- vulkan_core.h:4555
      protectedMemory : aliased VkBool32;  -- vulkan_core.h:4556
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceProtectedMemoryFeatures);  -- vulkan_core.h:4553

   type VkPhysicalDeviceProtectedMemoryProperties is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4560
      pNext : System.Address;  -- vulkan_core.h:4561
      protectedNoFault : aliased VkBool32;  -- vulkan_core.h:4562
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceProtectedMemoryProperties);  -- vulkan_core.h:4559

   type VkDeviceQueueInfo2 is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4566
      pNext : System.Address;  -- vulkan_core.h:4567
      flags : aliased VkDeviceQueueCreateFlags;  -- vulkan_core.h:4568
      queueFamilyIndex : aliased stdint_h.uint32_t;  -- vulkan_core.h:4569
      queueIndex : aliased stdint_h.uint32_t;  -- vulkan_core.h:4570
   end record;
   pragma Convention (C_Pass_By_Copy, VkDeviceQueueInfo2);  -- vulkan_core.h:4565

   type VkProtectedSubmitInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4574
      pNext : System.Address;  -- vulkan_core.h:4575
      protectedSubmit : aliased VkBool32;  -- vulkan_core.h:4576
   end record;
   pragma Convention (C_Pass_By_Copy, VkProtectedSubmitInfo);  -- vulkan_core.h:4573

   type VkSamplerYcbcrConversionCreateInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4580
      pNext : System.Address;  -- vulkan_core.h:4581
      format : aliased VkFormat;  -- vulkan_core.h:4582
      ycbcrModel : aliased VkSamplerYcbcrModelConversion;  -- vulkan_core.h:4583
      ycbcrRange : aliased VkSamplerYcbcrRange;  -- vulkan_core.h:4584
      components : aliased VkComponentMapping;  -- vulkan_core.h:4585
      xChromaOffset : aliased VkChromaLocation;  -- vulkan_core.h:4586
      yChromaOffset : aliased VkChromaLocation;  -- vulkan_core.h:4587
      chromaFilter : aliased VkFilter;  -- vulkan_core.h:4588
      forceExplicitReconstruction : aliased VkBool32;  -- vulkan_core.h:4589
   end record;
   pragma Convention (C_Pass_By_Copy, VkSamplerYcbcrConversionCreateInfo);  -- vulkan_core.h:4579

   type VkSamplerYcbcrConversionInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4593
      pNext : System.Address;  -- vulkan_core.h:4594
      conversion : VkSamplerYcbcrConversion;  -- vulkan_core.h:4595
   end record;
   pragma Convention (C_Pass_By_Copy, VkSamplerYcbcrConversionInfo);  -- vulkan_core.h:4592

   type VkBindImagePlaneMemoryInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4599
      pNext : System.Address;  -- vulkan_core.h:4600
      planeAspect : aliased VkImageAspectFlagBits;  -- vulkan_core.h:4601
   end record;
   pragma Convention (C_Pass_By_Copy, VkBindImagePlaneMemoryInfo);  -- vulkan_core.h:4598

   type VkImagePlaneMemoryRequirementsInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4605
      pNext : System.Address;  -- vulkan_core.h:4606
      planeAspect : aliased VkImageAspectFlagBits;  -- vulkan_core.h:4607
   end record;
   pragma Convention (C_Pass_By_Copy, VkImagePlaneMemoryRequirementsInfo);  -- vulkan_core.h:4604

   type VkPhysicalDeviceSamplerYcbcrConversionFeatures is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4611
      pNext : System.Address;  -- vulkan_core.h:4612
      samplerYcbcrConversion : aliased VkBool32;  -- vulkan_core.h:4613
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceSamplerYcbcrConversionFeatures);  -- vulkan_core.h:4610

   type VkSamplerYcbcrConversionImageFormatProperties is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4617
      pNext : System.Address;  -- vulkan_core.h:4618
      combinedImageSamplerDescriptorCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:4619
   end record;
   pragma Convention (C_Pass_By_Copy, VkSamplerYcbcrConversionImageFormatProperties);  -- vulkan_core.h:4616

   type VkDescriptorUpdateTemplateEntry is record
      dstBinding : aliased stdint_h.uint32_t;  -- vulkan_core.h:4623
      dstArrayElement : aliased stdint_h.uint32_t;  -- vulkan_core.h:4624
      descriptorCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:4625
      descriptorType : aliased VkDescriptorType;  -- vulkan_core.h:4626
      offset : aliased crtdefs_h.size_t;  -- vulkan_core.h:4627
      stride : aliased crtdefs_h.size_t;  -- vulkan_core.h:4628
   end record;
   pragma Convention (C_Pass_By_Copy, VkDescriptorUpdateTemplateEntry);  -- vulkan_core.h:4622

   type VkDescriptorUpdateTemplateCreateInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4632
      pNext : System.Address;  -- vulkan_core.h:4633
      flags : aliased VkDescriptorUpdateTemplateCreateFlags;  -- vulkan_core.h:4634
      descriptorUpdateEntryCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:4635
      pDescriptorUpdateEntries : System.Address;  -- vulkan_core.h:4636
      templateType : aliased VkDescriptorUpdateTemplateType;  -- vulkan_core.h:4637
      descriptorSetLayout : VkDescriptorSetLayout;  -- vulkan_core.h:4638
      pipelineBindPoint : aliased VkPipelineBindPoint;  -- vulkan_core.h:4639
      pipelineLayout : VkPipelineLayout;  -- vulkan_core.h:4640
      set : aliased stdint_h.uint32_t;  -- vulkan_core.h:4641
   end record;
   pragma Convention (C_Pass_By_Copy, VkDescriptorUpdateTemplateCreateInfo);  -- vulkan_core.h:4631

   type VkExternalMemoryProperties is record
      externalMemoryFeatures : aliased VkExternalMemoryFeatureFlags;  -- vulkan_core.h:4645
      exportFromImportedHandleTypes : aliased VkExternalMemoryHandleTypeFlags;  -- vulkan_core.h:4646
      compatibleHandleTypes : aliased VkExternalMemoryHandleTypeFlags;  -- vulkan_core.h:4647
   end record;
   pragma Convention (C_Pass_By_Copy, VkExternalMemoryProperties);  -- vulkan_core.h:4644

   type VkPhysicalDeviceExternalImageFormatInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4651
      pNext : System.Address;  -- vulkan_core.h:4652
      handleType : aliased VkExternalMemoryHandleTypeFlagBits;  -- vulkan_core.h:4653
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceExternalImageFormatInfo);  -- vulkan_core.h:4650

   type VkExternalImageFormatProperties is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4657
      pNext : System.Address;  -- vulkan_core.h:4658
      externalMemoryProperties : aliased VkExternalMemoryProperties;  -- vulkan_core.h:4659
   end record;
   pragma Convention (C_Pass_By_Copy, VkExternalImageFormatProperties);  -- vulkan_core.h:4656

   type VkPhysicalDeviceExternalBufferInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4663
      pNext : System.Address;  -- vulkan_core.h:4664
      flags : aliased VkBufferCreateFlags;  -- vulkan_core.h:4665
      usage : aliased VkBufferUsageFlags;  -- vulkan_core.h:4666
      handleType : aliased VkExternalMemoryHandleTypeFlagBits;  -- vulkan_core.h:4667
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceExternalBufferInfo);  -- vulkan_core.h:4662

   type VkExternalBufferProperties is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4671
      pNext : System.Address;  -- vulkan_core.h:4672
      externalMemoryProperties : aliased VkExternalMemoryProperties;  -- vulkan_core.h:4673
   end record;
   pragma Convention (C_Pass_By_Copy, VkExternalBufferProperties);  -- vulkan_core.h:4670

   type VkPhysicalDeviceIDProperties_deviceUUID_array is array (0 .. 15) of aliased stdint_h.uint8_t;
   type VkPhysicalDeviceIDProperties_driverUUID_array is array (0 .. 15) of aliased stdint_h.uint8_t;
   type VkPhysicalDeviceIDProperties_deviceLUID_array is array (0 .. 7) of aliased stdint_h.uint8_t;
   type VkPhysicalDeviceIDProperties is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4677
      pNext : System.Address;  -- vulkan_core.h:4678
      deviceUUID : aliased VkPhysicalDeviceIDProperties_deviceUUID_array;  -- vulkan_core.h:4679
      driverUUID : aliased VkPhysicalDeviceIDProperties_driverUUID_array;  -- vulkan_core.h:4680
      deviceLUID : aliased VkPhysicalDeviceIDProperties_deviceLUID_array;  -- vulkan_core.h:4681
      deviceNodeMask : aliased stdint_h.uint32_t;  -- vulkan_core.h:4682
      deviceLUIDValid : aliased VkBool32;  -- vulkan_core.h:4683
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceIDProperties);  -- vulkan_core.h:4676

   type VkExternalMemoryImageCreateInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4687
      pNext : System.Address;  -- vulkan_core.h:4688
      handleTypes : aliased VkExternalMemoryHandleTypeFlags;  -- vulkan_core.h:4689
   end record;
   pragma Convention (C_Pass_By_Copy, VkExternalMemoryImageCreateInfo);  -- vulkan_core.h:4686

   type VkExternalMemoryBufferCreateInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4693
      pNext : System.Address;  -- vulkan_core.h:4694
      handleTypes : aliased VkExternalMemoryHandleTypeFlags;  -- vulkan_core.h:4695
   end record;
   pragma Convention (C_Pass_By_Copy, VkExternalMemoryBufferCreateInfo);  -- vulkan_core.h:4692

   type VkExportMemoryAllocateInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4699
      pNext : System.Address;  -- vulkan_core.h:4700
      handleTypes : aliased VkExternalMemoryHandleTypeFlags;  -- vulkan_core.h:4701
   end record;
   pragma Convention (C_Pass_By_Copy, VkExportMemoryAllocateInfo);  -- vulkan_core.h:4698

   type VkPhysicalDeviceExternalFenceInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4705
      pNext : System.Address;  -- vulkan_core.h:4706
      handleType : aliased VkExternalFenceHandleTypeFlagBits;  -- vulkan_core.h:4707
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceExternalFenceInfo);  -- vulkan_core.h:4704

   type VkExternalFenceProperties is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4711
      pNext : System.Address;  -- vulkan_core.h:4712
      exportFromImportedHandleTypes : aliased VkExternalFenceHandleTypeFlags;  -- vulkan_core.h:4713
      compatibleHandleTypes : aliased VkExternalFenceHandleTypeFlags;  -- vulkan_core.h:4714
      externalFenceFeatures : aliased VkExternalFenceFeatureFlags;  -- vulkan_core.h:4715
   end record;
   pragma Convention (C_Pass_By_Copy, VkExternalFenceProperties);  -- vulkan_core.h:4710

   type VkExportFenceCreateInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4719
      pNext : System.Address;  -- vulkan_core.h:4720
      handleTypes : aliased VkExternalFenceHandleTypeFlags;  -- vulkan_core.h:4721
   end record;
   pragma Convention (C_Pass_By_Copy, VkExportFenceCreateInfo);  -- vulkan_core.h:4718

   type VkExportSemaphoreCreateInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4725
      pNext : System.Address;  -- vulkan_core.h:4726
      handleTypes : aliased VkExternalSemaphoreHandleTypeFlags;  -- vulkan_core.h:4727
   end record;
   pragma Convention (C_Pass_By_Copy, VkExportSemaphoreCreateInfo);  -- vulkan_core.h:4724

   type VkPhysicalDeviceExternalSemaphoreInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4731
      pNext : System.Address;  -- vulkan_core.h:4732
      handleType : aliased VkExternalSemaphoreHandleTypeFlagBits;  -- vulkan_core.h:4733
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceExternalSemaphoreInfo);  -- vulkan_core.h:4730

   type VkExternalSemaphoreProperties is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4737
      pNext : System.Address;  -- vulkan_core.h:4738
      exportFromImportedHandleTypes : aliased VkExternalSemaphoreHandleTypeFlags;  -- vulkan_core.h:4739
      compatibleHandleTypes : aliased VkExternalSemaphoreHandleTypeFlags;  -- vulkan_core.h:4740
      externalSemaphoreFeatures : aliased VkExternalSemaphoreFeatureFlags;  -- vulkan_core.h:4741
   end record;
   pragma Convention (C_Pass_By_Copy, VkExternalSemaphoreProperties);  -- vulkan_core.h:4736

   type VkPhysicalDeviceMaintenance3Properties is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4745
      pNext : System.Address;  -- vulkan_core.h:4746
      maxPerSetDescriptors : aliased stdint_h.uint32_t;  -- vulkan_core.h:4747
      maxMemoryAllocationSize : aliased VkDeviceSize;  -- vulkan_core.h:4748
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceMaintenance3Properties);  -- vulkan_core.h:4744

   type VkDescriptorSetLayoutSupport is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4752
      pNext : System.Address;  -- vulkan_core.h:4753
      supported : aliased VkBool32;  -- vulkan_core.h:4754
   end record;
   pragma Convention (C_Pass_By_Copy, VkDescriptorSetLayoutSupport);  -- vulkan_core.h:4751

   type VkPhysicalDeviceShaderDrawParametersFeatures is record
      sType : aliased VkStructureType;  -- vulkan_core.h:4758
      pNext : System.Address;  -- vulkan_core.h:4759
      shaderDrawParameters : aliased VkBool32;  -- vulkan_core.h:4760
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceShaderDrawParametersFeatures);  -- vulkan_core.h:4757

   subtype VkPhysicalDeviceShaderDrawParameterFeatures is VkPhysicalDeviceShaderDrawParametersFeatures;

   type PFN_vkEnumerateInstanceVersion is access function (arg1 : access stdint_h.uint32_t) return VkResult;
   pragma Convention (C, PFN_vkEnumerateInstanceVersion);  -- vulkan_core.h:4765

   type PFN_vkBindBufferMemory2 is access function
        (arg1 : VkDevice;
         arg2 : stdint_h.uint32_t;
         arg3 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkBindBufferMemory2);  -- vulkan_core.h:4766

   type PFN_vkBindImageMemory2 is access function
        (arg1 : VkDevice;
         arg2 : stdint_h.uint32_t;
         arg3 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkBindImageMemory2);  -- vulkan_core.h:4767

   type PFN_vkGetDeviceGroupPeerMemoryFeatures is access procedure
        (arg1 : VkDevice;
         arg2 : stdint_h.uint32_t;
         arg3 : stdint_h.uint32_t;
         arg4 : stdint_h.uint32_t;
         arg5 : access VkPeerMemoryFeatureFlags);
   pragma Convention (C, PFN_vkGetDeviceGroupPeerMemoryFeatures);  -- vulkan_core.h:4768

   type PFN_vkCmdSetDeviceMask is access procedure (arg1 : VkCommandBuffer; arg2 : stdint_h.uint32_t);
   pragma Convention (C, PFN_vkCmdSetDeviceMask);  -- vulkan_core.h:4769

   type PFN_vkCmdDispatchBase is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : stdint_h.uint32_t;
         arg3 : stdint_h.uint32_t;
         arg4 : stdint_h.uint32_t;
         arg5 : stdint_h.uint32_t;
         arg6 : stdint_h.uint32_t;
         arg7 : stdint_h.uint32_t);
   pragma Convention (C, PFN_vkCmdDispatchBase);  -- vulkan_core.h:4770

   type PFN_vkEnumeratePhysicalDeviceGroups is access function
        (arg1 : VkInstance;
         arg2 : access stdint_h.uint32_t;
         arg3 : access VkPhysicalDeviceGroupProperties) return VkResult;
   pragma Convention (C, PFN_vkEnumeratePhysicalDeviceGroups);  -- vulkan_core.h:4771

   type PFN_vkGetImageMemoryRequirements2 is access procedure
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : access VkMemoryRequirements2);
   pragma Convention (C, PFN_vkGetImageMemoryRequirements2);  -- vulkan_core.h:4772

   type PFN_vkGetBufferMemoryRequirements2 is access procedure
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : access VkMemoryRequirements2);
   pragma Convention (C, PFN_vkGetBufferMemoryRequirements2);  -- vulkan_core.h:4773

   type PFN_vkGetImageSparseMemoryRequirements2 is access procedure
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : access stdint_h.uint32_t;
         arg4 : access VkSparseImageMemoryRequirements2);
   pragma Convention (C, PFN_vkGetImageSparseMemoryRequirements2);  -- vulkan_core.h:4774

   type PFN_vkGetPhysicalDeviceFeatures2 is access procedure (arg1 : VkPhysicalDevice; arg2 : access VkPhysicalDeviceFeatures2);
   pragma Convention (C, PFN_vkGetPhysicalDeviceFeatures2);  -- vulkan_core.h:4775

   type PFN_vkGetPhysicalDeviceProperties2 is access procedure (arg1 : VkPhysicalDevice; arg2 : access VkPhysicalDeviceProperties2);
   pragma Convention (C, PFN_vkGetPhysicalDeviceProperties2);  -- vulkan_core.h:4776

   type PFN_vkGetPhysicalDeviceFormatProperties2 is access procedure
        (arg1 : VkPhysicalDevice;
         arg2 : VkFormat;
         arg3 : access VkFormatProperties2);
   pragma Convention (C, PFN_vkGetPhysicalDeviceFormatProperties2);  -- vulkan_core.h:4777

   type PFN_vkGetPhysicalDeviceImageFormatProperties2 is access function
        (arg1 : VkPhysicalDevice;
         arg2 : System.Address;
         arg3 : access VkImageFormatProperties2) return VkResult;
   pragma Convention (C, PFN_vkGetPhysicalDeviceImageFormatProperties2);  -- vulkan_core.h:4778

   type PFN_vkGetPhysicalDeviceQueueFamilyProperties2 is access procedure
        (arg1 : VkPhysicalDevice;
         arg2 : access stdint_h.uint32_t;
         arg3 : access VkQueueFamilyProperties2);
   pragma Convention (C, PFN_vkGetPhysicalDeviceQueueFamilyProperties2);  -- vulkan_core.h:4779

   type PFN_vkGetPhysicalDeviceMemoryProperties2 is access procedure (arg1 : VkPhysicalDevice; arg2 : access VkPhysicalDeviceMemoryProperties2);
   pragma Convention (C, PFN_vkGetPhysicalDeviceMemoryProperties2);  -- vulkan_core.h:4780

   type PFN_vkGetPhysicalDeviceSparseImageFormatProperties2 is access procedure
        (arg1 : VkPhysicalDevice;
         arg2 : System.Address;
         arg3 : access stdint_h.uint32_t;
         arg4 : access VkSparseImageFormatProperties2);
   pragma Convention (C, PFN_vkGetPhysicalDeviceSparseImageFormatProperties2);  -- vulkan_core.h:4781

   type PFN_vkTrimCommandPool is access procedure
        (arg1 : VkDevice;
         arg2 : VkCommandPool;
         arg3 : VkCommandPoolTrimFlags);
   pragma Convention (C, PFN_vkTrimCommandPool);  -- vulkan_core.h:4782

   type PFN_vkGetDeviceQueue2 is access procedure
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkGetDeviceQueue2);  -- vulkan_core.h:4783

   type PFN_vkCreateSamplerYcbcrConversion is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : System.Address;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateSamplerYcbcrConversion);  -- vulkan_core.h:4784

   type PFN_vkDestroySamplerYcbcrConversion is access procedure
        (arg1 : VkDevice;
         arg2 : VkSamplerYcbcrConversion;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkDestroySamplerYcbcrConversion);  -- vulkan_core.h:4785

   type PFN_vkCreateDescriptorUpdateTemplate is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : System.Address;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateDescriptorUpdateTemplate);  -- vulkan_core.h:4786

   type PFN_vkDestroyDescriptorUpdateTemplate is access procedure
        (arg1 : VkDevice;
         arg2 : VkDescriptorUpdateTemplate;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkDestroyDescriptorUpdateTemplate);  -- vulkan_core.h:4787

   type PFN_vkUpdateDescriptorSetWithTemplate is access procedure
        (arg1 : VkDevice;
         arg2 : VkDescriptorSet;
         arg3 : VkDescriptorUpdateTemplate;
         arg4 : System.Address);
   pragma Convention (C, PFN_vkUpdateDescriptorSetWithTemplate);  -- vulkan_core.h:4788

   type PFN_vkGetPhysicalDeviceExternalBufferProperties is access procedure
        (arg1 : VkPhysicalDevice;
         arg2 : System.Address;
         arg3 : access VkExternalBufferProperties);
   pragma Convention (C, PFN_vkGetPhysicalDeviceExternalBufferProperties);  -- vulkan_core.h:4789

   type PFN_vkGetPhysicalDeviceExternalFenceProperties is access procedure
        (arg1 : VkPhysicalDevice;
         arg2 : System.Address;
         arg3 : access VkExternalFenceProperties);
   pragma Convention (C, PFN_vkGetPhysicalDeviceExternalFenceProperties);  -- vulkan_core.h:4790

   type PFN_vkGetPhysicalDeviceExternalSemaphoreProperties is access procedure
        (arg1 : VkPhysicalDevice;
         arg2 : System.Address;
         arg3 : access VkExternalSemaphoreProperties);
   pragma Convention (C, PFN_vkGetPhysicalDeviceExternalSemaphoreProperties);  -- vulkan_core.h:4791

   type PFN_vkGetDescriptorSetLayoutSupport is access procedure
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : access VkDescriptorSetLayoutSupport);
   pragma Convention (C, PFN_vkGetDescriptorSetLayoutSupport);  -- vulkan_core.h:4792

   function vkEnumerateInstanceVersion (pApiVersion : access stdint_h.uint32_t) return VkResult;  -- vulkan_core.h:4795
   pragma Import (C, vkEnumerateInstanceVersion, "vkEnumerateInstanceVersion");

   function vkBindBufferMemory2
     (device : VkDevice;
      bindInfoCount : stdint_h.uint32_t;
      pBindInfos : System.Address) return VkResult;  -- vulkan_core.h:4798
   pragma Import (C, vkBindBufferMemory2, "vkBindBufferMemory2");

   function vkBindImageMemory2
     (device : VkDevice;
      bindInfoCount : stdint_h.uint32_t;
      pBindInfos : System.Address) return VkResult;  -- vulkan_core.h:4803
   pragma Import (C, vkBindImageMemory2, "vkBindImageMemory2");

   procedure vkGetDeviceGroupPeerMemoryFeatures
     (device : VkDevice;
      heapIndex : stdint_h.uint32_t;
      localDeviceIndex : stdint_h.uint32_t;
      remoteDeviceIndex : stdint_h.uint32_t;
      pPeerMemoryFeatures : access VkPeerMemoryFeatureFlags);  -- vulkan_core.h:4808
   pragma Import (C, vkGetDeviceGroupPeerMemoryFeatures, "vkGetDeviceGroupPeerMemoryFeatures");

   procedure vkCmdSetDeviceMask (commandBuffer : VkCommandBuffer; deviceMask : stdint_h.uint32_t);  -- vulkan_core.h:4815
   pragma Import (C, vkCmdSetDeviceMask, "vkCmdSetDeviceMask");

   procedure vkCmdDispatchBase
     (commandBuffer : VkCommandBuffer;
      baseGroupX : stdint_h.uint32_t;
      baseGroupY : stdint_h.uint32_t;
      baseGroupZ : stdint_h.uint32_t;
      groupCountX : stdint_h.uint32_t;
      groupCountY : stdint_h.uint32_t;
      groupCountZ : stdint_h.uint32_t);  -- vulkan_core.h:4819
   pragma Import (C, vkCmdDispatchBase, "vkCmdDispatchBase");

   function vkEnumeratePhysicalDeviceGroups
     (instance : VkInstance;
      pPhysicalDeviceGroupCount : access stdint_h.uint32_t;
      pPhysicalDeviceGroupProperties : access VkPhysicalDeviceGroupProperties) return VkResult;  -- vulkan_core.h:4828
   pragma Import (C, vkEnumeratePhysicalDeviceGroups, "vkEnumeratePhysicalDeviceGroups");

   procedure vkGetImageMemoryRequirements2
     (device : VkDevice;
      pInfo : System.Address;
      pMemoryRequirements : access VkMemoryRequirements2);  -- vulkan_core.h:4833
   pragma Import (C, vkGetImageMemoryRequirements2, "vkGetImageMemoryRequirements2");

   procedure vkGetBufferMemoryRequirements2
     (device : VkDevice;
      pInfo : System.Address;
      pMemoryRequirements : access VkMemoryRequirements2);  -- vulkan_core.h:4838
   pragma Import (C, vkGetBufferMemoryRequirements2, "vkGetBufferMemoryRequirements2");

   procedure vkGetImageSparseMemoryRequirements2
     (device : VkDevice;
      pInfo : System.Address;
      pSparseMemoryRequirementCount : access stdint_h.uint32_t;
      pSparseMemoryRequirements : access VkSparseImageMemoryRequirements2);  -- vulkan_core.h:4843
   pragma Import (C, vkGetImageSparseMemoryRequirements2, "vkGetImageSparseMemoryRequirements2");

   procedure vkGetPhysicalDeviceFeatures2 (physicalDevice : VkPhysicalDevice; pFeatures : access VkPhysicalDeviceFeatures2);  -- vulkan_core.h:4849
   pragma Import (C, vkGetPhysicalDeviceFeatures2, "vkGetPhysicalDeviceFeatures2");

   procedure vkGetPhysicalDeviceProperties2 (physicalDevice : VkPhysicalDevice; pProperties : access VkPhysicalDeviceProperties2);  -- vulkan_core.h:4853
   pragma Import (C, vkGetPhysicalDeviceProperties2, "vkGetPhysicalDeviceProperties2");

   procedure vkGetPhysicalDeviceFormatProperties2
     (physicalDevice : VkPhysicalDevice;
      format : VkFormat;
      pFormatProperties : access VkFormatProperties2);  -- vulkan_core.h:4857
   pragma Import (C, vkGetPhysicalDeviceFormatProperties2, "vkGetPhysicalDeviceFormatProperties2");

   function vkGetPhysicalDeviceImageFormatProperties2
     (physicalDevice : VkPhysicalDevice;
      pImageFormatInfo : System.Address;
      pImageFormatProperties : access VkImageFormatProperties2) return VkResult;  -- vulkan_core.h:4862
   pragma Import (C, vkGetPhysicalDeviceImageFormatProperties2, "vkGetPhysicalDeviceImageFormatProperties2");

   procedure vkGetPhysicalDeviceQueueFamilyProperties2
     (physicalDevice : VkPhysicalDevice;
      pQueueFamilyPropertyCount : access stdint_h.uint32_t;
      pQueueFamilyProperties : access VkQueueFamilyProperties2);  -- vulkan_core.h:4867
   pragma Import (C, vkGetPhysicalDeviceQueueFamilyProperties2, "vkGetPhysicalDeviceQueueFamilyProperties2");

   procedure vkGetPhysicalDeviceMemoryProperties2 (physicalDevice : VkPhysicalDevice; pMemoryProperties : access VkPhysicalDeviceMemoryProperties2);  -- vulkan_core.h:4872
   pragma Import (C, vkGetPhysicalDeviceMemoryProperties2, "vkGetPhysicalDeviceMemoryProperties2");

   procedure vkGetPhysicalDeviceSparseImageFormatProperties2
     (physicalDevice : VkPhysicalDevice;
      pFormatInfo : System.Address;
      pPropertyCount : access stdint_h.uint32_t;
      pProperties : access VkSparseImageFormatProperties2);  -- vulkan_core.h:4876
   pragma Import (C, vkGetPhysicalDeviceSparseImageFormatProperties2, "vkGetPhysicalDeviceSparseImageFormatProperties2");

   procedure vkTrimCommandPool
     (device : VkDevice;
      commandPool : VkCommandPool;
      flags : VkCommandPoolTrimFlags);  -- vulkan_core.h:4882
   pragma Import (C, vkTrimCommandPool, "vkTrimCommandPool");

   procedure vkGetDeviceQueue2
     (device : VkDevice;
      pQueueInfo : System.Address;
      pQueue : System.Address);  -- vulkan_core.h:4887
   pragma Import (C, vkGetDeviceQueue2, "vkGetDeviceQueue2");

   function vkCreateSamplerYcbcrConversion
     (device : VkDevice;
      pCreateInfo : System.Address;
      pAllocator : System.Address;
      pYcbcrConversion : System.Address) return VkResult;  -- vulkan_core.h:4892
   pragma Import (C, vkCreateSamplerYcbcrConversion, "vkCreateSamplerYcbcrConversion");

   procedure vkDestroySamplerYcbcrConversion
     (device : VkDevice;
      ycbcrConversion : VkSamplerYcbcrConversion;
      pAllocator : System.Address);  -- vulkan_core.h:4898
   pragma Import (C, vkDestroySamplerYcbcrConversion, "vkDestroySamplerYcbcrConversion");

   function vkCreateDescriptorUpdateTemplate
     (device : VkDevice;
      pCreateInfo : System.Address;
      pAllocator : System.Address;
      pDescriptorUpdateTemplate : System.Address) return VkResult;  -- vulkan_core.h:4903
   pragma Import (C, vkCreateDescriptorUpdateTemplate, "vkCreateDescriptorUpdateTemplate");

   procedure vkDestroyDescriptorUpdateTemplate
     (device : VkDevice;
      descriptorUpdateTemplate : VkDescriptorUpdateTemplate;
      pAllocator : System.Address);  -- vulkan_core.h:4909
   pragma Import (C, vkDestroyDescriptorUpdateTemplate, "vkDestroyDescriptorUpdateTemplate");

   procedure vkUpdateDescriptorSetWithTemplate
     (device : VkDevice;
      descriptorSet : VkDescriptorSet;
      descriptorUpdateTemplate : VkDescriptorUpdateTemplate;
      pData : System.Address);  -- vulkan_core.h:4914
   pragma Import (C, vkUpdateDescriptorSetWithTemplate, "vkUpdateDescriptorSetWithTemplate");

   procedure vkGetPhysicalDeviceExternalBufferProperties
     (physicalDevice : VkPhysicalDevice;
      pExternalBufferInfo : System.Address;
      pExternalBufferProperties : access VkExternalBufferProperties);  -- vulkan_core.h:4920
   pragma Import (C, vkGetPhysicalDeviceExternalBufferProperties, "vkGetPhysicalDeviceExternalBufferProperties");

   procedure vkGetPhysicalDeviceExternalFenceProperties
     (physicalDevice : VkPhysicalDevice;
      pExternalFenceInfo : System.Address;
      pExternalFenceProperties : access VkExternalFenceProperties);  -- vulkan_core.h:4925
   pragma Import (C, vkGetPhysicalDeviceExternalFenceProperties, "vkGetPhysicalDeviceExternalFenceProperties");

   procedure vkGetPhysicalDeviceExternalSemaphoreProperties
     (physicalDevice : VkPhysicalDevice;
      pExternalSemaphoreInfo : System.Address;
      pExternalSemaphoreProperties : access VkExternalSemaphoreProperties);  -- vulkan_core.h:4930
   pragma Import (C, vkGetPhysicalDeviceExternalSemaphoreProperties, "vkGetPhysicalDeviceExternalSemaphoreProperties");

   procedure vkGetDescriptorSetLayoutSupport
     (device : VkDevice;
      pCreateInfo : System.Address;
      pSupport : access VkDescriptorSetLayoutSupport);  -- vulkan_core.h:4935
   pragma Import (C, vkGetDescriptorSetLayoutSupport, "vkGetDescriptorSetLayoutSupport");

  -- Vulkan 1.2 version number
   subtype VkDriverId is unsigned;
   VK_DRIVER_ID_AMD_PROPRIETARY : constant VkDriverId := 1;
   VK_DRIVER_ID_AMD_OPEN_SOURCE : constant VkDriverId := 2;
   VK_DRIVER_ID_MESA_RADV : constant VkDriverId := 3;
   VK_DRIVER_ID_NVIDIA_PROPRIETARY : constant VkDriverId := 4;
   VK_DRIVER_ID_INTEL_PROPRIETARY_WINDOWS : constant VkDriverId := 5;
   VK_DRIVER_ID_INTEL_OPEN_SOURCE_MESA : constant VkDriverId := 6;
   VK_DRIVER_ID_IMAGINATION_PROPRIETARY : constant VkDriverId := 7;
   VK_DRIVER_ID_QUALCOMM_PROPRIETARY : constant VkDriverId := 8;
   VK_DRIVER_ID_ARM_PROPRIETARY : constant VkDriverId := 9;
   VK_DRIVER_ID_GOOGLE_SWIFTSHADER : constant VkDriverId := 10;
   VK_DRIVER_ID_GGP_PROPRIETARY : constant VkDriverId := 11;
   VK_DRIVER_ID_BROADCOM_PROPRIETARY : constant VkDriverId := 12;
   VK_DRIVER_ID_MESA_LLVMPIPE : constant VkDriverId := 13;
   VK_DRIVER_ID_MOLTENVK : constant VkDriverId := 14;
   VK_DRIVER_ID_AMD_PROPRIETARY_KHR : constant VkDriverId := 1;
   VK_DRIVER_ID_AMD_OPEN_SOURCE_KHR : constant VkDriverId := 2;
   VK_DRIVER_ID_MESA_RADV_KHR : constant VkDriverId := 3;
   VK_DRIVER_ID_NVIDIA_PROPRIETARY_KHR : constant VkDriverId := 4;
   VK_DRIVER_ID_INTEL_PROPRIETARY_WINDOWS_KHR : constant VkDriverId := 5;
   VK_DRIVER_ID_INTEL_OPEN_SOURCE_MESA_KHR : constant VkDriverId := 6;
   VK_DRIVER_ID_IMAGINATION_PROPRIETARY_KHR : constant VkDriverId := 7;
   VK_DRIVER_ID_QUALCOMM_PROPRIETARY_KHR : constant VkDriverId := 8;
   VK_DRIVER_ID_ARM_PROPRIETARY_KHR : constant VkDriverId := 9;
   VK_DRIVER_ID_GOOGLE_SWIFTSHADER_KHR : constant VkDriverId := 10;
   VK_DRIVER_ID_GGP_PROPRIETARY_KHR : constant VkDriverId := 11;
   VK_DRIVER_ID_BROADCOM_PROPRIETARY_KHR : constant VkDriverId := 12;
   VK_DRIVER_ID_MAX_ENUM : constant VkDriverId := 2147483647;  -- vulkan_core.h:4949

   subtype VkShaderFloatControlsIndependence is unsigned;
   VK_SHADER_FLOAT_CONTROLS_INDEPENDENCE_32_BIT_ONLY : constant VkShaderFloatControlsIndependence := 0;
   VK_SHADER_FLOAT_CONTROLS_INDEPENDENCE_ALL : constant VkShaderFloatControlsIndependence := 1;
   VK_SHADER_FLOAT_CONTROLS_INDEPENDENCE_NONE : constant VkShaderFloatControlsIndependence := 2;
   VK_SHADER_FLOAT_CONTROLS_INDEPENDENCE_32_BIT_ONLY_KHR : constant VkShaderFloatControlsIndependence := 0;
   VK_SHADER_FLOAT_CONTROLS_INDEPENDENCE_ALL_KHR : constant VkShaderFloatControlsIndependence := 1;
   VK_SHADER_FLOAT_CONTROLS_INDEPENDENCE_NONE_KHR : constant VkShaderFloatControlsIndependence := 2;
   VK_SHADER_FLOAT_CONTROLS_INDEPENDENCE_MAX_ENUM : constant VkShaderFloatControlsIndependence := 2147483647;  -- vulkan_core.h:4979

   subtype VkSamplerReductionMode is unsigned;
   VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE : constant VkSamplerReductionMode := 0;
   VK_SAMPLER_REDUCTION_MODE_MIN : constant VkSamplerReductionMode := 1;
   VK_SAMPLER_REDUCTION_MODE_MAX : constant VkSamplerReductionMode := 2;
   VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT : constant VkSamplerReductionMode := 0;
   VK_SAMPLER_REDUCTION_MODE_MIN_EXT : constant VkSamplerReductionMode := 1;
   VK_SAMPLER_REDUCTION_MODE_MAX_EXT : constant VkSamplerReductionMode := 2;
   VK_SAMPLER_REDUCTION_MODE_MAX_ENUM : constant VkSamplerReductionMode := 2147483647;  -- vulkan_core.h:4989

   subtype VkSemaphoreType is unsigned;
   VK_SEMAPHORE_TYPE_BINARY : constant VkSemaphoreType := 0;
   VK_SEMAPHORE_TYPE_TIMELINE : constant VkSemaphoreType := 1;
   VK_SEMAPHORE_TYPE_BINARY_KHR : constant VkSemaphoreType := 0;
   VK_SEMAPHORE_TYPE_TIMELINE_KHR : constant VkSemaphoreType := 1;
   VK_SEMAPHORE_TYPE_MAX_ENUM : constant VkSemaphoreType := 2147483647;  -- vulkan_core.h:4999

   subtype VkResolveModeFlagBits is unsigned;
   VK_RESOLVE_MODE_NONE : constant VkResolveModeFlagBits := 0;
   VK_RESOLVE_MODE_SAMPLE_ZERO_BIT : constant VkResolveModeFlagBits := 1;
   VK_RESOLVE_MODE_AVERAGE_BIT : constant VkResolveModeFlagBits := 2;
   VK_RESOLVE_MODE_MIN_BIT : constant VkResolveModeFlagBits := 4;
   VK_RESOLVE_MODE_MAX_BIT : constant VkResolveModeFlagBits := 8;
   VK_RESOLVE_MODE_NONE_KHR : constant VkResolveModeFlagBits := 0;
   VK_RESOLVE_MODE_SAMPLE_ZERO_BIT_KHR : constant VkResolveModeFlagBits := 1;
   VK_RESOLVE_MODE_AVERAGE_BIT_KHR : constant VkResolveModeFlagBits := 2;
   VK_RESOLVE_MODE_MIN_BIT_KHR : constant VkResolveModeFlagBits := 4;
   VK_RESOLVE_MODE_MAX_BIT_KHR : constant VkResolveModeFlagBits := 8;
   VK_RESOLVE_MODE_FLAG_BITS_MAX_ENUM : constant VkResolveModeFlagBits := 2147483647;  -- vulkan_core.h:5007

   subtype VkResolveModeFlags is VkFlags;  -- vulkan_core.h:5020

   subtype VkDescriptorBindingFlagBits is unsigned;
   VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT : constant VkDescriptorBindingFlagBits := 1;
   VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT : constant VkDescriptorBindingFlagBits := 2;
   VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT : constant VkDescriptorBindingFlagBits := 4;
   VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT : constant VkDescriptorBindingFlagBits := 8;
   VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT : constant VkDescriptorBindingFlagBits := 1;
   VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT : constant VkDescriptorBindingFlagBits := 2;
   VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT : constant VkDescriptorBindingFlagBits := 4;
   VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT : constant VkDescriptorBindingFlagBits := 8;
   VK_DESCRIPTOR_BINDING_FLAG_BITS_MAX_ENUM : constant VkDescriptorBindingFlagBits := 2147483647;  -- vulkan_core.h:5022

   subtype VkDescriptorBindingFlags is VkFlags;  -- vulkan_core.h:5033

   subtype VkSemaphoreWaitFlagBits is unsigned;
   VK_SEMAPHORE_WAIT_ANY_BIT : constant VkSemaphoreWaitFlagBits := 1;
   VK_SEMAPHORE_WAIT_ANY_BIT_KHR : constant VkSemaphoreWaitFlagBits := 1;
   VK_SEMAPHORE_WAIT_FLAG_BITS_MAX_ENUM : constant VkSemaphoreWaitFlagBits := 2147483647;  -- vulkan_core.h:5035

   subtype VkSemaphoreWaitFlags is VkFlags;  -- vulkan_core.h:5040

   type VkPhysicalDeviceVulkan11Features is record
      sType : aliased VkStructureType;  -- vulkan_core.h:5042
      pNext : System.Address;  -- vulkan_core.h:5043
      storageBuffer16BitAccess : aliased VkBool32;  -- vulkan_core.h:5044
      uniformAndStorageBuffer16BitAccess : aliased VkBool32;  -- vulkan_core.h:5045
      storagePushConstant16 : aliased VkBool32;  -- vulkan_core.h:5046
      storageInputOutput16 : aliased VkBool32;  -- vulkan_core.h:5047
      multiview : aliased VkBool32;  -- vulkan_core.h:5048
      multiviewGeometryShader : aliased VkBool32;  -- vulkan_core.h:5049
      multiviewTessellationShader : aliased VkBool32;  -- vulkan_core.h:5050
      variablePointersStorageBuffer : aliased VkBool32;  -- vulkan_core.h:5051
      variablePointers : aliased VkBool32;  -- vulkan_core.h:5052
      protectedMemory : aliased VkBool32;  -- vulkan_core.h:5053
      samplerYcbcrConversion : aliased VkBool32;  -- vulkan_core.h:5054
      shaderDrawParameters : aliased VkBool32;  -- vulkan_core.h:5055
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceVulkan11Features);  -- vulkan_core.h:5041

   type VkPhysicalDeviceVulkan11Properties_deviceUUID_array is array (0 .. 15) of aliased stdint_h.uint8_t;
   type VkPhysicalDeviceVulkan11Properties_driverUUID_array is array (0 .. 15) of aliased stdint_h.uint8_t;
   type VkPhysicalDeviceVulkan11Properties_deviceLUID_array is array (0 .. 7) of aliased stdint_h.uint8_t;
   type VkPhysicalDeviceVulkan11Properties is record
      sType : aliased VkStructureType;  -- vulkan_core.h:5059
      pNext : System.Address;  -- vulkan_core.h:5060
      deviceUUID : aliased VkPhysicalDeviceVulkan11Properties_deviceUUID_array;  -- vulkan_core.h:5061
      driverUUID : aliased VkPhysicalDeviceVulkan11Properties_driverUUID_array;  -- vulkan_core.h:5062
      deviceLUID : aliased VkPhysicalDeviceVulkan11Properties_deviceLUID_array;  -- vulkan_core.h:5063
      deviceNodeMask : aliased stdint_h.uint32_t;  -- vulkan_core.h:5064
      deviceLUIDValid : aliased VkBool32;  -- vulkan_core.h:5065
      subgroupSize : aliased stdint_h.uint32_t;  -- vulkan_core.h:5066
      subgroupSupportedStages : aliased VkShaderStageFlags;  -- vulkan_core.h:5067
      subgroupSupportedOperations : aliased VkSubgroupFeatureFlags;  -- vulkan_core.h:5068
      subgroupQuadOperationsInAllStages : aliased VkBool32;  -- vulkan_core.h:5069
      pointClippingBehavior : aliased VkPointClippingBehavior;  -- vulkan_core.h:5070
      maxMultiviewViewCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:5071
      maxMultiviewInstanceIndex : aliased stdint_h.uint32_t;  -- vulkan_core.h:5072
      protectedNoFault : aliased VkBool32;  -- vulkan_core.h:5073
      maxPerSetDescriptors : aliased stdint_h.uint32_t;  -- vulkan_core.h:5074
      maxMemoryAllocationSize : aliased VkDeviceSize;  -- vulkan_core.h:5075
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceVulkan11Properties);  -- vulkan_core.h:5058

   type VkPhysicalDeviceVulkan12Features is record
      sType : aliased VkStructureType;  -- vulkan_core.h:5079
      pNext : System.Address;  -- vulkan_core.h:5080
      samplerMirrorClampToEdge : aliased VkBool32;  -- vulkan_core.h:5081
      drawIndirectCount : aliased VkBool32;  -- vulkan_core.h:5082
      storageBuffer8BitAccess : aliased VkBool32;  -- vulkan_core.h:5083
      uniformAndStorageBuffer8BitAccess : aliased VkBool32;  -- vulkan_core.h:5084
      storagePushConstant8 : aliased VkBool32;  -- vulkan_core.h:5085
      shaderBufferInt64Atomics : aliased VkBool32;  -- vulkan_core.h:5086
      shaderSharedInt64Atomics : aliased VkBool32;  -- vulkan_core.h:5087
      shaderFloat16 : aliased VkBool32;  -- vulkan_core.h:5088
      shaderInt8 : aliased VkBool32;  -- vulkan_core.h:5089
      descriptorIndexing : aliased VkBool32;  -- vulkan_core.h:5090
      shaderInputAttachmentArrayDynamicIndexing : aliased VkBool32;  -- vulkan_core.h:5091
      shaderUniformTexelBufferArrayDynamicIndexing : aliased VkBool32;  -- vulkan_core.h:5092
      shaderStorageTexelBufferArrayDynamicIndexing : aliased VkBool32;  -- vulkan_core.h:5093
      shaderUniformBufferArrayNonUniformIndexing : aliased VkBool32;  -- vulkan_core.h:5094
      shaderSampledImageArrayNonUniformIndexing : aliased VkBool32;  -- vulkan_core.h:5095
      shaderStorageBufferArrayNonUniformIndexing : aliased VkBool32;  -- vulkan_core.h:5096
      shaderStorageImageArrayNonUniformIndexing : aliased VkBool32;  -- vulkan_core.h:5097
      shaderInputAttachmentArrayNonUniformIndexing : aliased VkBool32;  -- vulkan_core.h:5098
      shaderUniformTexelBufferArrayNonUniformIndexing : aliased VkBool32;  -- vulkan_core.h:5099
      shaderStorageTexelBufferArrayNonUniformIndexing : aliased VkBool32;  -- vulkan_core.h:5100
      descriptorBindingUniformBufferUpdateAfterBind : aliased VkBool32;  -- vulkan_core.h:5101
      descriptorBindingSampledImageUpdateAfterBind : aliased VkBool32;  -- vulkan_core.h:5102
      descriptorBindingStorageImageUpdateAfterBind : aliased VkBool32;  -- vulkan_core.h:5103
      descriptorBindingStorageBufferUpdateAfterBind : aliased VkBool32;  -- vulkan_core.h:5104
      descriptorBindingUniformTexelBufferUpdateAfterBind : aliased VkBool32;  -- vulkan_core.h:5105
      descriptorBindingStorageTexelBufferUpdateAfterBind : aliased VkBool32;  -- vulkan_core.h:5106
      descriptorBindingUpdateUnusedWhilePending : aliased VkBool32;  -- vulkan_core.h:5107
      descriptorBindingPartiallyBound : aliased VkBool32;  -- vulkan_core.h:5108
      descriptorBindingVariableDescriptorCount : aliased VkBool32;  -- vulkan_core.h:5109
      runtimeDescriptorArray : aliased VkBool32;  -- vulkan_core.h:5110
      samplerFilterMinmax : aliased VkBool32;  -- vulkan_core.h:5111
      scalarBlockLayout : aliased VkBool32;  -- vulkan_core.h:5112
      imagelessFramebuffer : aliased VkBool32;  -- vulkan_core.h:5113
      uniformBufferStandardLayout : aliased VkBool32;  -- vulkan_core.h:5114
      shaderSubgroupExtendedTypes : aliased VkBool32;  -- vulkan_core.h:5115
      separateDepthStencilLayouts : aliased VkBool32;  -- vulkan_core.h:5116
      hostQueryReset : aliased VkBool32;  -- vulkan_core.h:5117
      timelineSemaphore : aliased VkBool32;  -- vulkan_core.h:5118
      bufferDeviceAddress : aliased VkBool32;  -- vulkan_core.h:5119
      bufferDeviceAddressCaptureReplay : aliased VkBool32;  -- vulkan_core.h:5120
      bufferDeviceAddressMultiDevice : aliased VkBool32;  -- vulkan_core.h:5121
      vulkanMemoryModel : aliased VkBool32;  -- vulkan_core.h:5122
      vulkanMemoryModelDeviceScope : aliased VkBool32;  -- vulkan_core.h:5123
      vulkanMemoryModelAvailabilityVisibilityChains : aliased VkBool32;  -- vulkan_core.h:5124
      shaderOutputViewportIndex : aliased VkBool32;  -- vulkan_core.h:5125
      shaderOutputLayer : aliased VkBool32;  -- vulkan_core.h:5126
      subgroupBroadcastDynamicId : aliased VkBool32;  -- vulkan_core.h:5127
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceVulkan12Features);  -- vulkan_core.h:5078

   type VkConformanceVersion is record
      major : aliased stdint_h.uint8_t;  -- vulkan_core.h:5131
      minor : aliased stdint_h.uint8_t;  -- vulkan_core.h:5132
      subminor : aliased stdint_h.uint8_t;  -- vulkan_core.h:5133
      patch : aliased stdint_h.uint8_t;  -- vulkan_core.h:5134
   end record;
   pragma Convention (C_Pass_By_Copy, VkConformanceVersion);  -- vulkan_core.h:5130

   subtype VkPhysicalDeviceVulkan12Properties_driverName_array is Interfaces.C.char_array (0 .. 255);
   subtype VkPhysicalDeviceVulkan12Properties_driverInfo_array is Interfaces.C.char_array (0 .. 255);
   type VkPhysicalDeviceVulkan12Properties is record
      sType : aliased VkStructureType;  -- vulkan_core.h:5138
      pNext : System.Address;  -- vulkan_core.h:5139
      driverID : aliased VkDriverId;  -- vulkan_core.h:5140
      driverName : aliased VkPhysicalDeviceVulkan12Properties_driverName_array;  -- vulkan_core.h:5141
      driverInfo : aliased VkPhysicalDeviceVulkan12Properties_driverInfo_array;  -- vulkan_core.h:5142
      conformanceVersion : aliased VkConformanceVersion;  -- vulkan_core.h:5143
      denormBehaviorIndependence : aliased VkShaderFloatControlsIndependence;  -- vulkan_core.h:5144
      roundingModeIndependence : aliased VkShaderFloatControlsIndependence;  -- vulkan_core.h:5145
      shaderSignedZeroInfNanPreserveFloat16 : aliased VkBool32;  -- vulkan_core.h:5146
      shaderSignedZeroInfNanPreserveFloat32 : aliased VkBool32;  -- vulkan_core.h:5147
      shaderSignedZeroInfNanPreserveFloat64 : aliased VkBool32;  -- vulkan_core.h:5148
      shaderDenormPreserveFloat16 : aliased VkBool32;  -- vulkan_core.h:5149
      shaderDenormPreserveFloat32 : aliased VkBool32;  -- vulkan_core.h:5150
      shaderDenormPreserveFloat64 : aliased VkBool32;  -- vulkan_core.h:5151
      shaderDenormFlushToZeroFloat16 : aliased VkBool32;  -- vulkan_core.h:5152
      shaderDenormFlushToZeroFloat32 : aliased VkBool32;  -- vulkan_core.h:5153
      shaderDenormFlushToZeroFloat64 : aliased VkBool32;  -- vulkan_core.h:5154
      shaderRoundingModeRTEFloat16 : aliased VkBool32;  -- vulkan_core.h:5155
      shaderRoundingModeRTEFloat32 : aliased VkBool32;  -- vulkan_core.h:5156
      shaderRoundingModeRTEFloat64 : aliased VkBool32;  -- vulkan_core.h:5157
      shaderRoundingModeRTZFloat16 : aliased VkBool32;  -- vulkan_core.h:5158
      shaderRoundingModeRTZFloat32 : aliased VkBool32;  -- vulkan_core.h:5159
      shaderRoundingModeRTZFloat64 : aliased VkBool32;  -- vulkan_core.h:5160
      maxUpdateAfterBindDescriptorsInAllPools : aliased stdint_h.uint32_t;  -- vulkan_core.h:5161
      shaderUniformBufferArrayNonUniformIndexingNative : aliased VkBool32;  -- vulkan_core.h:5162
      shaderSampledImageArrayNonUniformIndexingNative : aliased VkBool32;  -- vulkan_core.h:5163
      shaderStorageBufferArrayNonUniformIndexingNative : aliased VkBool32;  -- vulkan_core.h:5164
      shaderStorageImageArrayNonUniformIndexingNative : aliased VkBool32;  -- vulkan_core.h:5165
      shaderInputAttachmentArrayNonUniformIndexingNative : aliased VkBool32;  -- vulkan_core.h:5166
      robustBufferAccessUpdateAfterBind : aliased VkBool32;  -- vulkan_core.h:5167
      quadDivergentImplicitLod : aliased VkBool32;  -- vulkan_core.h:5168
      maxPerStageDescriptorUpdateAfterBindSamplers : aliased stdint_h.uint32_t;  -- vulkan_core.h:5169
      maxPerStageDescriptorUpdateAfterBindUniformBuffers : aliased stdint_h.uint32_t;  -- vulkan_core.h:5170
      maxPerStageDescriptorUpdateAfterBindStorageBuffers : aliased stdint_h.uint32_t;  -- vulkan_core.h:5171
      maxPerStageDescriptorUpdateAfterBindSampledImages : aliased stdint_h.uint32_t;  -- vulkan_core.h:5172
      maxPerStageDescriptorUpdateAfterBindStorageImages : aliased stdint_h.uint32_t;  -- vulkan_core.h:5173
      maxPerStageDescriptorUpdateAfterBindInputAttachments : aliased stdint_h.uint32_t;  -- vulkan_core.h:5174
      maxPerStageUpdateAfterBindResources : aliased stdint_h.uint32_t;  -- vulkan_core.h:5175
      maxDescriptorSetUpdateAfterBindSamplers : aliased stdint_h.uint32_t;  -- vulkan_core.h:5176
      maxDescriptorSetUpdateAfterBindUniformBuffers : aliased stdint_h.uint32_t;  -- vulkan_core.h:5177
      maxDescriptorSetUpdateAfterBindUniformBuffersDynamic : aliased stdint_h.uint32_t;  -- vulkan_core.h:5178
      maxDescriptorSetUpdateAfterBindStorageBuffers : aliased stdint_h.uint32_t;  -- vulkan_core.h:5179
      maxDescriptorSetUpdateAfterBindStorageBuffersDynamic : aliased stdint_h.uint32_t;  -- vulkan_core.h:5180
      maxDescriptorSetUpdateAfterBindSampledImages : aliased stdint_h.uint32_t;  -- vulkan_core.h:5181
      maxDescriptorSetUpdateAfterBindStorageImages : aliased stdint_h.uint32_t;  -- vulkan_core.h:5182
      maxDescriptorSetUpdateAfterBindInputAttachments : aliased stdint_h.uint32_t;  -- vulkan_core.h:5183
      supportedDepthResolveModes : aliased VkResolveModeFlags;  -- vulkan_core.h:5184
      supportedStencilResolveModes : aliased VkResolveModeFlags;  -- vulkan_core.h:5185
      independentResolveNone : aliased VkBool32;  -- vulkan_core.h:5186
      independentResolve : aliased VkBool32;  -- vulkan_core.h:5187
      filterMinmaxSingleComponentFormats : aliased VkBool32;  -- vulkan_core.h:5188
      filterMinmaxImageComponentMapping : aliased VkBool32;  -- vulkan_core.h:5189
      maxTimelineSemaphoreValueDifference : aliased stdint_h.uint64_t;  -- vulkan_core.h:5190
      framebufferIntegerColorSampleCounts : aliased VkSampleCountFlags;  -- vulkan_core.h:5191
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceVulkan12Properties);  -- vulkan_core.h:5137

   type VkImageFormatListCreateInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:5195
      pNext : System.Address;  -- vulkan_core.h:5196
      viewFormatCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:5197
      pViewFormats : System.Address;  -- vulkan_core.h:5198
   end record;
   pragma Convention (C_Pass_By_Copy, VkImageFormatListCreateInfo);  -- vulkan_core.h:5194

   type VkAttachmentDescription2 is record
      sType : aliased VkStructureType;  -- vulkan_core.h:5202
      pNext : System.Address;  -- vulkan_core.h:5203
      flags : aliased VkAttachmentDescriptionFlags;  -- vulkan_core.h:5204
      format : aliased VkFormat;  -- vulkan_core.h:5205
      samples : aliased VkSampleCountFlagBits;  -- vulkan_core.h:5206
      loadOp : aliased VkAttachmentLoadOp;  -- vulkan_core.h:5207
      storeOp : aliased VkAttachmentStoreOp;  -- vulkan_core.h:5208
      stencilLoadOp : aliased VkAttachmentLoadOp;  -- vulkan_core.h:5209
      stencilStoreOp : aliased VkAttachmentStoreOp;  -- vulkan_core.h:5210
      initialLayout : aliased VkImageLayout;  -- vulkan_core.h:5211
      finalLayout : aliased VkImageLayout;  -- vulkan_core.h:5212
   end record;
   pragma Convention (C_Pass_By_Copy, VkAttachmentDescription2);  -- vulkan_core.h:5201

   type VkAttachmentReference2 is record
      sType : aliased VkStructureType;  -- vulkan_core.h:5216
      pNext : System.Address;  -- vulkan_core.h:5217
      attachment : aliased stdint_h.uint32_t;  -- vulkan_core.h:5218
      layout : aliased VkImageLayout;  -- vulkan_core.h:5219
      aspectMask : aliased VkImageAspectFlags;  -- vulkan_core.h:5220
   end record;
   pragma Convention (C_Pass_By_Copy, VkAttachmentReference2);  -- vulkan_core.h:5215

   type VkSubpassDescription2 is record
      sType : aliased VkStructureType;  -- vulkan_core.h:5224
      pNext : System.Address;  -- vulkan_core.h:5225
      flags : aliased VkSubpassDescriptionFlags;  -- vulkan_core.h:5226
      pipelineBindPoint : aliased VkPipelineBindPoint;  -- vulkan_core.h:5227
      viewMask : aliased stdint_h.uint32_t;  -- vulkan_core.h:5228
      inputAttachmentCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:5229
      pInputAttachments : System.Address;  -- vulkan_core.h:5230
      colorAttachmentCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:5231
      pColorAttachments : System.Address;  -- vulkan_core.h:5232
      pResolveAttachments : System.Address;  -- vulkan_core.h:5233
      pDepthStencilAttachment : System.Address;  -- vulkan_core.h:5234
      preserveAttachmentCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:5235
      pPreserveAttachments : access stdint_h.uint32_t;  -- vulkan_core.h:5236
   end record;
   pragma Convention (C_Pass_By_Copy, VkSubpassDescription2);  -- vulkan_core.h:5223

   type VkSubpassDependency2 is record
      sType : aliased VkStructureType;  -- vulkan_core.h:5240
      pNext : System.Address;  -- vulkan_core.h:5241
      srcSubpass : aliased stdint_h.uint32_t;  -- vulkan_core.h:5242
      dstSubpass : aliased stdint_h.uint32_t;  -- vulkan_core.h:5243
      srcStageMask : aliased VkPipelineStageFlags;  -- vulkan_core.h:5244
      dstStageMask : aliased VkPipelineStageFlags;  -- vulkan_core.h:5245
      srcAccessMask : aliased VkAccessFlags;  -- vulkan_core.h:5246
      dstAccessMask : aliased VkAccessFlags;  -- vulkan_core.h:5247
      dependencyFlags : aliased VkDependencyFlags;  -- vulkan_core.h:5248
      viewOffset : aliased stdint_h.int32_t;  -- vulkan_core.h:5249
   end record;
   pragma Convention (C_Pass_By_Copy, VkSubpassDependency2);  -- vulkan_core.h:5239

   type VkRenderPassCreateInfo2 is record
      sType : aliased VkStructureType;  -- vulkan_core.h:5253
      pNext : System.Address;  -- vulkan_core.h:5254
      flags : aliased VkRenderPassCreateFlags;  -- vulkan_core.h:5255
      attachmentCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:5256
      pAttachments : System.Address;  -- vulkan_core.h:5257
      subpassCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:5258
      pSubpasses : System.Address;  -- vulkan_core.h:5259
      dependencyCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:5260
      pDependencies : System.Address;  -- vulkan_core.h:5261
      correlatedViewMaskCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:5262
      pCorrelatedViewMasks : access stdint_h.uint32_t;  -- vulkan_core.h:5263
   end record;
   pragma Convention (C_Pass_By_Copy, VkRenderPassCreateInfo2);  -- vulkan_core.h:5252

   type VkSubpassBeginInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:5267
      pNext : System.Address;  -- vulkan_core.h:5268
      contents : aliased VkSubpassContents;  -- vulkan_core.h:5269
   end record;
   pragma Convention (C_Pass_By_Copy, VkSubpassBeginInfo);  -- vulkan_core.h:5266

   type VkSubpassEndInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:5273
      pNext : System.Address;  -- vulkan_core.h:5274
   end record;
   pragma Convention (C_Pass_By_Copy, VkSubpassEndInfo);  -- vulkan_core.h:5272

   type VkPhysicalDevice8BitStorageFeatures is record
      sType : aliased VkStructureType;  -- vulkan_core.h:5278
      pNext : System.Address;  -- vulkan_core.h:5279
      storageBuffer8BitAccess : aliased VkBool32;  -- vulkan_core.h:5280
      uniformAndStorageBuffer8BitAccess : aliased VkBool32;  -- vulkan_core.h:5281
      storagePushConstant8 : aliased VkBool32;  -- vulkan_core.h:5282
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDevice8BitStorageFeatures);  -- vulkan_core.h:5277

   subtype VkPhysicalDeviceDriverProperties_driverName_array is Interfaces.C.char_array (0 .. 255);
   subtype VkPhysicalDeviceDriverProperties_driverInfo_array is Interfaces.C.char_array (0 .. 255);
   type VkPhysicalDeviceDriverProperties is record
      sType : aliased VkStructureType;  -- vulkan_core.h:5286
      pNext : System.Address;  -- vulkan_core.h:5287
      driverID : aliased VkDriverId;  -- vulkan_core.h:5288
      driverName : aliased VkPhysicalDeviceDriverProperties_driverName_array;  -- vulkan_core.h:5289
      driverInfo : aliased VkPhysicalDeviceDriverProperties_driverInfo_array;  -- vulkan_core.h:5290
      conformanceVersion : aliased VkConformanceVersion;  -- vulkan_core.h:5291
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceDriverProperties);  -- vulkan_core.h:5285

   type VkPhysicalDeviceShaderAtomicInt64Features is record
      sType : aliased VkStructureType;  -- vulkan_core.h:5295
      pNext : System.Address;  -- vulkan_core.h:5296
      shaderBufferInt64Atomics : aliased VkBool32;  -- vulkan_core.h:5297
      shaderSharedInt64Atomics : aliased VkBool32;  -- vulkan_core.h:5298
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceShaderAtomicInt64Features);  -- vulkan_core.h:5294

   type VkPhysicalDeviceShaderFloat16Int8Features is record
      sType : aliased VkStructureType;  -- vulkan_core.h:5302
      pNext : System.Address;  -- vulkan_core.h:5303
      shaderFloat16 : aliased VkBool32;  -- vulkan_core.h:5304
      shaderInt8 : aliased VkBool32;  -- vulkan_core.h:5305
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceShaderFloat16Int8Features);  -- vulkan_core.h:5301

   type VkPhysicalDeviceFloatControlsProperties is record
      sType : aliased VkStructureType;  -- vulkan_core.h:5309
      pNext : System.Address;  -- vulkan_core.h:5310
      denormBehaviorIndependence : aliased VkShaderFloatControlsIndependence;  -- vulkan_core.h:5311
      roundingModeIndependence : aliased VkShaderFloatControlsIndependence;  -- vulkan_core.h:5312
      shaderSignedZeroInfNanPreserveFloat16 : aliased VkBool32;  -- vulkan_core.h:5313
      shaderSignedZeroInfNanPreserveFloat32 : aliased VkBool32;  -- vulkan_core.h:5314
      shaderSignedZeroInfNanPreserveFloat64 : aliased VkBool32;  -- vulkan_core.h:5315
      shaderDenormPreserveFloat16 : aliased VkBool32;  -- vulkan_core.h:5316
      shaderDenormPreserveFloat32 : aliased VkBool32;  -- vulkan_core.h:5317
      shaderDenormPreserveFloat64 : aliased VkBool32;  -- vulkan_core.h:5318
      shaderDenormFlushToZeroFloat16 : aliased VkBool32;  -- vulkan_core.h:5319
      shaderDenormFlushToZeroFloat32 : aliased VkBool32;  -- vulkan_core.h:5320
      shaderDenormFlushToZeroFloat64 : aliased VkBool32;  -- vulkan_core.h:5321
      shaderRoundingModeRTEFloat16 : aliased VkBool32;  -- vulkan_core.h:5322
      shaderRoundingModeRTEFloat32 : aliased VkBool32;  -- vulkan_core.h:5323
      shaderRoundingModeRTEFloat64 : aliased VkBool32;  -- vulkan_core.h:5324
      shaderRoundingModeRTZFloat16 : aliased VkBool32;  -- vulkan_core.h:5325
      shaderRoundingModeRTZFloat32 : aliased VkBool32;  -- vulkan_core.h:5326
      shaderRoundingModeRTZFloat64 : aliased VkBool32;  -- vulkan_core.h:5327
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceFloatControlsProperties);  -- vulkan_core.h:5308

   type VkDescriptorSetLayoutBindingFlagsCreateInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:5331
      pNext : System.Address;  -- vulkan_core.h:5332
      bindingCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:5333
      pBindingFlags : access VkDescriptorBindingFlags;  -- vulkan_core.h:5334
   end record;
   pragma Convention (C_Pass_By_Copy, VkDescriptorSetLayoutBindingFlagsCreateInfo);  -- vulkan_core.h:5330

   type VkPhysicalDeviceDescriptorIndexingFeatures is record
      sType : aliased VkStructureType;  -- vulkan_core.h:5338
      pNext : System.Address;  -- vulkan_core.h:5339
      shaderInputAttachmentArrayDynamicIndexing : aliased VkBool32;  -- vulkan_core.h:5340
      shaderUniformTexelBufferArrayDynamicIndexing : aliased VkBool32;  -- vulkan_core.h:5341
      shaderStorageTexelBufferArrayDynamicIndexing : aliased VkBool32;  -- vulkan_core.h:5342
      shaderUniformBufferArrayNonUniformIndexing : aliased VkBool32;  -- vulkan_core.h:5343
      shaderSampledImageArrayNonUniformIndexing : aliased VkBool32;  -- vulkan_core.h:5344
      shaderStorageBufferArrayNonUniformIndexing : aliased VkBool32;  -- vulkan_core.h:5345
      shaderStorageImageArrayNonUniformIndexing : aliased VkBool32;  -- vulkan_core.h:5346
      shaderInputAttachmentArrayNonUniformIndexing : aliased VkBool32;  -- vulkan_core.h:5347
      shaderUniformTexelBufferArrayNonUniformIndexing : aliased VkBool32;  -- vulkan_core.h:5348
      shaderStorageTexelBufferArrayNonUniformIndexing : aliased VkBool32;  -- vulkan_core.h:5349
      descriptorBindingUniformBufferUpdateAfterBind : aliased VkBool32;  -- vulkan_core.h:5350
      descriptorBindingSampledImageUpdateAfterBind : aliased VkBool32;  -- vulkan_core.h:5351
      descriptorBindingStorageImageUpdateAfterBind : aliased VkBool32;  -- vulkan_core.h:5352
      descriptorBindingStorageBufferUpdateAfterBind : aliased VkBool32;  -- vulkan_core.h:5353
      descriptorBindingUniformTexelBufferUpdateAfterBind : aliased VkBool32;  -- vulkan_core.h:5354
      descriptorBindingStorageTexelBufferUpdateAfterBind : aliased VkBool32;  -- vulkan_core.h:5355
      descriptorBindingUpdateUnusedWhilePending : aliased VkBool32;  -- vulkan_core.h:5356
      descriptorBindingPartiallyBound : aliased VkBool32;  -- vulkan_core.h:5357
      descriptorBindingVariableDescriptorCount : aliased VkBool32;  -- vulkan_core.h:5358
      runtimeDescriptorArray : aliased VkBool32;  -- vulkan_core.h:5359
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceDescriptorIndexingFeatures);  -- vulkan_core.h:5337

   type VkPhysicalDeviceDescriptorIndexingProperties is record
      sType : aliased VkStructureType;  -- vulkan_core.h:5363
      pNext : System.Address;  -- vulkan_core.h:5364
      maxUpdateAfterBindDescriptorsInAllPools : aliased stdint_h.uint32_t;  -- vulkan_core.h:5365
      shaderUniformBufferArrayNonUniformIndexingNative : aliased VkBool32;  -- vulkan_core.h:5366
      shaderSampledImageArrayNonUniformIndexingNative : aliased VkBool32;  -- vulkan_core.h:5367
      shaderStorageBufferArrayNonUniformIndexingNative : aliased VkBool32;  -- vulkan_core.h:5368
      shaderStorageImageArrayNonUniformIndexingNative : aliased VkBool32;  -- vulkan_core.h:5369
      shaderInputAttachmentArrayNonUniformIndexingNative : aliased VkBool32;  -- vulkan_core.h:5370
      robustBufferAccessUpdateAfterBind : aliased VkBool32;  -- vulkan_core.h:5371
      quadDivergentImplicitLod : aliased VkBool32;  -- vulkan_core.h:5372
      maxPerStageDescriptorUpdateAfterBindSamplers : aliased stdint_h.uint32_t;  -- vulkan_core.h:5373
      maxPerStageDescriptorUpdateAfterBindUniformBuffers : aliased stdint_h.uint32_t;  -- vulkan_core.h:5374
      maxPerStageDescriptorUpdateAfterBindStorageBuffers : aliased stdint_h.uint32_t;  -- vulkan_core.h:5375
      maxPerStageDescriptorUpdateAfterBindSampledImages : aliased stdint_h.uint32_t;  -- vulkan_core.h:5376
      maxPerStageDescriptorUpdateAfterBindStorageImages : aliased stdint_h.uint32_t;  -- vulkan_core.h:5377
      maxPerStageDescriptorUpdateAfterBindInputAttachments : aliased stdint_h.uint32_t;  -- vulkan_core.h:5378
      maxPerStageUpdateAfterBindResources : aliased stdint_h.uint32_t;  -- vulkan_core.h:5379
      maxDescriptorSetUpdateAfterBindSamplers : aliased stdint_h.uint32_t;  -- vulkan_core.h:5380
      maxDescriptorSetUpdateAfterBindUniformBuffers : aliased stdint_h.uint32_t;  -- vulkan_core.h:5381
      maxDescriptorSetUpdateAfterBindUniformBuffersDynamic : aliased stdint_h.uint32_t;  -- vulkan_core.h:5382
      maxDescriptorSetUpdateAfterBindStorageBuffers : aliased stdint_h.uint32_t;  -- vulkan_core.h:5383
      maxDescriptorSetUpdateAfterBindStorageBuffersDynamic : aliased stdint_h.uint32_t;  -- vulkan_core.h:5384
      maxDescriptorSetUpdateAfterBindSampledImages : aliased stdint_h.uint32_t;  -- vulkan_core.h:5385
      maxDescriptorSetUpdateAfterBindStorageImages : aliased stdint_h.uint32_t;  -- vulkan_core.h:5386
      maxDescriptorSetUpdateAfterBindInputAttachments : aliased stdint_h.uint32_t;  -- vulkan_core.h:5387
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceDescriptorIndexingProperties);  -- vulkan_core.h:5362

   type VkDescriptorSetVariableDescriptorCountAllocateInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:5391
      pNext : System.Address;  -- vulkan_core.h:5392
      descriptorSetCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:5393
      pDescriptorCounts : access stdint_h.uint32_t;  -- vulkan_core.h:5394
   end record;
   pragma Convention (C_Pass_By_Copy, VkDescriptorSetVariableDescriptorCountAllocateInfo);  -- vulkan_core.h:5390

   type VkDescriptorSetVariableDescriptorCountLayoutSupport is record
      sType : aliased VkStructureType;  -- vulkan_core.h:5398
      pNext : System.Address;  -- vulkan_core.h:5399
      maxVariableDescriptorCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:5400
   end record;
   pragma Convention (C_Pass_By_Copy, VkDescriptorSetVariableDescriptorCountLayoutSupport);  -- vulkan_core.h:5397

   type VkSubpassDescriptionDepthStencilResolve is record
      sType : aliased VkStructureType;  -- vulkan_core.h:5404
      pNext : System.Address;  -- vulkan_core.h:5405
      depthResolveMode : aliased VkResolveModeFlagBits;  -- vulkan_core.h:5406
      stencilResolveMode : aliased VkResolveModeFlagBits;  -- vulkan_core.h:5407
      pDepthStencilResolveAttachment : System.Address;  -- vulkan_core.h:5408
   end record;
   pragma Convention (C_Pass_By_Copy, VkSubpassDescriptionDepthStencilResolve);  -- vulkan_core.h:5403

   type VkPhysicalDeviceDepthStencilResolveProperties is record
      sType : aliased VkStructureType;  -- vulkan_core.h:5412
      pNext : System.Address;  -- vulkan_core.h:5413
      supportedDepthResolveModes : aliased VkResolveModeFlags;  -- vulkan_core.h:5414
      supportedStencilResolveModes : aliased VkResolveModeFlags;  -- vulkan_core.h:5415
      independentResolveNone : aliased VkBool32;  -- vulkan_core.h:5416
      independentResolve : aliased VkBool32;  -- vulkan_core.h:5417
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceDepthStencilResolveProperties);  -- vulkan_core.h:5411

   type VkPhysicalDeviceScalarBlockLayoutFeatures is record
      sType : aliased VkStructureType;  -- vulkan_core.h:5421
      pNext : System.Address;  -- vulkan_core.h:5422
      scalarBlockLayout : aliased VkBool32;  -- vulkan_core.h:5423
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceScalarBlockLayoutFeatures);  -- vulkan_core.h:5420

   type VkImageStencilUsageCreateInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:5427
      pNext : System.Address;  -- vulkan_core.h:5428
      stencilUsage : aliased VkImageUsageFlags;  -- vulkan_core.h:5429
   end record;
   pragma Convention (C_Pass_By_Copy, VkImageStencilUsageCreateInfo);  -- vulkan_core.h:5426

   type VkSamplerReductionModeCreateInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:5433
      pNext : System.Address;  -- vulkan_core.h:5434
      reductionMode : aliased VkSamplerReductionMode;  -- vulkan_core.h:5435
   end record;
   pragma Convention (C_Pass_By_Copy, VkSamplerReductionModeCreateInfo);  -- vulkan_core.h:5432

   type VkPhysicalDeviceSamplerFilterMinmaxProperties is record
      sType : aliased VkStructureType;  -- vulkan_core.h:5439
      pNext : System.Address;  -- vulkan_core.h:5440
      filterMinmaxSingleComponentFormats : aliased VkBool32;  -- vulkan_core.h:5441
      filterMinmaxImageComponentMapping : aliased VkBool32;  -- vulkan_core.h:5442
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceSamplerFilterMinmaxProperties);  -- vulkan_core.h:5438

   type VkPhysicalDeviceVulkanMemoryModelFeatures is record
      sType : aliased VkStructureType;  -- vulkan_core.h:5446
      pNext : System.Address;  -- vulkan_core.h:5447
      vulkanMemoryModel : aliased VkBool32;  -- vulkan_core.h:5448
      vulkanMemoryModelDeviceScope : aliased VkBool32;  -- vulkan_core.h:5449
      vulkanMemoryModelAvailabilityVisibilityChains : aliased VkBool32;  -- vulkan_core.h:5450
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceVulkanMemoryModelFeatures);  -- vulkan_core.h:5445

   type VkPhysicalDeviceImagelessFramebufferFeatures is record
      sType : aliased VkStructureType;  -- vulkan_core.h:5454
      pNext : System.Address;  -- vulkan_core.h:5455
      imagelessFramebuffer : aliased VkBool32;  -- vulkan_core.h:5456
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceImagelessFramebufferFeatures);  -- vulkan_core.h:5453

   type VkFramebufferAttachmentImageInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:5460
      pNext : System.Address;  -- vulkan_core.h:5461
      flags : aliased VkImageCreateFlags;  -- vulkan_core.h:5462
      usage : aliased VkImageUsageFlags;  -- vulkan_core.h:5463
      width : aliased stdint_h.uint32_t;  -- vulkan_core.h:5464
      height : aliased stdint_h.uint32_t;  -- vulkan_core.h:5465
      layerCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:5466
      viewFormatCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:5467
      pViewFormats : System.Address;  -- vulkan_core.h:5468
   end record;
   pragma Convention (C_Pass_By_Copy, VkFramebufferAttachmentImageInfo);  -- vulkan_core.h:5459

   type VkFramebufferAttachmentsCreateInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:5472
      pNext : System.Address;  -- vulkan_core.h:5473
      attachmentImageInfoCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:5474
      pAttachmentImageInfos : System.Address;  -- vulkan_core.h:5475
   end record;
   pragma Convention (C_Pass_By_Copy, VkFramebufferAttachmentsCreateInfo);  -- vulkan_core.h:5471

   type VkRenderPassAttachmentBeginInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:5479
      pNext : System.Address;  -- vulkan_core.h:5480
      attachmentCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:5481
      pAttachments : System.Address;  -- vulkan_core.h:5482
   end record;
   pragma Convention (C_Pass_By_Copy, VkRenderPassAttachmentBeginInfo);  -- vulkan_core.h:5478

   type VkPhysicalDeviceUniformBufferStandardLayoutFeatures is record
      sType : aliased VkStructureType;  -- vulkan_core.h:5486
      pNext : System.Address;  -- vulkan_core.h:5487
      uniformBufferStandardLayout : aliased VkBool32;  -- vulkan_core.h:5488
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceUniformBufferStandardLayoutFeatures);  -- vulkan_core.h:5485

   type VkPhysicalDeviceShaderSubgroupExtendedTypesFeatures is record
      sType : aliased VkStructureType;  -- vulkan_core.h:5492
      pNext : System.Address;  -- vulkan_core.h:5493
      shaderSubgroupExtendedTypes : aliased VkBool32;  -- vulkan_core.h:5494
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceShaderSubgroupExtendedTypesFeatures);  -- vulkan_core.h:5491

   type VkPhysicalDeviceSeparateDepthStencilLayoutsFeatures is record
      sType : aliased VkStructureType;  -- vulkan_core.h:5498
      pNext : System.Address;  -- vulkan_core.h:5499
      separateDepthStencilLayouts : aliased VkBool32;  -- vulkan_core.h:5500
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceSeparateDepthStencilLayoutsFeatures);  -- vulkan_core.h:5497

   type VkAttachmentReferenceStencilLayout is record
      sType : aliased VkStructureType;  -- vulkan_core.h:5504
      pNext : System.Address;  -- vulkan_core.h:5505
      stencilLayout : aliased VkImageLayout;  -- vulkan_core.h:5506
   end record;
   pragma Convention (C_Pass_By_Copy, VkAttachmentReferenceStencilLayout);  -- vulkan_core.h:5503

   type VkAttachmentDescriptionStencilLayout is record
      sType : aliased VkStructureType;  -- vulkan_core.h:5510
      pNext : System.Address;  -- vulkan_core.h:5511
      stencilInitialLayout : aliased VkImageLayout;  -- vulkan_core.h:5512
      stencilFinalLayout : aliased VkImageLayout;  -- vulkan_core.h:5513
   end record;
   pragma Convention (C_Pass_By_Copy, VkAttachmentDescriptionStencilLayout);  -- vulkan_core.h:5509

   type VkPhysicalDeviceHostQueryResetFeatures is record
      sType : aliased VkStructureType;  -- vulkan_core.h:5517
      pNext : System.Address;  -- vulkan_core.h:5518
      hostQueryReset : aliased VkBool32;  -- vulkan_core.h:5519
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceHostQueryResetFeatures);  -- vulkan_core.h:5516

   type VkPhysicalDeviceTimelineSemaphoreFeatures is record
      sType : aliased VkStructureType;  -- vulkan_core.h:5523
      pNext : System.Address;  -- vulkan_core.h:5524
      timelineSemaphore : aliased VkBool32;  -- vulkan_core.h:5525
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceTimelineSemaphoreFeatures);  -- vulkan_core.h:5522

   type VkPhysicalDeviceTimelineSemaphoreProperties is record
      sType : aliased VkStructureType;  -- vulkan_core.h:5529
      pNext : System.Address;  -- vulkan_core.h:5530
      maxTimelineSemaphoreValueDifference : aliased stdint_h.uint64_t;  -- vulkan_core.h:5531
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceTimelineSemaphoreProperties);  -- vulkan_core.h:5528

   type VkSemaphoreTypeCreateInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:5535
      pNext : System.Address;  -- vulkan_core.h:5536
      semaphoreType : aliased VkSemaphoreType;  -- vulkan_core.h:5537
      initialValue : aliased stdint_h.uint64_t;  -- vulkan_core.h:5538
   end record;
   pragma Convention (C_Pass_By_Copy, VkSemaphoreTypeCreateInfo);  -- vulkan_core.h:5534

   type VkTimelineSemaphoreSubmitInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:5542
      pNext : System.Address;  -- vulkan_core.h:5543
      waitSemaphoreValueCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:5544
      pWaitSemaphoreValues : access stdint_h.uint64_t;  -- vulkan_core.h:5545
      signalSemaphoreValueCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:5546
      pSignalSemaphoreValues : access stdint_h.uint64_t;  -- vulkan_core.h:5547
   end record;
   pragma Convention (C_Pass_By_Copy, VkTimelineSemaphoreSubmitInfo);  -- vulkan_core.h:5541

   type VkSemaphoreWaitInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:5551
      pNext : System.Address;  -- vulkan_core.h:5552
      flags : aliased VkSemaphoreWaitFlags;  -- vulkan_core.h:5553
      semaphoreCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:5554
      pSemaphores : System.Address;  -- vulkan_core.h:5555
      pValues : access stdint_h.uint64_t;  -- vulkan_core.h:5556
   end record;
   pragma Convention (C_Pass_By_Copy, VkSemaphoreWaitInfo);  -- vulkan_core.h:5550

   type VkSemaphoreSignalInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:5560
      pNext : System.Address;  -- vulkan_core.h:5561
      semaphore : VkSemaphore;  -- vulkan_core.h:5562
      value : aliased stdint_h.uint64_t;  -- vulkan_core.h:5563
   end record;
   pragma Convention (C_Pass_By_Copy, VkSemaphoreSignalInfo);  -- vulkan_core.h:5559

   type VkPhysicalDeviceBufferDeviceAddressFeatures is record
      sType : aliased VkStructureType;  -- vulkan_core.h:5567
      pNext : System.Address;  -- vulkan_core.h:5568
      bufferDeviceAddress : aliased VkBool32;  -- vulkan_core.h:5569
      bufferDeviceAddressCaptureReplay : aliased VkBool32;  -- vulkan_core.h:5570
      bufferDeviceAddressMultiDevice : aliased VkBool32;  -- vulkan_core.h:5571
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceBufferDeviceAddressFeatures);  -- vulkan_core.h:5566

   type VkBufferDeviceAddressInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:5575
      pNext : System.Address;  -- vulkan_core.h:5576
      buffer : VkBuffer;  -- vulkan_core.h:5577
   end record;
   pragma Convention (C_Pass_By_Copy, VkBufferDeviceAddressInfo);  -- vulkan_core.h:5574

   type VkBufferOpaqueCaptureAddressCreateInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:5581
      pNext : System.Address;  -- vulkan_core.h:5582
      opaqueCaptureAddress : aliased stdint_h.uint64_t;  -- vulkan_core.h:5583
   end record;
   pragma Convention (C_Pass_By_Copy, VkBufferOpaqueCaptureAddressCreateInfo);  -- vulkan_core.h:5580

   type VkMemoryOpaqueCaptureAddressAllocateInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:5587
      pNext : System.Address;  -- vulkan_core.h:5588
      opaqueCaptureAddress : aliased stdint_h.uint64_t;  -- vulkan_core.h:5589
   end record;
   pragma Convention (C_Pass_By_Copy, VkMemoryOpaqueCaptureAddressAllocateInfo);  -- vulkan_core.h:5586

   type VkDeviceMemoryOpaqueCaptureAddressInfo is record
      sType : aliased VkStructureType;  -- vulkan_core.h:5593
      pNext : System.Address;  -- vulkan_core.h:5594
      memory : VkDeviceMemory;  -- vulkan_core.h:5595
   end record;
   pragma Convention (C_Pass_By_Copy, VkDeviceMemoryOpaqueCaptureAddressInfo);  -- vulkan_core.h:5592

   type PFN_vkCmdDrawIndirectCount is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkBuffer;
         arg3 : VkDeviceSize;
         arg4 : VkBuffer;
         arg5 : VkDeviceSize;
         arg6 : stdint_h.uint32_t;
         arg7 : stdint_h.uint32_t);
   pragma Convention (C, PFN_vkCmdDrawIndirectCount);  -- vulkan_core.h:5598

   type PFN_vkCmdDrawIndexedIndirectCount is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkBuffer;
         arg3 : VkDeviceSize;
         arg4 : VkBuffer;
         arg5 : VkDeviceSize;
         arg6 : stdint_h.uint32_t;
         arg7 : stdint_h.uint32_t);
   pragma Convention (C, PFN_vkCmdDrawIndexedIndirectCount);  -- vulkan_core.h:5599

   type PFN_vkCreateRenderPass2 is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : System.Address;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateRenderPass2);  -- vulkan_core.h:5600

   type PFN_vkCmdBeginRenderPass2 is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : System.Address;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkCmdBeginRenderPass2);  -- vulkan_core.h:5601

   type PFN_vkCmdNextSubpass2 is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : System.Address;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkCmdNextSubpass2);  -- vulkan_core.h:5602

   type PFN_vkCmdEndRenderPass2 is access procedure (arg1 : VkCommandBuffer; arg2 : System.Address);
   pragma Convention (C, PFN_vkCmdEndRenderPass2);  -- vulkan_core.h:5603

   type PFN_vkResetQueryPool is access procedure
        (arg1 : VkDevice;
         arg2 : VkQueryPool;
         arg3 : stdint_h.uint32_t;
         arg4 : stdint_h.uint32_t);
   pragma Convention (C, PFN_vkResetQueryPool);  -- vulkan_core.h:5604

   type PFN_vkGetSemaphoreCounterValue is access function
        (arg1 : VkDevice;
         arg2 : VkSemaphore;
         arg3 : access stdint_h.uint64_t) return VkResult;
   pragma Convention (C, PFN_vkGetSemaphoreCounterValue);  -- vulkan_core.h:5605

   type PFN_vkWaitSemaphores is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : stdint_h.uint64_t) return VkResult;
   pragma Convention (C, PFN_vkWaitSemaphores);  -- vulkan_core.h:5606

   type PFN_vkSignalSemaphore is access function (arg1 : VkDevice; arg2 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkSignalSemaphore);  -- vulkan_core.h:5607

   type PFN_vkGetBufferDeviceAddress is access function (arg1 : VkDevice; arg2 : System.Address) return VkDeviceAddress;
   pragma Convention (C, PFN_vkGetBufferDeviceAddress);  -- vulkan_core.h:5608

   type PFN_vkGetBufferOpaqueCaptureAddress is access function (arg1 : VkDevice; arg2 : System.Address) return stdint_h.uint64_t;
   pragma Convention (C, PFN_vkGetBufferOpaqueCaptureAddress);  -- vulkan_core.h:5609

   type PFN_vkGetDeviceMemoryOpaqueCaptureAddress is access function (arg1 : VkDevice; arg2 : System.Address) return stdint_h.uint64_t;
   pragma Convention (C, PFN_vkGetDeviceMemoryOpaqueCaptureAddress);  -- vulkan_core.h:5610

   procedure vkCmdDrawIndirectCount
     (commandBuffer : VkCommandBuffer;
      buffer : VkBuffer;
      offset : VkDeviceSize;
      countBuffer : VkBuffer;
      countBufferOffset : VkDeviceSize;
      maxDrawCount : stdint_h.uint32_t;
      stride : stdint_h.uint32_t);  -- vulkan_core.h:5613
   pragma Import (C, vkCmdDrawIndirectCount, "vkCmdDrawIndirectCount");

   procedure vkCmdDrawIndexedIndirectCount
     (commandBuffer : VkCommandBuffer;
      buffer : VkBuffer;
      offset : VkDeviceSize;
      countBuffer : VkBuffer;
      countBufferOffset : VkDeviceSize;
      maxDrawCount : stdint_h.uint32_t;
      stride : stdint_h.uint32_t);  -- vulkan_core.h:5622
   pragma Import (C, vkCmdDrawIndexedIndirectCount, "vkCmdDrawIndexedIndirectCount");

   function vkCreateRenderPass2
     (device : VkDevice;
      pCreateInfo : System.Address;
      pAllocator : System.Address;
      pRenderPass : System.Address) return VkResult;  -- vulkan_core.h:5631
   pragma Import (C, vkCreateRenderPass2, "vkCreateRenderPass2");

   procedure vkCmdBeginRenderPass2
     (commandBuffer : VkCommandBuffer;
      pRenderPassBegin : System.Address;
      pSubpassBeginInfo : System.Address);  -- vulkan_core.h:5637
   pragma Import (C, vkCmdBeginRenderPass2, "vkCmdBeginRenderPass2");

   procedure vkCmdNextSubpass2
     (commandBuffer : VkCommandBuffer;
      pSubpassBeginInfo : System.Address;
      pSubpassEndInfo : System.Address);  -- vulkan_core.h:5642
   pragma Import (C, vkCmdNextSubpass2, "vkCmdNextSubpass2");

   procedure vkCmdEndRenderPass2 (commandBuffer : VkCommandBuffer; pSubpassEndInfo : System.Address);  -- vulkan_core.h:5647
   pragma Import (C, vkCmdEndRenderPass2, "vkCmdEndRenderPass2");

   procedure vkResetQueryPool
     (device : VkDevice;
      queryPool : VkQueryPool;
      firstQuery : stdint_h.uint32_t;
      queryCount : stdint_h.uint32_t);  -- vulkan_core.h:5651
   pragma Import (C, vkResetQueryPool, "vkResetQueryPool");

   function vkGetSemaphoreCounterValue
     (device : VkDevice;
      semaphore : VkSemaphore;
      pValue : access stdint_h.uint64_t) return VkResult;  -- vulkan_core.h:5657
   pragma Import (C, vkGetSemaphoreCounterValue, "vkGetSemaphoreCounterValue");

   function vkWaitSemaphores
     (device : VkDevice;
      pWaitInfo : System.Address;
      timeout : stdint_h.uint64_t) return VkResult;  -- vulkan_core.h:5662
   pragma Import (C, vkWaitSemaphores, "vkWaitSemaphores");

   function vkSignalSemaphore (device : VkDevice; pSignalInfo : System.Address) return VkResult;  -- vulkan_core.h:5667
   pragma Import (C, vkSignalSemaphore, "vkSignalSemaphore");

   function vkGetBufferDeviceAddress (device : VkDevice; pInfo : System.Address) return VkDeviceAddress;  -- vulkan_core.h:5671
   pragma Import (C, vkGetBufferDeviceAddress, "vkGetBufferDeviceAddress");

   function vkGetBufferOpaqueCaptureAddress (device : VkDevice; pInfo : System.Address) return stdint_h.uint64_t;  -- vulkan_core.h:5675
   pragma Import (C, vkGetBufferOpaqueCaptureAddress, "vkGetBufferOpaqueCaptureAddress");

   function vkGetDeviceMemoryOpaqueCaptureAddress (device : VkDevice; pInfo : System.Address) return stdint_h.uint64_t;  -- vulkan_core.h:5679
   pragma Import (C, vkGetDeviceMemoryOpaqueCaptureAddress, "vkGetDeviceMemoryOpaqueCaptureAddress");

   type VkSurfaceKHR is new System.Address;  -- vulkan_core.h:5686

   --  skipped empty struct VkSurfaceKHR_T

   subtype VkPresentModeKHR is unsigned;
   VK_PRESENT_MODE_IMMEDIATE_KHR : constant VkPresentModeKHR := 0;
   VK_PRESENT_MODE_MAILBOX_KHR : constant VkPresentModeKHR := 1;
   VK_PRESENT_MODE_FIFO_KHR : constant VkPresentModeKHR := 2;
   VK_PRESENT_MODE_FIFO_RELAXED_KHR : constant VkPresentModeKHR := 3;
   VK_PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR : constant VkPresentModeKHR := 1000111000;
   VK_PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR : constant VkPresentModeKHR := 1000111001;
   VK_PRESENT_MODE_MAX_ENUM_KHR : constant VkPresentModeKHR := 2147483647;  -- vulkan_core.h:5690

   subtype VkColorSpaceKHR is unsigned;
   VK_COLOR_SPACE_SRGB_NONLINEAR_KHR : constant VkColorSpaceKHR := 0;
   VK_COLOR_SPACE_DISPLAY_P3_NONLINEAR_EXT : constant VkColorSpaceKHR := 1000104001;
   VK_COLOR_SPACE_EXTENDED_SRGB_LINEAR_EXT : constant VkColorSpaceKHR := 1000104002;
   VK_COLOR_SPACE_DISPLAY_P3_LINEAR_EXT : constant VkColorSpaceKHR := 1000104003;
   VK_COLOR_SPACE_DCI_P3_NONLINEAR_EXT : constant VkColorSpaceKHR := 1000104004;
   VK_COLOR_SPACE_BT709_LINEAR_EXT : constant VkColorSpaceKHR := 1000104005;
   VK_COLOR_SPACE_BT709_NONLINEAR_EXT : constant VkColorSpaceKHR := 1000104006;
   VK_COLOR_SPACE_BT2020_LINEAR_EXT : constant VkColorSpaceKHR := 1000104007;
   VK_COLOR_SPACE_HDR10_ST2084_EXT : constant VkColorSpaceKHR := 1000104008;
   VK_COLOR_SPACE_DOLBYVISION_EXT : constant VkColorSpaceKHR := 1000104009;
   VK_COLOR_SPACE_HDR10_HLG_EXT : constant VkColorSpaceKHR := 1000104010;
   VK_COLOR_SPACE_ADOBERGB_LINEAR_EXT : constant VkColorSpaceKHR := 1000104011;
   VK_COLOR_SPACE_ADOBERGB_NONLINEAR_EXT : constant VkColorSpaceKHR := 1000104012;
   VK_COLOR_SPACE_PASS_THROUGH_EXT : constant VkColorSpaceKHR := 1000104013;
   VK_COLOR_SPACE_EXTENDED_SRGB_NONLINEAR_EXT : constant VkColorSpaceKHR := 1000104014;
   VK_COLOR_SPACE_DISPLAY_NATIVE_AMD : constant VkColorSpaceKHR := 1000213000;
   VK_COLORSPACE_SRGB_NONLINEAR_KHR : constant VkColorSpaceKHR := 0;
   VK_COLOR_SPACE_DCI_P3_LINEAR_EXT : constant VkColorSpaceKHR := 1000104003;
   VK_COLOR_SPACE_MAX_ENUM_KHR : constant VkColorSpaceKHR := 2147483647;  -- vulkan_core.h:5700

   subtype VkSurfaceTransformFlagBitsKHR is unsigned;
   VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR : constant VkSurfaceTransformFlagBitsKHR := 1;
   VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR : constant VkSurfaceTransformFlagBitsKHR := 2;
   VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR : constant VkSurfaceTransformFlagBitsKHR := 4;
   VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR : constant VkSurfaceTransformFlagBitsKHR := 8;
   VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR : constant VkSurfaceTransformFlagBitsKHR := 16;
   VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR : constant VkSurfaceTransformFlagBitsKHR := 32;
   VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR : constant VkSurfaceTransformFlagBitsKHR := 64;
   VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR : constant VkSurfaceTransformFlagBitsKHR := 128;
   VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR : constant VkSurfaceTransformFlagBitsKHR := 256;
   VK_SURFACE_TRANSFORM_FLAG_BITS_MAX_ENUM_KHR : constant VkSurfaceTransformFlagBitsKHR := 2147483647;  -- vulkan_core.h:5722

   subtype VkCompositeAlphaFlagBitsKHR is unsigned;
   VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR : constant VkCompositeAlphaFlagBitsKHR := 1;
   VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR : constant VkCompositeAlphaFlagBitsKHR := 2;
   VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR : constant VkCompositeAlphaFlagBitsKHR := 4;
   VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR : constant VkCompositeAlphaFlagBitsKHR := 8;
   VK_COMPOSITE_ALPHA_FLAG_BITS_MAX_ENUM_KHR : constant VkCompositeAlphaFlagBitsKHR := 2147483647;  -- vulkan_core.h:5735

   subtype VkCompositeAlphaFlagsKHR is VkFlags;  -- vulkan_core.h:5742

   subtype VkSurfaceTransformFlagsKHR is VkFlags;  -- vulkan_core.h:5743

   type VkSurfaceCapabilitiesKHR is record
      minImageCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:5745
      maxImageCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:5746
      currentExtent : aliased VkExtent2D;  -- vulkan_core.h:5747
      minImageExtent : aliased VkExtent2D;  -- vulkan_core.h:5748
      maxImageExtent : aliased VkExtent2D;  -- vulkan_core.h:5749
      maxImageArrayLayers : aliased stdint_h.uint32_t;  -- vulkan_core.h:5750
      supportedTransforms : aliased VkSurfaceTransformFlagsKHR;  -- vulkan_core.h:5751
      currentTransform : aliased VkSurfaceTransformFlagBitsKHR;  -- vulkan_core.h:5752
      supportedCompositeAlpha : aliased VkCompositeAlphaFlagsKHR;  -- vulkan_core.h:5753
      supportedUsageFlags : aliased VkImageUsageFlags;  -- vulkan_core.h:5754
   end record;
   pragma Convention (C_Pass_By_Copy, VkSurfaceCapabilitiesKHR);  -- vulkan_core.h:5744

   type VkSurfaceFormatKHR is record
      format : aliased VkFormat;  -- vulkan_core.h:5758
      colorSpace : aliased VkColorSpaceKHR;  -- vulkan_core.h:5759
   end record;
   pragma Convention (C_Pass_By_Copy, VkSurfaceFormatKHR);  -- vulkan_core.h:5757

   type PFN_vkDestroySurfaceKHR is access procedure
        (arg1 : VkInstance;
         arg2 : VkSurfaceKHR;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkDestroySurfaceKHR);  -- vulkan_core.h:5762

   type PFN_vkGetPhysicalDeviceSurfaceSupportKHR is access function
        (arg1 : VkPhysicalDevice;
         arg2 : stdint_h.uint32_t;
         arg3 : VkSurfaceKHR;
         arg4 : access VkBool32) return VkResult;
   pragma Convention (C, PFN_vkGetPhysicalDeviceSurfaceSupportKHR);  -- vulkan_core.h:5763

   type PFN_vkGetPhysicalDeviceSurfaceCapabilitiesKHR is access function
        (arg1 : VkPhysicalDevice;
         arg2 : VkSurfaceKHR;
         arg3 : access VkSurfaceCapabilitiesKHR) return VkResult;
   pragma Convention (C, PFN_vkGetPhysicalDeviceSurfaceCapabilitiesKHR);  -- vulkan_core.h:5764

   type PFN_vkGetPhysicalDeviceSurfaceFormatsKHR is access function
        (arg1 : VkPhysicalDevice;
         arg2 : VkSurfaceKHR;
         arg3 : access stdint_h.uint32_t;
         arg4 : access VkSurfaceFormatKHR) return VkResult;
   pragma Convention (C, PFN_vkGetPhysicalDeviceSurfaceFormatsKHR);  -- vulkan_core.h:5765

   type PFN_vkGetPhysicalDeviceSurfacePresentModesKHR is access function
        (arg1 : VkPhysicalDevice;
         arg2 : VkSurfaceKHR;
         arg3 : access stdint_h.uint32_t;
         arg4 : access VkPresentModeKHR) return VkResult;
   pragma Convention (C, PFN_vkGetPhysicalDeviceSurfacePresentModesKHR);  -- vulkan_core.h:5766

   procedure vkDestroySurfaceKHR
     (instance : VkInstance;
      surface : VkSurfaceKHR;
      pAllocator : System.Address);  -- vulkan_core.h:5769
   pragma Import (C, vkDestroySurfaceKHR, "vkDestroySurfaceKHR");

   function vkGetPhysicalDeviceSurfaceSupportKHR
     (physicalDevice : VkPhysicalDevice;
      queueFamilyIndex : stdint_h.uint32_t;
      surface : VkSurfaceKHR;
      pSupported : access VkBool32) return VkResult;  -- vulkan_core.h:5774
   pragma Import (C, vkGetPhysicalDeviceSurfaceSupportKHR, "vkGetPhysicalDeviceSurfaceSupportKHR");

   function vkGetPhysicalDeviceSurfaceCapabilitiesKHR
     (physicalDevice : VkPhysicalDevice;
      surface : VkSurfaceKHR;
      pSurfaceCapabilities : access VkSurfaceCapabilitiesKHR) return VkResult;  -- vulkan_core.h:5780
   pragma Import (C, vkGetPhysicalDeviceSurfaceCapabilitiesKHR, "vkGetPhysicalDeviceSurfaceCapabilitiesKHR");

   function vkGetPhysicalDeviceSurfaceFormatsKHR
     (physicalDevice : VkPhysicalDevice;
      surface : VkSurfaceKHR;
      pSurfaceFormatCount : access stdint_h.uint32_t;
      pSurfaceFormats : access VkSurfaceFormatKHR) return VkResult;  -- vulkan_core.h:5785
   pragma Import (C, vkGetPhysicalDeviceSurfaceFormatsKHR, "vkGetPhysicalDeviceSurfaceFormatsKHR");

   function vkGetPhysicalDeviceSurfacePresentModesKHR
     (physicalDevice : VkPhysicalDevice;
      surface : VkSurfaceKHR;
      pPresentModeCount : access stdint_h.uint32_t;
      pPresentModes : access VkPresentModeKHR) return VkResult;  -- vulkan_core.h:5791
   pragma Import (C, vkGetPhysicalDeviceSurfacePresentModesKHR, "vkGetPhysicalDeviceSurfacePresentModesKHR");

   --  skipped empty struct VkSwapchainKHR_T

   type VkSwapchainKHR is new System.Address;  -- vulkan_core.h:5800

   subtype VkSwapchainCreateFlagBitsKHR is unsigned;
   VK_SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR : constant VkSwapchainCreateFlagBitsKHR := 1;
   VK_SWAPCHAIN_CREATE_PROTECTED_BIT_KHR : constant VkSwapchainCreateFlagBitsKHR := 2;
   VK_SWAPCHAIN_CREATE_MUTABLE_FORMAT_BIT_KHR : constant VkSwapchainCreateFlagBitsKHR := 4;
   VK_SWAPCHAIN_CREATE_FLAG_BITS_MAX_ENUM_KHR : constant VkSwapchainCreateFlagBitsKHR := 2147483647;  -- vulkan_core.h:5804

   subtype VkSwapchainCreateFlagsKHR is VkFlags;  -- vulkan_core.h:5810

   subtype VkDeviceGroupPresentModeFlagBitsKHR is unsigned;
   VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR : constant VkDeviceGroupPresentModeFlagBitsKHR := 1;
   VK_DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHR : constant VkDeviceGroupPresentModeFlagBitsKHR := 2;
   VK_DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHR : constant VkDeviceGroupPresentModeFlagBitsKHR := 4;
   VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHR : constant VkDeviceGroupPresentModeFlagBitsKHR := 8;
   VK_DEVICE_GROUP_PRESENT_MODE_FLAG_BITS_MAX_ENUM_KHR : constant VkDeviceGroupPresentModeFlagBitsKHR := 2147483647;  -- vulkan_core.h:5812

   subtype VkDeviceGroupPresentModeFlagsKHR is VkFlags;  -- vulkan_core.h:5819

   type VkSwapchainCreateInfoKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:5821
      pNext : System.Address;  -- vulkan_core.h:5822
      flags : aliased VkSwapchainCreateFlagsKHR;  -- vulkan_core.h:5823
      surface : VkSurfaceKHR;  -- vulkan_core.h:5824
      minImageCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:5825
      imageFormat : aliased VkFormat;  -- vulkan_core.h:5826
      imageColorSpace : aliased VkColorSpaceKHR;  -- vulkan_core.h:5827
      imageExtent : aliased VkExtent2D;  -- vulkan_core.h:5828
      imageArrayLayers : aliased stdint_h.uint32_t;  -- vulkan_core.h:5829
      imageUsage : aliased VkImageUsageFlags;  -- vulkan_core.h:5830
      imageSharingMode : aliased VkSharingMode;  -- vulkan_core.h:5831
      queueFamilyIndexCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:5832
      pQueueFamilyIndices : access stdint_h.uint32_t;  -- vulkan_core.h:5833
      preTransform : aliased VkSurfaceTransformFlagBitsKHR;  -- vulkan_core.h:5834
      compositeAlpha : aliased VkCompositeAlphaFlagBitsKHR;  -- vulkan_core.h:5835
      presentMode : aliased VkPresentModeKHR;  -- vulkan_core.h:5836
      clipped : aliased VkBool32;  -- vulkan_core.h:5837
      oldSwapchain : VkSwapchainKHR;  -- vulkan_core.h:5838
   end record;
   pragma Convention (C_Pass_By_Copy, VkSwapchainCreateInfoKHR);  -- vulkan_core.h:5820

   type VkPresentInfoKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:5842
      pNext : System.Address;  -- vulkan_core.h:5843
      waitSemaphoreCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:5844
      pWaitSemaphores : System.Address;  -- vulkan_core.h:5845
      swapchainCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:5846
      pSwapchains : System.Address;  -- vulkan_core.h:5847
      pImageIndices : access stdint_h.uint32_t;  -- vulkan_core.h:5848
      pResults : access VkResult;  -- vulkan_core.h:5849
   end record;
   pragma Convention (C_Pass_By_Copy, VkPresentInfoKHR);  -- vulkan_core.h:5841

   type VkImageSwapchainCreateInfoKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:5853
      pNext : System.Address;  -- vulkan_core.h:5854
      swapchain : VkSwapchainKHR;  -- vulkan_core.h:5855
   end record;
   pragma Convention (C_Pass_By_Copy, VkImageSwapchainCreateInfoKHR);  -- vulkan_core.h:5852

   type VkBindImageMemorySwapchainInfoKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:5859
      pNext : System.Address;  -- vulkan_core.h:5860
      swapchain : VkSwapchainKHR;  -- vulkan_core.h:5861
      imageIndex : aliased stdint_h.uint32_t;  -- vulkan_core.h:5862
   end record;
   pragma Convention (C_Pass_By_Copy, VkBindImageMemorySwapchainInfoKHR);  -- vulkan_core.h:5858

   type VkAcquireNextImageInfoKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:5866
      pNext : System.Address;  -- vulkan_core.h:5867
      swapchain : VkSwapchainKHR;  -- vulkan_core.h:5868
      timeout : aliased stdint_h.uint64_t;  -- vulkan_core.h:5869
      semaphore : VkSemaphore;  -- vulkan_core.h:5870
      fence : VkFence;  -- vulkan_core.h:5871
      deviceMask : aliased stdint_h.uint32_t;  -- vulkan_core.h:5872
   end record;
   pragma Convention (C_Pass_By_Copy, VkAcquireNextImageInfoKHR);  -- vulkan_core.h:5865

   type VkDeviceGroupPresentCapabilitiesKHR_presentMask_array is array (0 .. 31) of aliased stdint_h.uint32_t;
   type VkDeviceGroupPresentCapabilitiesKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:5876
      pNext : System.Address;  -- vulkan_core.h:5877
      presentMask : aliased VkDeviceGroupPresentCapabilitiesKHR_presentMask_array;  -- vulkan_core.h:5878
      modes : aliased VkDeviceGroupPresentModeFlagsKHR;  -- vulkan_core.h:5879
   end record;
   pragma Convention (C_Pass_By_Copy, VkDeviceGroupPresentCapabilitiesKHR);  -- vulkan_core.h:5875

   type VkDeviceGroupPresentInfoKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:5883
      pNext : System.Address;  -- vulkan_core.h:5884
      swapchainCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:5885
      pDeviceMasks : access stdint_h.uint32_t;  -- vulkan_core.h:5886
      mode : aliased VkDeviceGroupPresentModeFlagBitsKHR;  -- vulkan_core.h:5887
   end record;
   pragma Convention (C_Pass_By_Copy, VkDeviceGroupPresentInfoKHR);  -- vulkan_core.h:5882

   type VkDeviceGroupSwapchainCreateInfoKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:5891
      pNext : System.Address;  -- vulkan_core.h:5892
      modes : aliased VkDeviceGroupPresentModeFlagsKHR;  -- vulkan_core.h:5893
   end record;
   pragma Convention (C_Pass_By_Copy, VkDeviceGroupSwapchainCreateInfoKHR);  -- vulkan_core.h:5890

   type PFN_vkCreateSwapchainKHR is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : System.Address;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateSwapchainKHR);  -- vulkan_core.h:5896

   type PFN_vkDestroySwapchainKHR is access procedure
        (arg1 : VkDevice;
         arg2 : VkSwapchainKHR;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkDestroySwapchainKHR);  -- vulkan_core.h:5897

   type PFN_vkGetSwapchainImagesKHR is access function
        (arg1 : VkDevice;
         arg2 : VkSwapchainKHR;
         arg3 : access stdint_h.uint32_t;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkGetSwapchainImagesKHR);  -- vulkan_core.h:5898

   type PFN_vkAcquireNextImageKHR is access function
        (arg1 : VkDevice;
         arg2 : VkSwapchainKHR;
         arg3 : stdint_h.uint64_t;
         arg4 : VkSemaphore;
         arg5 : VkFence;
         arg6 : access stdint_h.uint32_t) return VkResult;
   pragma Convention (C, PFN_vkAcquireNextImageKHR);  -- vulkan_core.h:5899

   type PFN_vkQueuePresentKHR is access function (arg1 : VkQueue; arg2 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkQueuePresentKHR);  -- vulkan_core.h:5900

   type PFN_vkGetDeviceGroupPresentCapabilitiesKHR is access function (arg1 : VkDevice; arg2 : access VkDeviceGroupPresentCapabilitiesKHR) return VkResult;
   pragma Convention (C, PFN_vkGetDeviceGroupPresentCapabilitiesKHR);  -- vulkan_core.h:5901

   type PFN_vkGetDeviceGroupSurfacePresentModesKHR is access function
        (arg1 : VkDevice;
         arg2 : VkSurfaceKHR;
         arg3 : access VkDeviceGroupPresentModeFlagsKHR) return VkResult;
   pragma Convention (C, PFN_vkGetDeviceGroupSurfacePresentModesKHR);  -- vulkan_core.h:5902

   type PFN_vkGetPhysicalDevicePresentRectanglesKHR is access function
        (arg1 : VkPhysicalDevice;
         arg2 : VkSurfaceKHR;
         arg3 : access stdint_h.uint32_t;
         arg4 : access VkRect2D) return VkResult;
   pragma Convention (C, PFN_vkGetPhysicalDevicePresentRectanglesKHR);  -- vulkan_core.h:5903

   type PFN_vkAcquireNextImage2KHR is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : access stdint_h.uint32_t) return VkResult;
   pragma Convention (C, PFN_vkAcquireNextImage2KHR);  -- vulkan_core.h:5904

   function vkCreateSwapchainKHR
     (device : VkDevice;
      pCreateInfo : System.Address;
      pAllocator : System.Address;
      pSwapchain : System.Address) return VkResult;  -- vulkan_core.h:5907
   pragma Import (C, vkCreateSwapchainKHR, "vkCreateSwapchainKHR");

   procedure vkDestroySwapchainKHR
     (device : VkDevice;
      swapchain : VkSwapchainKHR;
      pAllocator : System.Address);  -- vulkan_core.h:5913
   pragma Import (C, vkDestroySwapchainKHR, "vkDestroySwapchainKHR");

   function vkGetSwapchainImagesKHR
     (device : VkDevice;
      swapchain : VkSwapchainKHR;
      pSwapchainImageCount : access stdint_h.uint32_t;
      pSwapchainImages : System.Address) return VkResult;  -- vulkan_core.h:5918
   pragma Import (C, vkGetSwapchainImagesKHR, "vkGetSwapchainImagesKHR");

   function vkAcquireNextImageKHR
     (device : VkDevice;
      swapchain : VkSwapchainKHR;
      timeout : stdint_h.uint64_t;
      semaphore : VkSemaphore;
      fence : VkFence;
      pImageIndex : access stdint_h.uint32_t) return VkResult;  -- vulkan_core.h:5924
   pragma Import (C, vkAcquireNextImageKHR, "vkAcquireNextImageKHR");

   function vkQueuePresentKHR (queue : VkQueue; pPresentInfo : System.Address) return VkResult;  -- vulkan_core.h:5932
   pragma Import (C, vkQueuePresentKHR, "vkQueuePresentKHR");

   function vkGetDeviceGroupPresentCapabilitiesKHR (device : VkDevice; pDeviceGroupPresentCapabilities : access VkDeviceGroupPresentCapabilitiesKHR) return VkResult;  -- vulkan_core.h:5936
   pragma Import (C, vkGetDeviceGroupPresentCapabilitiesKHR, "vkGetDeviceGroupPresentCapabilitiesKHR");

   function vkGetDeviceGroupSurfacePresentModesKHR
     (device : VkDevice;
      surface : VkSurfaceKHR;
      pModes : access VkDeviceGroupPresentModeFlagsKHR) return VkResult;  -- vulkan_core.h:5940
   pragma Import (C, vkGetDeviceGroupSurfacePresentModesKHR, "vkGetDeviceGroupSurfacePresentModesKHR");

   function vkGetPhysicalDevicePresentRectanglesKHR
     (physicalDevice : VkPhysicalDevice;
      surface : VkSurfaceKHR;
      pRectCount : access stdint_h.uint32_t;
      pRects : access VkRect2D) return VkResult;  -- vulkan_core.h:5945
   pragma Import (C, vkGetPhysicalDevicePresentRectanglesKHR, "vkGetPhysicalDevicePresentRectanglesKHR");

   function vkAcquireNextImage2KHR
     (device : VkDevice;
      pAcquireInfo : System.Address;
      pImageIndex : access stdint_h.uint32_t) return VkResult;  -- vulkan_core.h:5951
   pragma Import (C, vkAcquireNextImage2KHR, "vkAcquireNextImage2KHR");

   type VkDisplayKHR is new System.Address;  -- vulkan_core.h:5959

   --  skipped empty struct VkDisplayKHR_T

   --  skipped empty struct VkDisplayModeKHR_T

   type VkDisplayModeKHR is new System.Address;  -- vulkan_core.h:5960

   subtype VkDisplayModeCreateFlagsKHR is VkFlags;  -- vulkan_core.h:5963

   subtype VkDisplayPlaneAlphaFlagBitsKHR is unsigned;
   VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR : constant VkDisplayPlaneAlphaFlagBitsKHR := 1;
   VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR : constant VkDisplayPlaneAlphaFlagBitsKHR := 2;
   VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR : constant VkDisplayPlaneAlphaFlagBitsKHR := 4;
   VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR : constant VkDisplayPlaneAlphaFlagBitsKHR := 8;
   VK_DISPLAY_PLANE_ALPHA_FLAG_BITS_MAX_ENUM_KHR : constant VkDisplayPlaneAlphaFlagBitsKHR := 2147483647;  -- vulkan_core.h:5965

   subtype VkDisplayPlaneAlphaFlagsKHR is VkFlags;  -- vulkan_core.h:5972

   subtype VkDisplaySurfaceCreateFlagsKHR is VkFlags;  -- vulkan_core.h:5973

   type VkDisplayModeParametersKHR is record
      visibleRegion : aliased VkExtent2D;  -- vulkan_core.h:5975
      refreshRate : aliased stdint_h.uint32_t;  -- vulkan_core.h:5976
   end record;
   pragma Convention (C_Pass_By_Copy, VkDisplayModeParametersKHR);  -- vulkan_core.h:5974

   type VkDisplayModeCreateInfoKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:5980
      pNext : System.Address;  -- vulkan_core.h:5981
      flags : aliased VkDisplayModeCreateFlagsKHR;  -- vulkan_core.h:5982
      parameters : aliased VkDisplayModeParametersKHR;  -- vulkan_core.h:5983
   end record;
   pragma Convention (C_Pass_By_Copy, VkDisplayModeCreateInfoKHR);  -- vulkan_core.h:5979

   type VkDisplayModePropertiesKHR is record
      displayMode : VkDisplayModeKHR;  -- vulkan_core.h:5987
      parameters : aliased VkDisplayModeParametersKHR;  -- vulkan_core.h:5988
   end record;
   pragma Convention (C_Pass_By_Copy, VkDisplayModePropertiesKHR);  -- vulkan_core.h:5986

   type VkDisplayPlaneCapabilitiesKHR is record
      supportedAlpha : aliased VkDisplayPlaneAlphaFlagsKHR;  -- vulkan_core.h:5992
      minSrcPosition : aliased VkOffset2D;  -- vulkan_core.h:5993
      maxSrcPosition : aliased VkOffset2D;  -- vulkan_core.h:5994
      minSrcExtent : aliased VkExtent2D;  -- vulkan_core.h:5995
      maxSrcExtent : aliased VkExtent2D;  -- vulkan_core.h:5996
      minDstPosition : aliased VkOffset2D;  -- vulkan_core.h:5997
      maxDstPosition : aliased VkOffset2D;  -- vulkan_core.h:5998
      minDstExtent : aliased VkExtent2D;  -- vulkan_core.h:5999
      maxDstExtent : aliased VkExtent2D;  -- vulkan_core.h:6000
   end record;
   pragma Convention (C_Pass_By_Copy, VkDisplayPlaneCapabilitiesKHR);  -- vulkan_core.h:5991

   type VkDisplayPlanePropertiesKHR is record
      currentDisplay : VkDisplayKHR;  -- vulkan_core.h:6004
      currentStackIndex : aliased stdint_h.uint32_t;  -- vulkan_core.h:6005
   end record;
   pragma Convention (C_Pass_By_Copy, VkDisplayPlanePropertiesKHR);  -- vulkan_core.h:6003

   type VkDisplayPropertiesKHR is record
      display : VkDisplayKHR;  -- vulkan_core.h:6009
      displayName : Interfaces.C.Strings.chars_ptr;  -- vulkan_core.h:6010
      physicalDimensions : aliased VkExtent2D;  -- vulkan_core.h:6011
      physicalResolution : aliased VkExtent2D;  -- vulkan_core.h:6012
      supportedTransforms : aliased VkSurfaceTransformFlagsKHR;  -- vulkan_core.h:6013
      planeReorderPossible : aliased VkBool32;  -- vulkan_core.h:6014
      persistentContent : aliased VkBool32;  -- vulkan_core.h:6015
   end record;
   pragma Convention (C_Pass_By_Copy, VkDisplayPropertiesKHR);  -- vulkan_core.h:6008

   type VkDisplaySurfaceCreateInfoKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:6019
      pNext : System.Address;  -- vulkan_core.h:6020
      flags : aliased VkDisplaySurfaceCreateFlagsKHR;  -- vulkan_core.h:6021
      displayMode : VkDisplayModeKHR;  -- vulkan_core.h:6022
      planeIndex : aliased stdint_h.uint32_t;  -- vulkan_core.h:6023
      planeStackIndex : aliased stdint_h.uint32_t;  -- vulkan_core.h:6024
      transform : aliased VkSurfaceTransformFlagBitsKHR;  -- vulkan_core.h:6025
      globalAlpha : aliased float;  -- vulkan_core.h:6026
      alphaMode : aliased VkDisplayPlaneAlphaFlagBitsKHR;  -- vulkan_core.h:6027
      imageExtent : aliased VkExtent2D;  -- vulkan_core.h:6028
   end record;
   pragma Convention (C_Pass_By_Copy, VkDisplaySurfaceCreateInfoKHR);  -- vulkan_core.h:6018

   type PFN_vkGetPhysicalDeviceDisplayPropertiesKHR is access function
        (arg1 : VkPhysicalDevice;
         arg2 : access stdint_h.uint32_t;
         arg3 : access VkDisplayPropertiesKHR) return VkResult;
   pragma Convention (C, PFN_vkGetPhysicalDeviceDisplayPropertiesKHR);  -- vulkan_core.h:6031

   type PFN_vkGetPhysicalDeviceDisplayPlanePropertiesKHR is access function
        (arg1 : VkPhysicalDevice;
         arg2 : access stdint_h.uint32_t;
         arg3 : access VkDisplayPlanePropertiesKHR) return VkResult;
   pragma Convention (C, PFN_vkGetPhysicalDeviceDisplayPlanePropertiesKHR);  -- vulkan_core.h:6032

   type PFN_vkGetDisplayPlaneSupportedDisplaysKHR is access function
        (arg1 : VkPhysicalDevice;
         arg2 : stdint_h.uint32_t;
         arg3 : access stdint_h.uint32_t;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkGetDisplayPlaneSupportedDisplaysKHR);  -- vulkan_core.h:6033

   type PFN_vkGetDisplayModePropertiesKHR is access function
        (arg1 : VkPhysicalDevice;
         arg2 : VkDisplayKHR;
         arg3 : access stdint_h.uint32_t;
         arg4 : access VkDisplayModePropertiesKHR) return VkResult;
   pragma Convention (C, PFN_vkGetDisplayModePropertiesKHR);  -- vulkan_core.h:6034

   type PFN_vkCreateDisplayModeKHR is access function
        (arg1 : VkPhysicalDevice;
         arg2 : VkDisplayKHR;
         arg3 : System.Address;
         arg4 : System.Address;
         arg5 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateDisplayModeKHR);  -- vulkan_core.h:6035

   type PFN_vkGetDisplayPlaneCapabilitiesKHR is access function
        (arg1 : VkPhysicalDevice;
         arg2 : VkDisplayModeKHR;
         arg3 : stdint_h.uint32_t;
         arg4 : access VkDisplayPlaneCapabilitiesKHR) return VkResult;
   pragma Convention (C, PFN_vkGetDisplayPlaneCapabilitiesKHR);  -- vulkan_core.h:6036

   type PFN_vkCreateDisplayPlaneSurfaceKHR is access function
        (arg1 : VkInstance;
         arg2 : System.Address;
         arg3 : System.Address;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateDisplayPlaneSurfaceKHR);  -- vulkan_core.h:6037

   function vkGetPhysicalDeviceDisplayPropertiesKHR
     (physicalDevice : VkPhysicalDevice;
      pPropertyCount : access stdint_h.uint32_t;
      pProperties : access VkDisplayPropertiesKHR) return VkResult;  -- vulkan_core.h:6040
   pragma Import (C, vkGetPhysicalDeviceDisplayPropertiesKHR, "vkGetPhysicalDeviceDisplayPropertiesKHR");

   function vkGetPhysicalDeviceDisplayPlanePropertiesKHR
     (physicalDevice : VkPhysicalDevice;
      pPropertyCount : access stdint_h.uint32_t;
      pProperties : access VkDisplayPlanePropertiesKHR) return VkResult;  -- vulkan_core.h:6045
   pragma Import (C, vkGetPhysicalDeviceDisplayPlanePropertiesKHR, "vkGetPhysicalDeviceDisplayPlanePropertiesKHR");

   function vkGetDisplayPlaneSupportedDisplaysKHR
     (physicalDevice : VkPhysicalDevice;
      planeIndex : stdint_h.uint32_t;
      pDisplayCount : access stdint_h.uint32_t;
      pDisplays : System.Address) return VkResult;  -- vulkan_core.h:6050
   pragma Import (C, vkGetDisplayPlaneSupportedDisplaysKHR, "vkGetDisplayPlaneSupportedDisplaysKHR");

   function vkGetDisplayModePropertiesKHR
     (physicalDevice : VkPhysicalDevice;
      display : VkDisplayKHR;
      pPropertyCount : access stdint_h.uint32_t;
      pProperties : access VkDisplayModePropertiesKHR) return VkResult;  -- vulkan_core.h:6056
   pragma Import (C, vkGetDisplayModePropertiesKHR, "vkGetDisplayModePropertiesKHR");

   function vkCreateDisplayModeKHR
     (physicalDevice : VkPhysicalDevice;
      display : VkDisplayKHR;
      pCreateInfo : System.Address;
      pAllocator : System.Address;
      pMode : System.Address) return VkResult;  -- vulkan_core.h:6062
   pragma Import (C, vkCreateDisplayModeKHR, "vkCreateDisplayModeKHR");

   function vkGetDisplayPlaneCapabilitiesKHR
     (physicalDevice : VkPhysicalDevice;
      mode : VkDisplayModeKHR;
      planeIndex : stdint_h.uint32_t;
      pCapabilities : access VkDisplayPlaneCapabilitiesKHR) return VkResult;  -- vulkan_core.h:6069
   pragma Import (C, vkGetDisplayPlaneCapabilitiesKHR, "vkGetDisplayPlaneCapabilitiesKHR");

   function vkCreateDisplayPlaneSurfaceKHR
     (instance : VkInstance;
      pCreateInfo : System.Address;
      pAllocator : System.Address;
      pSurface : System.Address) return VkResult;  -- vulkan_core.h:6075
   pragma Import (C, vkCreateDisplayPlaneSurfaceKHR, "vkCreateDisplayPlaneSurfaceKHR");

   type VkDisplayPresentInfoKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:6087
      pNext : System.Address;  -- vulkan_core.h:6088
      srcRect : aliased VkRect2D;  -- vulkan_core.h:6089
      dstRect : aliased VkRect2D;  -- vulkan_core.h:6090
      persistent : aliased VkBool32;  -- vulkan_core.h:6091
   end record;
   pragma Convention (C_Pass_By_Copy, VkDisplayPresentInfoKHR);  -- vulkan_core.h:6086

   type PFN_vkCreateSharedSwapchainsKHR is access function
        (arg1 : VkDevice;
         arg2 : stdint_h.uint32_t;
         arg3 : System.Address;
         arg4 : System.Address;
         arg5 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateSharedSwapchainsKHR);  -- vulkan_core.h:6094

   function vkCreateSharedSwapchainsKHR
     (device : VkDevice;
      swapchainCount : stdint_h.uint32_t;
      pCreateInfos : System.Address;
      pAllocator : System.Address;
      pSwapchains : System.Address) return VkResult;  -- vulkan_core.h:6097
   pragma Import (C, vkCreateSharedSwapchainsKHR, "vkCreateSharedSwapchainsKHR");

   subtype VkRenderPassMultiviewCreateInfoKHR is VkRenderPassMultiviewCreateInfo;

   subtype VkPhysicalDeviceMultiviewFeaturesKHR is VkPhysicalDeviceMultiviewFeatures;

   subtype VkPhysicalDeviceMultiviewPropertiesKHR is VkPhysicalDeviceMultiviewProperties;

   subtype VkPhysicalDeviceFeatures2KHR is VkPhysicalDeviceFeatures2;

   subtype VkPhysicalDeviceProperties2KHR is VkPhysicalDeviceProperties2;

   subtype VkFormatProperties2KHR is VkFormatProperties2;

   subtype VkImageFormatProperties2KHR is VkImageFormatProperties2;

   subtype VkPhysicalDeviceImageFormatInfo2KHR is VkPhysicalDeviceImageFormatInfo2;

   subtype VkQueueFamilyProperties2KHR is VkQueueFamilyProperties2;

   subtype VkPhysicalDeviceMemoryProperties2KHR is VkPhysicalDeviceMemoryProperties2;

   subtype VkSparseImageFormatProperties2KHR is VkSparseImageFormatProperties2;

   subtype VkPhysicalDeviceSparseImageFormatInfo2KHR is VkPhysicalDeviceSparseImageFormatInfo2;

   type PFN_vkGetPhysicalDeviceFeatures2KHR is access procedure (arg1 : VkPhysicalDevice; arg2 : access VkPhysicalDeviceFeatures2);
   pragma Convention (C, PFN_vkGetPhysicalDeviceFeatures2KHR);  -- vulkan_core.h:6143

   type PFN_vkGetPhysicalDeviceProperties2KHR is access procedure (arg1 : VkPhysicalDevice; arg2 : access VkPhysicalDeviceProperties2);
   pragma Convention (C, PFN_vkGetPhysicalDeviceProperties2KHR);  -- vulkan_core.h:6144

   type PFN_vkGetPhysicalDeviceFormatProperties2KHR is access procedure
        (arg1 : VkPhysicalDevice;
         arg2 : VkFormat;
         arg3 : access VkFormatProperties2);
   pragma Convention (C, PFN_vkGetPhysicalDeviceFormatProperties2KHR);  -- vulkan_core.h:6145

   type PFN_vkGetPhysicalDeviceImageFormatProperties2KHR is access function
        (arg1 : VkPhysicalDevice;
         arg2 : System.Address;
         arg3 : access VkImageFormatProperties2) return VkResult;
   pragma Convention (C, PFN_vkGetPhysicalDeviceImageFormatProperties2KHR);  -- vulkan_core.h:6146

   type PFN_vkGetPhysicalDeviceQueueFamilyProperties2KHR is access procedure
        (arg1 : VkPhysicalDevice;
         arg2 : access stdint_h.uint32_t;
         arg3 : access VkQueueFamilyProperties2);
   pragma Convention (C, PFN_vkGetPhysicalDeviceQueueFamilyProperties2KHR);  -- vulkan_core.h:6147

   type PFN_vkGetPhysicalDeviceMemoryProperties2KHR is access procedure (arg1 : VkPhysicalDevice; arg2 : access VkPhysicalDeviceMemoryProperties2);
   pragma Convention (C, PFN_vkGetPhysicalDeviceMemoryProperties2KHR);  -- vulkan_core.h:6148

   type PFN_vkGetPhysicalDeviceSparseImageFormatProperties2KHR is access procedure
        (arg1 : VkPhysicalDevice;
         arg2 : System.Address;
         arg3 : access stdint_h.uint32_t;
         arg4 : access VkSparseImageFormatProperties2);
   pragma Convention (C, PFN_vkGetPhysicalDeviceSparseImageFormatProperties2KHR);  -- vulkan_core.h:6149

   procedure vkGetPhysicalDeviceFeatures2KHR (physicalDevice : VkPhysicalDevice; pFeatures : access VkPhysicalDeviceFeatures2);  -- vulkan_core.h:6152
   pragma Import (C, vkGetPhysicalDeviceFeatures2KHR, "vkGetPhysicalDeviceFeatures2KHR");

   procedure vkGetPhysicalDeviceProperties2KHR (physicalDevice : VkPhysicalDevice; pProperties : access VkPhysicalDeviceProperties2);  -- vulkan_core.h:6156
   pragma Import (C, vkGetPhysicalDeviceProperties2KHR, "vkGetPhysicalDeviceProperties2KHR");

   procedure vkGetPhysicalDeviceFormatProperties2KHR
     (physicalDevice : VkPhysicalDevice;
      format : VkFormat;
      pFormatProperties : access VkFormatProperties2);  -- vulkan_core.h:6160
   pragma Import (C, vkGetPhysicalDeviceFormatProperties2KHR, "vkGetPhysicalDeviceFormatProperties2KHR");

   function vkGetPhysicalDeviceImageFormatProperties2KHR
     (physicalDevice : VkPhysicalDevice;
      pImageFormatInfo : System.Address;
      pImageFormatProperties : access VkImageFormatProperties2) return VkResult;  -- vulkan_core.h:6165
   pragma Import (C, vkGetPhysicalDeviceImageFormatProperties2KHR, "vkGetPhysicalDeviceImageFormatProperties2KHR");

   procedure vkGetPhysicalDeviceQueueFamilyProperties2KHR
     (physicalDevice : VkPhysicalDevice;
      pQueueFamilyPropertyCount : access stdint_h.uint32_t;
      pQueueFamilyProperties : access VkQueueFamilyProperties2);  -- vulkan_core.h:6170
   pragma Import (C, vkGetPhysicalDeviceQueueFamilyProperties2KHR, "vkGetPhysicalDeviceQueueFamilyProperties2KHR");

   procedure vkGetPhysicalDeviceMemoryProperties2KHR (physicalDevice : VkPhysicalDevice; pMemoryProperties : access VkPhysicalDeviceMemoryProperties2);  -- vulkan_core.h:6175
   pragma Import (C, vkGetPhysicalDeviceMemoryProperties2KHR, "vkGetPhysicalDeviceMemoryProperties2KHR");

   procedure vkGetPhysicalDeviceSparseImageFormatProperties2KHR
     (physicalDevice : VkPhysicalDevice;
      pFormatInfo : System.Address;
      pPropertyCount : access stdint_h.uint32_t;
      pProperties : access VkSparseImageFormatProperties2);  -- vulkan_core.h:6179
   pragma Import (C, vkGetPhysicalDeviceSparseImageFormatProperties2KHR, "vkGetPhysicalDeviceSparseImageFormatProperties2KHR");

   subtype VkPeerMemoryFeatureFlagsKHR is VkPeerMemoryFeatureFlags;  -- vulkan_core.h:6190

   subtype VkPeerMemoryFeatureFlagBitsKHR is VkPeerMemoryFeatureFlagBits;

   subtype VkMemoryAllocateFlagsKHR is VkMemoryAllocateFlags;  -- vulkan_core.h:6194

   subtype VkMemoryAllocateFlagBitsKHR is VkMemoryAllocateFlagBits;

   subtype VkMemoryAllocateFlagsInfoKHR is VkMemoryAllocateFlagsInfo;

   subtype VkDeviceGroupRenderPassBeginInfoKHR is VkDeviceGroupRenderPassBeginInfo;

   subtype VkDeviceGroupCommandBufferBeginInfoKHR is VkDeviceGroupCommandBufferBeginInfo;

   subtype VkDeviceGroupSubmitInfoKHR is VkDeviceGroupSubmitInfo;

   subtype VkDeviceGroupBindSparseInfoKHR is VkDeviceGroupBindSparseInfo;

   subtype VkBindBufferMemoryDeviceGroupInfoKHR is VkBindBufferMemoryDeviceGroupInfo;

   subtype VkBindImageMemoryDeviceGroupInfoKHR is VkBindImageMemoryDeviceGroupInfo;

   type PFN_vkGetDeviceGroupPeerMemoryFeaturesKHR is access procedure
        (arg1 : VkDevice;
         arg2 : stdint_h.uint32_t;
         arg3 : stdint_h.uint32_t;
         arg4 : stdint_h.uint32_t;
         arg5 : access VkPeerMemoryFeatureFlags);
   pragma Convention (C, PFN_vkGetDeviceGroupPeerMemoryFeaturesKHR);  -- vulkan_core.h:6212

   type PFN_vkCmdSetDeviceMaskKHR is access procedure (arg1 : VkCommandBuffer; arg2 : stdint_h.uint32_t);
   pragma Convention (C, PFN_vkCmdSetDeviceMaskKHR);  -- vulkan_core.h:6213

   type PFN_vkCmdDispatchBaseKHR is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : stdint_h.uint32_t;
         arg3 : stdint_h.uint32_t;
         arg4 : stdint_h.uint32_t;
         arg5 : stdint_h.uint32_t;
         arg6 : stdint_h.uint32_t;
         arg7 : stdint_h.uint32_t);
   pragma Convention (C, PFN_vkCmdDispatchBaseKHR);  -- vulkan_core.h:6214

   procedure vkGetDeviceGroupPeerMemoryFeaturesKHR
     (device : VkDevice;
      heapIndex : stdint_h.uint32_t;
      localDeviceIndex : stdint_h.uint32_t;
      remoteDeviceIndex : stdint_h.uint32_t;
      pPeerMemoryFeatures : access VkPeerMemoryFeatureFlags);  -- vulkan_core.h:6217
   pragma Import (C, vkGetDeviceGroupPeerMemoryFeaturesKHR, "vkGetDeviceGroupPeerMemoryFeaturesKHR");

   procedure vkCmdSetDeviceMaskKHR (commandBuffer : VkCommandBuffer; deviceMask : stdint_h.uint32_t);  -- vulkan_core.h:6224
   pragma Import (C, vkCmdSetDeviceMaskKHR, "vkCmdSetDeviceMaskKHR");

   procedure vkCmdDispatchBaseKHR
     (commandBuffer : VkCommandBuffer;
      baseGroupX : stdint_h.uint32_t;
      baseGroupY : stdint_h.uint32_t;
      baseGroupZ : stdint_h.uint32_t;
      groupCountX : stdint_h.uint32_t;
      groupCountY : stdint_h.uint32_t;
      groupCountZ : stdint_h.uint32_t);  -- vulkan_core.h:6228
   pragma Import (C, vkCmdDispatchBaseKHR, "vkCmdDispatchBaseKHR");

   subtype VkCommandPoolTrimFlagsKHR is VkCommandPoolTrimFlags;  -- vulkan_core.h:6247

   type PFN_vkTrimCommandPoolKHR is access procedure
        (arg1 : VkDevice;
         arg2 : VkCommandPool;
         arg3 : VkCommandPoolTrimFlags);
   pragma Convention (C, PFN_vkTrimCommandPoolKHR);  -- vulkan_core.h:6249

   procedure vkTrimCommandPoolKHR
     (device : VkDevice;
      commandPool : VkCommandPool;
      flags : VkCommandPoolTrimFlags);  -- vulkan_core.h:6252
   pragma Import (C, vkTrimCommandPoolKHR, "vkTrimCommandPoolKHR");

   subtype VkPhysicalDeviceGroupPropertiesKHR is VkPhysicalDeviceGroupProperties;

   subtype VkDeviceGroupDeviceCreateInfoKHR is VkDeviceGroupDeviceCreateInfo;

   type PFN_vkEnumeratePhysicalDeviceGroupsKHR is access function
        (arg1 : VkInstance;
         arg2 : access stdint_h.uint32_t;
         arg3 : access VkPhysicalDeviceGroupProperties) return VkResult;
   pragma Convention (C, PFN_vkEnumeratePhysicalDeviceGroupsKHR);  -- vulkan_core.h:6267

   function vkEnumeratePhysicalDeviceGroupsKHR
     (instance : VkInstance;
      pPhysicalDeviceGroupCount : access stdint_h.uint32_t;
      pPhysicalDeviceGroupProperties : access VkPhysicalDeviceGroupProperties) return VkResult;  -- vulkan_core.h:6270
   pragma Import (C, vkEnumeratePhysicalDeviceGroupsKHR, "vkEnumeratePhysicalDeviceGroupsKHR");

   subtype VkExternalMemoryHandleTypeFlagsKHR is VkExternalMemoryHandleTypeFlags;  -- vulkan_core.h:6281

   subtype VkExternalMemoryHandleTypeFlagBitsKHR is VkExternalMemoryHandleTypeFlagBits;

   subtype VkExternalMemoryFeatureFlagsKHR is VkExternalMemoryFeatureFlags;  -- vulkan_core.h:6285

   subtype VkExternalMemoryFeatureFlagBitsKHR is VkExternalMemoryFeatureFlagBits;

   subtype VkExternalMemoryPropertiesKHR is VkExternalMemoryProperties;

   subtype VkPhysicalDeviceExternalImageFormatInfoKHR is VkPhysicalDeviceExternalImageFormatInfo;

   subtype VkExternalImageFormatPropertiesKHR is VkExternalImageFormatProperties;

   subtype VkPhysicalDeviceExternalBufferInfoKHR is VkPhysicalDeviceExternalBufferInfo;

   subtype VkExternalBufferPropertiesKHR is VkExternalBufferProperties;

   subtype VkPhysicalDeviceIDPropertiesKHR is VkPhysicalDeviceIDProperties;

   type PFN_vkGetPhysicalDeviceExternalBufferPropertiesKHR is access procedure
        (arg1 : VkPhysicalDevice;
         arg2 : System.Address;
         arg3 : access VkExternalBufferProperties);
   pragma Convention (C, PFN_vkGetPhysicalDeviceExternalBufferPropertiesKHR);  -- vulkan_core.h:6301

   procedure vkGetPhysicalDeviceExternalBufferPropertiesKHR
     (physicalDevice : VkPhysicalDevice;
      pExternalBufferInfo : System.Address;
      pExternalBufferProperties : access VkExternalBufferProperties);  -- vulkan_core.h:6304
   pragma Import (C, vkGetPhysicalDeviceExternalBufferPropertiesKHR, "vkGetPhysicalDeviceExternalBufferPropertiesKHR");

   subtype VkExternalMemoryImageCreateInfoKHR is VkExternalMemoryImageCreateInfo;

   subtype VkExternalMemoryBufferCreateInfoKHR is VkExternalMemoryBufferCreateInfo;

   subtype VkExportMemoryAllocateInfoKHR is VkExportMemoryAllocateInfo;

   type VkImportMemoryFdInfoKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:6327
      pNext : System.Address;  -- vulkan_core.h:6328
      handleType : aliased VkExternalMemoryHandleTypeFlagBits;  -- vulkan_core.h:6329
      fd : aliased int;  -- vulkan_core.h:6330
   end record;
   pragma Convention (C_Pass_By_Copy, VkImportMemoryFdInfoKHR);  -- vulkan_core.h:6326

   type VkMemoryFdPropertiesKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:6334
      pNext : System.Address;  -- vulkan_core.h:6335
      memoryTypeBits : aliased stdint_h.uint32_t;  -- vulkan_core.h:6336
   end record;
   pragma Convention (C_Pass_By_Copy, VkMemoryFdPropertiesKHR);  -- vulkan_core.h:6333

   type VkMemoryGetFdInfoKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:6340
      pNext : System.Address;  -- vulkan_core.h:6341
      memory : VkDeviceMemory;  -- vulkan_core.h:6342
      handleType : aliased VkExternalMemoryHandleTypeFlagBits;  -- vulkan_core.h:6343
   end record;
   pragma Convention (C_Pass_By_Copy, VkMemoryGetFdInfoKHR);  -- vulkan_core.h:6339

   type PFN_vkGetMemoryFdKHR is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : access int) return VkResult;
   pragma Convention (C, PFN_vkGetMemoryFdKHR);  -- vulkan_core.h:6346

   type PFN_vkGetMemoryFdPropertiesKHR is access function
        (arg1 : VkDevice;
         arg2 : VkExternalMemoryHandleTypeFlagBits;
         arg3 : int;
         arg4 : access VkMemoryFdPropertiesKHR) return VkResult;
   pragma Convention (C, PFN_vkGetMemoryFdPropertiesKHR);  -- vulkan_core.h:6347

   function vkGetMemoryFdKHR
     (device : VkDevice;
      pGetFdInfo : System.Address;
      pFd : access int) return VkResult;  -- vulkan_core.h:6350
   pragma Import (C, vkGetMemoryFdKHR, "vkGetMemoryFdKHR");

   function vkGetMemoryFdPropertiesKHR
     (device : VkDevice;
      handleType : VkExternalMemoryHandleTypeFlagBits;
      fd : int;
      pMemoryFdProperties : access VkMemoryFdPropertiesKHR) return VkResult;  -- vulkan_core.h:6355
   pragma Import (C, vkGetMemoryFdPropertiesKHR, "vkGetMemoryFdPropertiesKHR");

   subtype VkExternalSemaphoreHandleTypeFlagsKHR is VkExternalSemaphoreHandleTypeFlags;  -- vulkan_core.h:6366

   subtype VkExternalSemaphoreHandleTypeFlagBitsKHR is VkExternalSemaphoreHandleTypeFlagBits;

   subtype VkExternalSemaphoreFeatureFlagsKHR is VkExternalSemaphoreFeatureFlags;  -- vulkan_core.h:6370

   subtype VkExternalSemaphoreFeatureFlagBitsKHR is VkExternalSemaphoreFeatureFlagBits;

   subtype VkPhysicalDeviceExternalSemaphoreInfoKHR is VkPhysicalDeviceExternalSemaphoreInfo;

   subtype VkExternalSemaphorePropertiesKHR is VkExternalSemaphoreProperties;

   type PFN_vkGetPhysicalDeviceExternalSemaphorePropertiesKHR is access procedure
        (arg1 : VkPhysicalDevice;
         arg2 : System.Address;
         arg3 : access VkExternalSemaphoreProperties);
   pragma Convention (C, PFN_vkGetPhysicalDeviceExternalSemaphorePropertiesKHR);  -- vulkan_core.h:6378

   procedure vkGetPhysicalDeviceExternalSemaphorePropertiesKHR
     (physicalDevice : VkPhysicalDevice;
      pExternalSemaphoreInfo : System.Address;
      pExternalSemaphoreProperties : access VkExternalSemaphoreProperties);  -- vulkan_core.h:6381
   pragma Import (C, vkGetPhysicalDeviceExternalSemaphorePropertiesKHR, "vkGetPhysicalDeviceExternalSemaphorePropertiesKHR");

   subtype VkSemaphoreImportFlagsKHR is VkSemaphoreImportFlags;  -- vulkan_core.h:6391

   subtype VkSemaphoreImportFlagBitsKHR is VkSemaphoreImportFlagBits;

   subtype VkExportSemaphoreCreateInfoKHR is VkExportSemaphoreCreateInfo;

   type VkImportSemaphoreFdInfoKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:6403
      pNext : System.Address;  -- vulkan_core.h:6404
      semaphore : VkSemaphore;  -- vulkan_core.h:6405
      flags : aliased VkSemaphoreImportFlags;  -- vulkan_core.h:6406
      handleType : aliased VkExternalSemaphoreHandleTypeFlagBits;  -- vulkan_core.h:6407
      fd : aliased int;  -- vulkan_core.h:6408
   end record;
   pragma Convention (C_Pass_By_Copy, VkImportSemaphoreFdInfoKHR);  -- vulkan_core.h:6402

   type VkSemaphoreGetFdInfoKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:6412
      pNext : System.Address;  -- vulkan_core.h:6413
      semaphore : VkSemaphore;  -- vulkan_core.h:6414
      handleType : aliased VkExternalSemaphoreHandleTypeFlagBits;  -- vulkan_core.h:6415
   end record;
   pragma Convention (C_Pass_By_Copy, VkSemaphoreGetFdInfoKHR);  -- vulkan_core.h:6411

   type PFN_vkImportSemaphoreFdKHR is access function (arg1 : VkDevice; arg2 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkImportSemaphoreFdKHR);  -- vulkan_core.h:6418

   type PFN_vkGetSemaphoreFdKHR is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : access int) return VkResult;
   pragma Convention (C, PFN_vkGetSemaphoreFdKHR);  -- vulkan_core.h:6419

   function vkImportSemaphoreFdKHR (device : VkDevice; pImportSemaphoreFdInfo : System.Address) return VkResult;  -- vulkan_core.h:6422
   pragma Import (C, vkImportSemaphoreFdKHR, "vkImportSemaphoreFdKHR");

   function vkGetSemaphoreFdKHR
     (device : VkDevice;
      pGetFdInfo : System.Address;
      pFd : access int) return VkResult;  -- vulkan_core.h:6426
   pragma Import (C, vkGetSemaphoreFdKHR, "vkGetSemaphoreFdKHR");

   type VkPhysicalDevicePushDescriptorPropertiesKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:6437
      pNext : System.Address;  -- vulkan_core.h:6438
      maxPushDescriptors : aliased stdint_h.uint32_t;  -- vulkan_core.h:6439
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDevicePushDescriptorPropertiesKHR);  -- vulkan_core.h:6436

   type PFN_vkCmdPushDescriptorSetKHR is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkPipelineBindPoint;
         arg3 : VkPipelineLayout;
         arg4 : stdint_h.uint32_t;
         arg5 : stdint_h.uint32_t;
         arg6 : System.Address);
   pragma Convention (C, PFN_vkCmdPushDescriptorSetKHR);  -- vulkan_core.h:6442

   type PFN_vkCmdPushDescriptorSetWithTemplateKHR is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkDescriptorUpdateTemplate;
         arg3 : VkPipelineLayout;
         arg4 : stdint_h.uint32_t;
         arg5 : System.Address);
   pragma Convention (C, PFN_vkCmdPushDescriptorSetWithTemplateKHR);  -- vulkan_core.h:6443

   procedure vkCmdPushDescriptorSetKHR
     (commandBuffer : VkCommandBuffer;
      pipelineBindPoint : VkPipelineBindPoint;
      layout : VkPipelineLayout;
      set : stdint_h.uint32_t;
      descriptorWriteCount : stdint_h.uint32_t;
      pDescriptorWrites : System.Address);  -- vulkan_core.h:6446
   pragma Import (C, vkCmdPushDescriptorSetKHR, "vkCmdPushDescriptorSetKHR");

   procedure vkCmdPushDescriptorSetWithTemplateKHR
     (commandBuffer : VkCommandBuffer;
      descriptorUpdateTemplate : VkDescriptorUpdateTemplate;
      layout : VkPipelineLayout;
      set : stdint_h.uint32_t;
      pData : System.Address);  -- vulkan_core.h:6454
   pragma Import (C, vkCmdPushDescriptorSetWithTemplateKHR, "vkCmdPushDescriptorSetWithTemplateKHR");

   subtype VkPhysicalDeviceShaderFloat16Int8FeaturesKHR is VkPhysicalDeviceShaderFloat16Int8Features;

   subtype VkPhysicalDeviceFloat16Int8FeaturesKHR is VkPhysicalDeviceShaderFloat16Int8Features;

   subtype VkPhysicalDevice16BitStorageFeaturesKHR is VkPhysicalDevice16BitStorageFeatures;

   type VkRectLayerKHR is record
      offset : aliased VkOffset2D;  -- vulkan_core.h:6483
      extent : aliased VkExtent2D;  -- vulkan_core.h:6484
      layer : aliased stdint_h.uint32_t;  -- vulkan_core.h:6485
   end record;
   pragma Convention (C_Pass_By_Copy, VkRectLayerKHR);  -- vulkan_core.h:6482

   type VkPresentRegionKHR is record
      rectangleCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:6489
      pRectangles : System.Address;  -- vulkan_core.h:6490
   end record;
   pragma Convention (C_Pass_By_Copy, VkPresentRegionKHR);  -- vulkan_core.h:6488

   type VkPresentRegionsKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:6494
      pNext : System.Address;  -- vulkan_core.h:6495
      swapchainCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:6496
      pRegions : System.Address;  -- vulkan_core.h:6497
   end record;
   pragma Convention (C_Pass_By_Copy, VkPresentRegionsKHR);  -- vulkan_core.h:6493

   subtype VkDescriptorUpdateTemplateKHR is VkDescriptorUpdateTemplate;  -- vulkan_core.h:6503

   subtype VkDescriptorUpdateTemplateTypeKHR is VkDescriptorUpdateTemplateType;

   subtype VkDescriptorUpdateTemplateCreateFlagsKHR is VkDescriptorUpdateTemplateCreateFlags;  -- vulkan_core.h:6509

   subtype VkDescriptorUpdateTemplateEntryKHR is VkDescriptorUpdateTemplateEntry;

   subtype VkDescriptorUpdateTemplateCreateInfoKHR is VkDescriptorUpdateTemplateCreateInfo;

   type PFN_vkCreateDescriptorUpdateTemplateKHR is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : System.Address;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateDescriptorUpdateTemplateKHR);  -- vulkan_core.h:6515

   type PFN_vkDestroyDescriptorUpdateTemplateKHR is access procedure
        (arg1 : VkDevice;
         arg2 : VkDescriptorUpdateTemplate;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkDestroyDescriptorUpdateTemplateKHR);  -- vulkan_core.h:6516

   type PFN_vkUpdateDescriptorSetWithTemplateKHR is access procedure
        (arg1 : VkDevice;
         arg2 : VkDescriptorSet;
         arg3 : VkDescriptorUpdateTemplate;
         arg4 : System.Address);
   pragma Convention (C, PFN_vkUpdateDescriptorSetWithTemplateKHR);  -- vulkan_core.h:6517

   function vkCreateDescriptorUpdateTemplateKHR
     (device : VkDevice;
      pCreateInfo : System.Address;
      pAllocator : System.Address;
      pDescriptorUpdateTemplate : System.Address) return VkResult;  -- vulkan_core.h:6520
   pragma Import (C, vkCreateDescriptorUpdateTemplateKHR, "vkCreateDescriptorUpdateTemplateKHR");

   procedure vkDestroyDescriptorUpdateTemplateKHR
     (device : VkDevice;
      descriptorUpdateTemplate : VkDescriptorUpdateTemplate;
      pAllocator : System.Address);  -- vulkan_core.h:6526
   pragma Import (C, vkDestroyDescriptorUpdateTemplateKHR, "vkDestroyDescriptorUpdateTemplateKHR");

   procedure vkUpdateDescriptorSetWithTemplateKHR
     (device : VkDevice;
      descriptorSet : VkDescriptorSet;
      descriptorUpdateTemplate : VkDescriptorUpdateTemplate;
      pData : System.Address);  -- vulkan_core.h:6531
   pragma Import (C, vkUpdateDescriptorSetWithTemplateKHR, "vkUpdateDescriptorSetWithTemplateKHR");

   subtype VkPhysicalDeviceImagelessFramebufferFeaturesKHR is VkPhysicalDeviceImagelessFramebufferFeatures;

   subtype VkFramebufferAttachmentsCreateInfoKHR is VkFramebufferAttachmentsCreateInfo;

   subtype VkFramebufferAttachmentImageInfoKHR is VkFramebufferAttachmentImageInfo;

   subtype VkRenderPassAttachmentBeginInfoKHR is VkRenderPassAttachmentBeginInfo;

   subtype VkRenderPassCreateInfo2KHR is VkRenderPassCreateInfo2;

   subtype VkAttachmentDescription2KHR is VkAttachmentDescription2;

   subtype VkAttachmentReference2KHR is VkAttachmentReference2;

   subtype VkSubpassDescription2KHR is VkSubpassDescription2;

   subtype VkSubpassDependency2KHR is VkSubpassDependency2;

   subtype VkSubpassBeginInfoKHR is VkSubpassBeginInfo;

   subtype VkSubpassEndInfoKHR is VkSubpassEndInfo;

   type PFN_vkCreateRenderPass2KHR is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : System.Address;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateRenderPass2KHR);  -- vulkan_core.h:6569

   type PFN_vkCmdBeginRenderPass2KHR is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : System.Address;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkCmdBeginRenderPass2KHR);  -- vulkan_core.h:6570

   type PFN_vkCmdNextSubpass2KHR is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : System.Address;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkCmdNextSubpass2KHR);  -- vulkan_core.h:6571

   type PFN_vkCmdEndRenderPass2KHR is access procedure (arg1 : VkCommandBuffer; arg2 : System.Address);
   pragma Convention (C, PFN_vkCmdEndRenderPass2KHR);  -- vulkan_core.h:6572

   function vkCreateRenderPass2KHR
     (device : VkDevice;
      pCreateInfo : System.Address;
      pAllocator : System.Address;
      pRenderPass : System.Address) return VkResult;  -- vulkan_core.h:6575
   pragma Import (C, vkCreateRenderPass2KHR, "vkCreateRenderPass2KHR");

   procedure vkCmdBeginRenderPass2KHR
     (commandBuffer : VkCommandBuffer;
      pRenderPassBegin : System.Address;
      pSubpassBeginInfo : System.Address);  -- vulkan_core.h:6581
   pragma Import (C, vkCmdBeginRenderPass2KHR, "vkCmdBeginRenderPass2KHR");

   procedure vkCmdNextSubpass2KHR
     (commandBuffer : VkCommandBuffer;
      pSubpassBeginInfo : System.Address;
      pSubpassEndInfo : System.Address);  -- vulkan_core.h:6586
   pragma Import (C, vkCmdNextSubpass2KHR, "vkCmdNextSubpass2KHR");

   procedure vkCmdEndRenderPass2KHR (commandBuffer : VkCommandBuffer; pSubpassEndInfo : System.Address);  -- vulkan_core.h:6591
   pragma Import (C, vkCmdEndRenderPass2KHR, "vkCmdEndRenderPass2KHR");

   type VkSharedPresentSurfaceCapabilitiesKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:6601
      pNext : System.Address;  -- vulkan_core.h:6602
      sharedPresentSupportedUsageFlags : aliased VkImageUsageFlags;  -- vulkan_core.h:6603
   end record;
   pragma Convention (C_Pass_By_Copy, VkSharedPresentSurfaceCapabilitiesKHR);  -- vulkan_core.h:6600

   type PFN_vkGetSwapchainStatusKHR is access function (arg1 : VkDevice; arg2 : VkSwapchainKHR) return VkResult;
   pragma Convention (C, PFN_vkGetSwapchainStatusKHR);  -- vulkan_core.h:6606

   function vkGetSwapchainStatusKHR (device : VkDevice; swapchain : VkSwapchainKHR) return VkResult;  -- vulkan_core.h:6609
   pragma Import (C, vkGetSwapchainStatusKHR, "vkGetSwapchainStatusKHR");

   subtype VkExternalFenceHandleTypeFlagsKHR is VkExternalFenceHandleTypeFlags;  -- vulkan_core.h:6618

   subtype VkExternalFenceHandleTypeFlagBitsKHR is VkExternalFenceHandleTypeFlagBits;

   subtype VkExternalFenceFeatureFlagsKHR is VkExternalFenceFeatureFlags;  -- vulkan_core.h:6622

   subtype VkExternalFenceFeatureFlagBitsKHR is VkExternalFenceFeatureFlagBits;

   subtype VkPhysicalDeviceExternalFenceInfoKHR is VkPhysicalDeviceExternalFenceInfo;

   subtype VkExternalFencePropertiesKHR is VkExternalFenceProperties;

   type PFN_vkGetPhysicalDeviceExternalFencePropertiesKHR is access procedure
        (arg1 : VkPhysicalDevice;
         arg2 : System.Address;
         arg3 : access VkExternalFenceProperties);
   pragma Convention (C, PFN_vkGetPhysicalDeviceExternalFencePropertiesKHR);  -- vulkan_core.h:6630

   procedure vkGetPhysicalDeviceExternalFencePropertiesKHR
     (physicalDevice : VkPhysicalDevice;
      pExternalFenceInfo : System.Address;
      pExternalFenceProperties : access VkExternalFenceProperties);  -- vulkan_core.h:6633
   pragma Import (C, vkGetPhysicalDeviceExternalFencePropertiesKHR, "vkGetPhysicalDeviceExternalFencePropertiesKHR");

   subtype VkFenceImportFlagsKHR is VkFenceImportFlags;  -- vulkan_core.h:6643

   subtype VkFenceImportFlagBitsKHR is VkFenceImportFlagBits;

   subtype VkExportFenceCreateInfoKHR is VkExportFenceCreateInfo;

   type VkImportFenceFdInfoKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:6655
      pNext : System.Address;  -- vulkan_core.h:6656
      fence : VkFence;  -- vulkan_core.h:6657
      flags : aliased VkFenceImportFlags;  -- vulkan_core.h:6658
      handleType : aliased VkExternalFenceHandleTypeFlagBits;  -- vulkan_core.h:6659
      fd : aliased int;  -- vulkan_core.h:6660
   end record;
   pragma Convention (C_Pass_By_Copy, VkImportFenceFdInfoKHR);  -- vulkan_core.h:6654

   type VkFenceGetFdInfoKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:6664
      pNext : System.Address;  -- vulkan_core.h:6665
      fence : VkFence;  -- vulkan_core.h:6666
      handleType : aliased VkExternalFenceHandleTypeFlagBits;  -- vulkan_core.h:6667
   end record;
   pragma Convention (C_Pass_By_Copy, VkFenceGetFdInfoKHR);  -- vulkan_core.h:6663

   type PFN_vkImportFenceFdKHR is access function (arg1 : VkDevice; arg2 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkImportFenceFdKHR);  -- vulkan_core.h:6670

   type PFN_vkGetFenceFdKHR is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : access int) return VkResult;
   pragma Convention (C, PFN_vkGetFenceFdKHR);  -- vulkan_core.h:6671

   function vkImportFenceFdKHR (device : VkDevice; pImportFenceFdInfo : System.Address) return VkResult;  -- vulkan_core.h:6674
   pragma Import (C, vkImportFenceFdKHR, "vkImportFenceFdKHR");

   function vkGetFenceFdKHR
     (device : VkDevice;
      pGetFdInfo : System.Address;
      pFd : access int) return VkResult;  -- vulkan_core.h:6678
   pragma Import (C, vkGetFenceFdKHR, "vkGetFenceFdKHR");

   subtype VkPerformanceCounterUnitKHR is unsigned;
   VK_PERFORMANCE_COUNTER_UNIT_GENERIC_KHR : constant VkPerformanceCounterUnitKHR := 0;
   VK_PERFORMANCE_COUNTER_UNIT_PERCENTAGE_KHR : constant VkPerformanceCounterUnitKHR := 1;
   VK_PERFORMANCE_COUNTER_UNIT_NANOSECONDS_KHR : constant VkPerformanceCounterUnitKHR := 2;
   VK_PERFORMANCE_COUNTER_UNIT_BYTES_KHR : constant VkPerformanceCounterUnitKHR := 3;
   VK_PERFORMANCE_COUNTER_UNIT_BYTES_PER_SECOND_KHR : constant VkPerformanceCounterUnitKHR := 4;
   VK_PERFORMANCE_COUNTER_UNIT_KELVIN_KHR : constant VkPerformanceCounterUnitKHR := 5;
   VK_PERFORMANCE_COUNTER_UNIT_WATTS_KHR : constant VkPerformanceCounterUnitKHR := 6;
   VK_PERFORMANCE_COUNTER_UNIT_VOLTS_KHR : constant VkPerformanceCounterUnitKHR := 7;
   VK_PERFORMANCE_COUNTER_UNIT_AMPS_KHR : constant VkPerformanceCounterUnitKHR := 8;
   VK_PERFORMANCE_COUNTER_UNIT_HERTZ_KHR : constant VkPerformanceCounterUnitKHR := 9;
   VK_PERFORMANCE_COUNTER_UNIT_CYCLES_KHR : constant VkPerformanceCounterUnitKHR := 10;
   VK_PERFORMANCE_COUNTER_UNIT_MAX_ENUM_KHR : constant VkPerformanceCounterUnitKHR := 2147483647;  -- vulkan_core.h:6689

   subtype VkPerformanceCounterScopeKHR is unsigned;
   VK_PERFORMANCE_COUNTER_SCOPE_COMMAND_BUFFER_KHR : constant VkPerformanceCounterScopeKHR := 0;
   VK_PERFORMANCE_COUNTER_SCOPE_RENDER_PASS_KHR : constant VkPerformanceCounterScopeKHR := 1;
   VK_PERFORMANCE_COUNTER_SCOPE_COMMAND_KHR : constant VkPerformanceCounterScopeKHR := 2;
   VK_QUERY_SCOPE_COMMAND_BUFFER_KHR : constant VkPerformanceCounterScopeKHR := 0;
   VK_QUERY_SCOPE_RENDER_PASS_KHR : constant VkPerformanceCounterScopeKHR := 1;
   VK_QUERY_SCOPE_COMMAND_KHR : constant VkPerformanceCounterScopeKHR := 2;
   VK_PERFORMANCE_COUNTER_SCOPE_MAX_ENUM_KHR : constant VkPerformanceCounterScopeKHR := 2147483647;  -- vulkan_core.h:6704

   subtype VkPerformanceCounterStorageKHR is unsigned;
   VK_PERFORMANCE_COUNTER_STORAGE_INT32_KHR : constant VkPerformanceCounterStorageKHR := 0;
   VK_PERFORMANCE_COUNTER_STORAGE_INT64_KHR : constant VkPerformanceCounterStorageKHR := 1;
   VK_PERFORMANCE_COUNTER_STORAGE_UINT32_KHR : constant VkPerformanceCounterStorageKHR := 2;
   VK_PERFORMANCE_COUNTER_STORAGE_UINT64_KHR : constant VkPerformanceCounterStorageKHR := 3;
   VK_PERFORMANCE_COUNTER_STORAGE_FLOAT32_KHR : constant VkPerformanceCounterStorageKHR := 4;
   VK_PERFORMANCE_COUNTER_STORAGE_FLOAT64_KHR : constant VkPerformanceCounterStorageKHR := 5;
   VK_PERFORMANCE_COUNTER_STORAGE_MAX_ENUM_KHR : constant VkPerformanceCounterStorageKHR := 2147483647;  -- vulkan_core.h:6714

   subtype VkPerformanceCounterDescriptionFlagBitsKHR is unsigned;
   VK_PERFORMANCE_COUNTER_DESCRIPTION_PERFORMANCE_IMPACTING_BIT_KHR : constant VkPerformanceCounterDescriptionFlagBitsKHR := 1;
   VK_PERFORMANCE_COUNTER_DESCRIPTION_CONCURRENTLY_IMPACTED_BIT_KHR : constant VkPerformanceCounterDescriptionFlagBitsKHR := 2;
   VK_PERFORMANCE_COUNTER_DESCRIPTION_PERFORMANCE_IMPACTING_KHR : constant VkPerformanceCounterDescriptionFlagBitsKHR := 1;
   VK_PERFORMANCE_COUNTER_DESCRIPTION_CONCURRENTLY_IMPACTED_KHR : constant VkPerformanceCounterDescriptionFlagBitsKHR := 2;
   VK_PERFORMANCE_COUNTER_DESCRIPTION_FLAG_BITS_MAX_ENUM_KHR : constant VkPerformanceCounterDescriptionFlagBitsKHR := 2147483647;  -- vulkan_core.h:6724

   subtype VkPerformanceCounterDescriptionFlagsKHR is VkFlags;  -- vulkan_core.h:6731

   subtype VkAcquireProfilingLockFlagBitsKHR is unsigned;
   VK_ACQUIRE_PROFILING_LOCK_FLAG_BITS_MAX_ENUM_KHR : constant VkAcquireProfilingLockFlagBitsKHR := 2147483647;  -- vulkan_core.h:6733

   subtype VkAcquireProfilingLockFlagsKHR is VkFlags;  -- vulkan_core.h:6736

   type VkPhysicalDevicePerformanceQueryFeaturesKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:6738
      pNext : System.Address;  -- vulkan_core.h:6739
      performanceCounterQueryPools : aliased VkBool32;  -- vulkan_core.h:6740
      performanceCounterMultipleQueryPools : aliased VkBool32;  -- vulkan_core.h:6741
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDevicePerformanceQueryFeaturesKHR);  -- vulkan_core.h:6737

   type VkPhysicalDevicePerformanceQueryPropertiesKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:6745
      pNext : System.Address;  -- vulkan_core.h:6746
      allowCommandBufferQueryCopies : aliased VkBool32;  -- vulkan_core.h:6747
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDevicePerformanceQueryPropertiesKHR);  -- vulkan_core.h:6744

   type VkPerformanceCounterKHR_uuid_array is array (0 .. 15) of aliased stdint_h.uint8_t;
   type VkPerformanceCounterKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:6751
      pNext : System.Address;  -- vulkan_core.h:6752
      unit : aliased VkPerformanceCounterUnitKHR;  -- vulkan_core.h:6753
      scope : aliased VkPerformanceCounterScopeKHR;  -- vulkan_core.h:6754
      storage : aliased VkPerformanceCounterStorageKHR;  -- vulkan_core.h:6755
      uuid : aliased VkPerformanceCounterKHR_uuid_array;  -- vulkan_core.h:6756
   end record;
   pragma Convention (C_Pass_By_Copy, VkPerformanceCounterKHR);  -- vulkan_core.h:6750

   subtype VkPerformanceCounterDescriptionKHR_name_array is Interfaces.C.char_array (0 .. 255);
   subtype VkPerformanceCounterDescriptionKHR_category_array is Interfaces.C.char_array (0 .. 255);
   subtype VkPerformanceCounterDescriptionKHR_description_array is Interfaces.C.char_array (0 .. 255);
   type VkPerformanceCounterDescriptionKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:6760
      pNext : System.Address;  -- vulkan_core.h:6761
      flags : aliased VkPerformanceCounterDescriptionFlagsKHR;  -- vulkan_core.h:6762
      name : aliased VkPerformanceCounterDescriptionKHR_name_array;  -- vulkan_core.h:6763
      category : aliased VkPerformanceCounterDescriptionKHR_category_array;  -- vulkan_core.h:6764
      description : aliased VkPerformanceCounterDescriptionKHR_description_array;  -- vulkan_core.h:6765
   end record;
   pragma Convention (C_Pass_By_Copy, VkPerformanceCounterDescriptionKHR);  -- vulkan_core.h:6759

   type VkQueryPoolPerformanceCreateInfoKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:6769
      pNext : System.Address;  -- vulkan_core.h:6770
      queueFamilyIndex : aliased stdint_h.uint32_t;  -- vulkan_core.h:6771
      counterIndexCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:6772
      pCounterIndices : access stdint_h.uint32_t;  -- vulkan_core.h:6773
   end record;
   pragma Convention (C_Pass_By_Copy, VkQueryPoolPerformanceCreateInfoKHR);  -- vulkan_core.h:6768

   type VkPerformanceCounterResultKHR (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            int32 : aliased stdint_h.int32_t;  -- vulkan_core.h:6777
         when 1 =>
            int64 : aliased stdint_h.int64_t;  -- vulkan_core.h:6778
         when 2 =>
            uint32 : aliased stdint_h.uint32_t;  -- vulkan_core.h:6779
         when 3 =>
            uint64 : aliased stdint_h.uint64_t;  -- vulkan_core.h:6780
         when 4 =>
            float32 : aliased float;  -- vulkan_core.h:6781
         when others =>
            float64 : aliased double;  -- vulkan_core.h:6782
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, VkPerformanceCounterResultKHR);
   pragma Unchecked_Union (VkPerformanceCounterResultKHR);  -- vulkan_core.h:6776

   type VkAcquireProfilingLockInfoKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:6786
      pNext : System.Address;  -- vulkan_core.h:6787
      flags : aliased VkAcquireProfilingLockFlagsKHR;  -- vulkan_core.h:6788
      timeout : aliased stdint_h.uint64_t;  -- vulkan_core.h:6789
   end record;
   pragma Convention (C_Pass_By_Copy, VkAcquireProfilingLockInfoKHR);  -- vulkan_core.h:6785

   type VkPerformanceQuerySubmitInfoKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:6793
      pNext : System.Address;  -- vulkan_core.h:6794
      counterPassIndex : aliased stdint_h.uint32_t;  -- vulkan_core.h:6795
   end record;
   pragma Convention (C_Pass_By_Copy, VkPerformanceQuerySubmitInfoKHR);  -- vulkan_core.h:6792

   type PFN_vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR is access function
        (arg1 : VkPhysicalDevice;
         arg2 : stdint_h.uint32_t;
         arg3 : access stdint_h.uint32_t;
         arg4 : access VkPerformanceCounterKHR;
         arg5 : access VkPerformanceCounterDescriptionKHR) return VkResult;
   pragma Convention (C, PFN_vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR);  -- vulkan_core.h:6798

   type PFN_vkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR is access procedure
        (arg1 : VkPhysicalDevice;
         arg2 : System.Address;
         arg3 : access stdint_h.uint32_t);
   pragma Convention (C, PFN_vkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR);  -- vulkan_core.h:6799

   type PFN_vkAcquireProfilingLockKHR is access function (arg1 : VkDevice; arg2 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkAcquireProfilingLockKHR);  -- vulkan_core.h:6800

   type PFN_vkReleaseProfilingLockKHR is access procedure (arg1 : VkDevice);
   pragma Convention (C, PFN_vkReleaseProfilingLockKHR);  -- vulkan_core.h:6801

   function vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR
     (physicalDevice : VkPhysicalDevice;
      queueFamilyIndex : stdint_h.uint32_t;
      pCounterCount : access stdint_h.uint32_t;
      pCounters : access VkPerformanceCounterKHR;
      pCounterDescriptions : access VkPerformanceCounterDescriptionKHR) return VkResult;  -- vulkan_core.h:6804
   pragma Import (C, vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR, "vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR");

   procedure vkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR
     (physicalDevice : VkPhysicalDevice;
      pPerformanceQueryCreateInfo : System.Address;
      pNumPasses : access stdint_h.uint32_t);  -- vulkan_core.h:6811
   pragma Import (C, vkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR, "vkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR");

   function vkAcquireProfilingLockKHR (device : VkDevice; pInfo : System.Address) return VkResult;  -- vulkan_core.h:6816
   pragma Import (C, vkAcquireProfilingLockKHR, "vkAcquireProfilingLockKHR");

   procedure vkReleaseProfilingLockKHR (device : VkDevice);  -- vulkan_core.h:6820
   pragma Import (C, vkReleaseProfilingLockKHR, "vkReleaseProfilingLockKHR");

   subtype VkPointClippingBehaviorKHR is VkPointClippingBehavior;

   subtype VkTessellationDomainOriginKHR is VkTessellationDomainOrigin;

   subtype VkPhysicalDevicePointClippingPropertiesKHR is VkPhysicalDevicePointClippingProperties;

   subtype VkRenderPassInputAttachmentAspectCreateInfoKHR is VkRenderPassInputAttachmentAspectCreateInfo;

   subtype VkInputAttachmentAspectReferenceKHR is VkInputAttachmentAspectReference;

   subtype VkImageViewUsageCreateInfoKHR is VkImageViewUsageCreateInfo;

   subtype VkPipelineTessellationDomainOriginStateCreateInfoKHR is VkPipelineTessellationDomainOriginStateCreateInfo;

   type VkPhysicalDeviceSurfaceInfo2KHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:6848
      pNext : System.Address;  -- vulkan_core.h:6849
      surface : VkSurfaceKHR;  -- vulkan_core.h:6850
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceSurfaceInfo2KHR);  -- vulkan_core.h:6847

   type VkSurfaceCapabilities2KHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:6854
      pNext : System.Address;  -- vulkan_core.h:6855
      surfaceCapabilities : aliased VkSurfaceCapabilitiesKHR;  -- vulkan_core.h:6856
   end record;
   pragma Convention (C_Pass_By_Copy, VkSurfaceCapabilities2KHR);  -- vulkan_core.h:6853

   type VkSurfaceFormat2KHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:6860
      pNext : System.Address;  -- vulkan_core.h:6861
      surfaceFormat : aliased VkSurfaceFormatKHR;  -- vulkan_core.h:6862
   end record;
   pragma Convention (C_Pass_By_Copy, VkSurfaceFormat2KHR);  -- vulkan_core.h:6859

   type PFN_vkGetPhysicalDeviceSurfaceCapabilities2KHR is access function
        (arg1 : VkPhysicalDevice;
         arg2 : System.Address;
         arg3 : access VkSurfaceCapabilities2KHR) return VkResult;
   pragma Convention (C, PFN_vkGetPhysicalDeviceSurfaceCapabilities2KHR);  -- vulkan_core.h:6865

   type PFN_vkGetPhysicalDeviceSurfaceFormats2KHR is access function
        (arg1 : VkPhysicalDevice;
         arg2 : System.Address;
         arg3 : access stdint_h.uint32_t;
         arg4 : access VkSurfaceFormat2KHR) return VkResult;
   pragma Convention (C, PFN_vkGetPhysicalDeviceSurfaceFormats2KHR);  -- vulkan_core.h:6866

   function vkGetPhysicalDeviceSurfaceCapabilities2KHR
     (physicalDevice : VkPhysicalDevice;
      pSurfaceInfo : System.Address;
      pSurfaceCapabilities : access VkSurfaceCapabilities2KHR) return VkResult;  -- vulkan_core.h:6869
   pragma Import (C, vkGetPhysicalDeviceSurfaceCapabilities2KHR, "vkGetPhysicalDeviceSurfaceCapabilities2KHR");

   function vkGetPhysicalDeviceSurfaceFormats2KHR
     (physicalDevice : VkPhysicalDevice;
      pSurfaceInfo : System.Address;
      pSurfaceFormatCount : access stdint_h.uint32_t;
      pSurfaceFormats : access VkSurfaceFormat2KHR) return VkResult;  -- vulkan_core.h:6874
   pragma Import (C, vkGetPhysicalDeviceSurfaceFormats2KHR, "vkGetPhysicalDeviceSurfaceFormats2KHR");

   subtype VkPhysicalDeviceVariablePointerFeaturesKHR is VkPhysicalDeviceVariablePointersFeatures;

   subtype VkPhysicalDeviceVariablePointersFeaturesKHR is VkPhysicalDeviceVariablePointersFeatures;

   type VkDisplayProperties2KHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:6895
      pNext : System.Address;  -- vulkan_core.h:6896
      displayProperties : aliased VkDisplayPropertiesKHR;  -- vulkan_core.h:6897
   end record;
   pragma Convention (C_Pass_By_Copy, VkDisplayProperties2KHR);  -- vulkan_core.h:6894

   type VkDisplayPlaneProperties2KHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:6901
      pNext : System.Address;  -- vulkan_core.h:6902
      displayPlaneProperties : aliased VkDisplayPlanePropertiesKHR;  -- vulkan_core.h:6903
   end record;
   pragma Convention (C_Pass_By_Copy, VkDisplayPlaneProperties2KHR);  -- vulkan_core.h:6900

   type VkDisplayModeProperties2KHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:6907
      pNext : System.Address;  -- vulkan_core.h:6908
      displayModeProperties : aliased VkDisplayModePropertiesKHR;  -- vulkan_core.h:6909
   end record;
   pragma Convention (C_Pass_By_Copy, VkDisplayModeProperties2KHR);  -- vulkan_core.h:6906

   type VkDisplayPlaneInfo2KHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:6913
      pNext : System.Address;  -- vulkan_core.h:6914
      mode : VkDisplayModeKHR;  -- vulkan_core.h:6915
      planeIndex : aliased stdint_h.uint32_t;  -- vulkan_core.h:6916
   end record;
   pragma Convention (C_Pass_By_Copy, VkDisplayPlaneInfo2KHR);  -- vulkan_core.h:6912

   type VkDisplayPlaneCapabilities2KHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:6920
      pNext : System.Address;  -- vulkan_core.h:6921
      capabilities : aliased VkDisplayPlaneCapabilitiesKHR;  -- vulkan_core.h:6922
   end record;
   pragma Convention (C_Pass_By_Copy, VkDisplayPlaneCapabilities2KHR);  -- vulkan_core.h:6919

   type PFN_vkGetPhysicalDeviceDisplayProperties2KHR is access function
        (arg1 : VkPhysicalDevice;
         arg2 : access stdint_h.uint32_t;
         arg3 : access VkDisplayProperties2KHR) return VkResult;
   pragma Convention (C, PFN_vkGetPhysicalDeviceDisplayProperties2KHR);  -- vulkan_core.h:6925

   type PFN_vkGetPhysicalDeviceDisplayPlaneProperties2KHR is access function
        (arg1 : VkPhysicalDevice;
         arg2 : access stdint_h.uint32_t;
         arg3 : access VkDisplayPlaneProperties2KHR) return VkResult;
   pragma Convention (C, PFN_vkGetPhysicalDeviceDisplayPlaneProperties2KHR);  -- vulkan_core.h:6926

   type PFN_vkGetDisplayModeProperties2KHR is access function
        (arg1 : VkPhysicalDevice;
         arg2 : VkDisplayKHR;
         arg3 : access stdint_h.uint32_t;
         arg4 : access VkDisplayModeProperties2KHR) return VkResult;
   pragma Convention (C, PFN_vkGetDisplayModeProperties2KHR);  -- vulkan_core.h:6927

   type PFN_vkGetDisplayPlaneCapabilities2KHR is access function
        (arg1 : VkPhysicalDevice;
         arg2 : System.Address;
         arg3 : access VkDisplayPlaneCapabilities2KHR) return VkResult;
   pragma Convention (C, PFN_vkGetDisplayPlaneCapabilities2KHR);  -- vulkan_core.h:6928

   function vkGetPhysicalDeviceDisplayProperties2KHR
     (physicalDevice : VkPhysicalDevice;
      pPropertyCount : access stdint_h.uint32_t;
      pProperties : access VkDisplayProperties2KHR) return VkResult;  -- vulkan_core.h:6931
   pragma Import (C, vkGetPhysicalDeviceDisplayProperties2KHR, "vkGetPhysicalDeviceDisplayProperties2KHR");

   function vkGetPhysicalDeviceDisplayPlaneProperties2KHR
     (physicalDevice : VkPhysicalDevice;
      pPropertyCount : access stdint_h.uint32_t;
      pProperties : access VkDisplayPlaneProperties2KHR) return VkResult;  -- vulkan_core.h:6936
   pragma Import (C, vkGetPhysicalDeviceDisplayPlaneProperties2KHR, "vkGetPhysicalDeviceDisplayPlaneProperties2KHR");

   function vkGetDisplayModeProperties2KHR
     (physicalDevice : VkPhysicalDevice;
      display : VkDisplayKHR;
      pPropertyCount : access stdint_h.uint32_t;
      pProperties : access VkDisplayModeProperties2KHR) return VkResult;  -- vulkan_core.h:6941
   pragma Import (C, vkGetDisplayModeProperties2KHR, "vkGetDisplayModeProperties2KHR");

   function vkGetDisplayPlaneCapabilities2KHR
     (physicalDevice : VkPhysicalDevice;
      pDisplayPlaneInfo : System.Address;
      pCapabilities : access VkDisplayPlaneCapabilities2KHR) return VkResult;  -- vulkan_core.h:6947
   pragma Import (C, vkGetDisplayPlaneCapabilities2KHR, "vkGetDisplayPlaneCapabilities2KHR");

   subtype VkMemoryDedicatedRequirementsKHR is VkMemoryDedicatedRequirements;

   subtype VkMemoryDedicatedAllocateInfoKHR is VkMemoryDedicatedAllocateInfo;

   subtype VkBufferMemoryRequirementsInfo2KHR is VkBufferMemoryRequirementsInfo2;

   subtype VkImageMemoryRequirementsInfo2KHR is VkImageMemoryRequirementsInfo2;

   subtype VkImageSparseMemoryRequirementsInfo2KHR is VkImageSparseMemoryRequirementsInfo2;

   subtype VkMemoryRequirements2KHR is VkMemoryRequirements2;

   subtype VkSparseImageMemoryRequirements2KHR is VkSparseImageMemoryRequirements2;

   type PFN_vkGetImageMemoryRequirements2KHR is access procedure
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : access VkMemoryRequirements2);
   pragma Convention (C, PFN_vkGetImageMemoryRequirements2KHR);  -- vulkan_core.h:6986

   type PFN_vkGetBufferMemoryRequirements2KHR is access procedure
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : access VkMemoryRequirements2);
   pragma Convention (C, PFN_vkGetBufferMemoryRequirements2KHR);  -- vulkan_core.h:6987

   type PFN_vkGetImageSparseMemoryRequirements2KHR is access procedure
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : access stdint_h.uint32_t;
         arg4 : access VkSparseImageMemoryRequirements2);
   pragma Convention (C, PFN_vkGetImageSparseMemoryRequirements2KHR);  -- vulkan_core.h:6988

   procedure vkGetImageMemoryRequirements2KHR
     (device : VkDevice;
      pInfo : System.Address;
      pMemoryRequirements : access VkMemoryRequirements2);  -- vulkan_core.h:6991
   pragma Import (C, vkGetImageMemoryRequirements2KHR, "vkGetImageMemoryRequirements2KHR");

   procedure vkGetBufferMemoryRequirements2KHR
     (device : VkDevice;
      pInfo : System.Address;
      pMemoryRequirements : access VkMemoryRequirements2);  -- vulkan_core.h:6996
   pragma Import (C, vkGetBufferMemoryRequirements2KHR, "vkGetBufferMemoryRequirements2KHR");

   procedure vkGetImageSparseMemoryRequirements2KHR
     (device : VkDevice;
      pInfo : System.Address;
      pSparseMemoryRequirementCount : access stdint_h.uint32_t;
      pSparseMemoryRequirements : access VkSparseImageMemoryRequirements2);  -- vulkan_core.h:7001
   pragma Import (C, vkGetImageSparseMemoryRequirements2KHR, "vkGetImageSparseMemoryRequirements2KHR");

   subtype VkImageFormatListCreateInfoKHR is VkImageFormatListCreateInfo;

   subtype VkSamplerYcbcrConversionKHR is VkSamplerYcbcrConversion;  -- vulkan_core.h:7017

   subtype VkSamplerYcbcrModelConversionKHR is VkSamplerYcbcrModelConversion;

   subtype VkSamplerYcbcrRangeKHR is VkSamplerYcbcrRange;

   subtype VkChromaLocationKHR is VkChromaLocation;

   subtype VkSamplerYcbcrConversionCreateInfoKHR is VkSamplerYcbcrConversionCreateInfo;

   subtype VkSamplerYcbcrConversionInfoKHR is VkSamplerYcbcrConversionInfo;

   subtype VkBindImagePlaneMemoryInfoKHR is VkBindImagePlaneMemoryInfo;

   subtype VkImagePlaneMemoryRequirementsInfoKHR is VkImagePlaneMemoryRequirementsInfo;

   subtype VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR is VkPhysicalDeviceSamplerYcbcrConversionFeatures;

   subtype VkSamplerYcbcrConversionImageFormatPropertiesKHR is VkSamplerYcbcrConversionImageFormatProperties;

   type PFN_vkCreateSamplerYcbcrConversionKHR is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : System.Address;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateSamplerYcbcrConversionKHR);  -- vulkan_core.h:7039

   type PFN_vkDestroySamplerYcbcrConversionKHR is access procedure
        (arg1 : VkDevice;
         arg2 : VkSamplerYcbcrConversion;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkDestroySamplerYcbcrConversionKHR);  -- vulkan_core.h:7040

   function vkCreateSamplerYcbcrConversionKHR
     (device : VkDevice;
      pCreateInfo : System.Address;
      pAllocator : System.Address;
      pYcbcrConversion : System.Address) return VkResult;  -- vulkan_core.h:7043
   pragma Import (C, vkCreateSamplerYcbcrConversionKHR, "vkCreateSamplerYcbcrConversionKHR");

   procedure vkDestroySamplerYcbcrConversionKHR
     (device : VkDevice;
      ycbcrConversion : VkSamplerYcbcrConversion;
      pAllocator : System.Address);  -- vulkan_core.h:7049
   pragma Import (C, vkDestroySamplerYcbcrConversionKHR, "vkDestroySamplerYcbcrConversionKHR");

   subtype VkBindBufferMemoryInfoKHR is VkBindBufferMemoryInfo;

   subtype VkBindImageMemoryInfoKHR is VkBindImageMemoryInfo;

   type PFN_vkBindBufferMemory2KHR is access function
        (arg1 : VkDevice;
         arg2 : stdint_h.uint32_t;
         arg3 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkBindBufferMemory2KHR);  -- vulkan_core.h:7063

   type PFN_vkBindImageMemory2KHR is access function
        (arg1 : VkDevice;
         arg2 : stdint_h.uint32_t;
         arg3 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkBindImageMemory2KHR);  -- vulkan_core.h:7064

   function vkBindBufferMemory2KHR
     (device : VkDevice;
      bindInfoCount : stdint_h.uint32_t;
      pBindInfos : System.Address) return VkResult;  -- vulkan_core.h:7067
   pragma Import (C, vkBindBufferMemory2KHR, "vkBindBufferMemory2KHR");

   function vkBindImageMemory2KHR
     (device : VkDevice;
      bindInfoCount : stdint_h.uint32_t;
      pBindInfos : System.Address) return VkResult;  -- vulkan_core.h:7072
   pragma Import (C, vkBindImageMemory2KHR, "vkBindImageMemory2KHR");

   subtype VkPhysicalDeviceMaintenance3PropertiesKHR is VkPhysicalDeviceMaintenance3Properties;

   subtype VkDescriptorSetLayoutSupportKHR is VkDescriptorSetLayoutSupport;

   type PFN_vkGetDescriptorSetLayoutSupportKHR is access procedure
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : access VkDescriptorSetLayoutSupport);
   pragma Convention (C, PFN_vkGetDescriptorSetLayoutSupportKHR);  -- vulkan_core.h:7086

   procedure vkGetDescriptorSetLayoutSupportKHR
     (device : VkDevice;
      pCreateInfo : System.Address;
      pSupport : access VkDescriptorSetLayoutSupport);  -- vulkan_core.h:7089
   pragma Import (C, vkGetDescriptorSetLayoutSupportKHR, "vkGetDescriptorSetLayoutSupportKHR");

   type PFN_vkCmdDrawIndirectCountKHR is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkBuffer;
         arg3 : VkDeviceSize;
         arg4 : VkBuffer;
         arg5 : VkDeviceSize;
         arg6 : stdint_h.uint32_t;
         arg7 : stdint_h.uint32_t);
   pragma Convention (C, PFN_vkCmdDrawIndirectCountKHR);  -- vulkan_core.h:7099

   type PFN_vkCmdDrawIndexedIndirectCountKHR is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkBuffer;
         arg3 : VkDeviceSize;
         arg4 : VkBuffer;
         arg5 : VkDeviceSize;
         arg6 : stdint_h.uint32_t;
         arg7 : stdint_h.uint32_t);
   pragma Convention (C, PFN_vkCmdDrawIndexedIndirectCountKHR);  -- vulkan_core.h:7100

   procedure vkCmdDrawIndirectCountKHR
     (commandBuffer : VkCommandBuffer;
      buffer : VkBuffer;
      offset : VkDeviceSize;
      countBuffer : VkBuffer;
      countBufferOffset : VkDeviceSize;
      maxDrawCount : stdint_h.uint32_t;
      stride : stdint_h.uint32_t);  -- vulkan_core.h:7103
   pragma Import (C, vkCmdDrawIndirectCountKHR, "vkCmdDrawIndirectCountKHR");

   procedure vkCmdDrawIndexedIndirectCountKHR
     (commandBuffer : VkCommandBuffer;
      buffer : VkBuffer;
      offset : VkDeviceSize;
      countBuffer : VkBuffer;
      countBufferOffset : VkDeviceSize;
      maxDrawCount : stdint_h.uint32_t;
      stride : stdint_h.uint32_t);  -- vulkan_core.h:7112
   pragma Import (C, vkCmdDrawIndexedIndirectCountKHR, "vkCmdDrawIndexedIndirectCountKHR");

   subtype VkPhysicalDeviceShaderSubgroupExtendedTypesFeaturesKHR is VkPhysicalDeviceShaderSubgroupExtendedTypesFeatures;

   subtype VkPhysicalDevice8BitStorageFeaturesKHR is VkPhysicalDevice8BitStorageFeatures;

   subtype VkPhysicalDeviceShaderAtomicInt64FeaturesKHR is VkPhysicalDeviceShaderAtomicInt64Features;

   type VkPhysicalDeviceShaderClockFeaturesKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:7148
      pNext : System.Address;  -- vulkan_core.h:7149
      shaderSubgroupClock : aliased VkBool32;  -- vulkan_core.h:7150
      shaderDeviceClock : aliased VkBool32;  -- vulkan_core.h:7151
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceShaderClockFeaturesKHR);  -- vulkan_core.h:7147

   subtype VkDriverIdKHR is VkDriverId;

   subtype VkConformanceVersionKHR is VkConformanceVersion;

   subtype VkPhysicalDeviceDriverPropertiesKHR is VkPhysicalDeviceDriverProperties;

   subtype VkShaderFloatControlsIndependenceKHR is VkShaderFloatControlsIndependence;

   subtype VkPhysicalDeviceFloatControlsPropertiesKHR is VkPhysicalDeviceFloatControlsProperties;

   subtype VkResolveModeFlagBitsKHR is VkResolveModeFlagBits;

   subtype VkResolveModeFlagsKHR is VkResolveModeFlags;  -- vulkan_core.h:7183

   subtype VkSubpassDescriptionDepthStencilResolveKHR is VkSubpassDescriptionDepthStencilResolve;

   subtype VkPhysicalDeviceDepthStencilResolvePropertiesKHR is VkPhysicalDeviceDepthStencilResolveProperties;

   subtype VkSemaphoreTypeKHR is VkSemaphoreType;

   subtype VkSemaphoreWaitFlagBitsKHR is VkSemaphoreWaitFlagBits;

   subtype VkSemaphoreWaitFlagsKHR is VkSemaphoreWaitFlags;  -- vulkan_core.h:7203

   subtype VkPhysicalDeviceTimelineSemaphoreFeaturesKHR is VkPhysicalDeviceTimelineSemaphoreFeatures;

   subtype VkPhysicalDeviceTimelineSemaphorePropertiesKHR is VkPhysicalDeviceTimelineSemaphoreProperties;

   subtype VkSemaphoreTypeCreateInfoKHR is VkSemaphoreTypeCreateInfo;

   subtype VkTimelineSemaphoreSubmitInfoKHR is VkTimelineSemaphoreSubmitInfo;

   subtype VkSemaphoreWaitInfoKHR is VkSemaphoreWaitInfo;

   subtype VkSemaphoreSignalInfoKHR is VkSemaphoreSignalInfo;

   type PFN_vkGetSemaphoreCounterValueKHR is access function
        (arg1 : VkDevice;
         arg2 : VkSemaphore;
         arg3 : access stdint_h.uint64_t) return VkResult;
   pragma Convention (C, PFN_vkGetSemaphoreCounterValueKHR);  -- vulkan_core.h:7217

   type PFN_vkWaitSemaphoresKHR is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : stdint_h.uint64_t) return VkResult;
   pragma Convention (C, PFN_vkWaitSemaphoresKHR);  -- vulkan_core.h:7218

   type PFN_vkSignalSemaphoreKHR is access function (arg1 : VkDevice; arg2 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkSignalSemaphoreKHR);  -- vulkan_core.h:7219

   function vkGetSemaphoreCounterValueKHR
     (device : VkDevice;
      semaphore : VkSemaphore;
      pValue : access stdint_h.uint64_t) return VkResult;  -- vulkan_core.h:7222
   pragma Import (C, vkGetSemaphoreCounterValueKHR, "vkGetSemaphoreCounterValueKHR");

   function vkWaitSemaphoresKHR
     (device : VkDevice;
      pWaitInfo : System.Address;
      timeout : stdint_h.uint64_t) return VkResult;  -- vulkan_core.h:7227
   pragma Import (C, vkWaitSemaphoresKHR, "vkWaitSemaphoresKHR");

   function vkSignalSemaphoreKHR (device : VkDevice; pSignalInfo : System.Address) return VkResult;  -- vulkan_core.h:7232
   pragma Import (C, vkSignalSemaphoreKHR, "vkSignalSemaphoreKHR");

   subtype VkPhysicalDeviceVulkanMemoryModelFeaturesKHR is VkPhysicalDeviceVulkanMemoryModelFeatures;

   type VkPhysicalDeviceShaderTerminateInvocationFeaturesKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:7249
      pNext : System.Address;  -- vulkan_core.h:7250
      shaderTerminateInvocation : aliased VkBool32;  -- vulkan_core.h:7251
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceShaderTerminateInvocationFeaturesKHR);  -- vulkan_core.h:7248

   subtype VkFragmentShadingRateCombinerOpKHR is unsigned;
   VK_FRAGMENT_SHADING_RATE_COMBINER_OP_KEEP_KHR : constant VkFragmentShadingRateCombinerOpKHR := 0;
   VK_FRAGMENT_SHADING_RATE_COMBINER_OP_REPLACE_KHR : constant VkFragmentShadingRateCombinerOpKHR := 1;
   VK_FRAGMENT_SHADING_RATE_COMBINER_OP_MIN_KHR : constant VkFragmentShadingRateCombinerOpKHR := 2;
   VK_FRAGMENT_SHADING_RATE_COMBINER_OP_MAX_KHR : constant VkFragmentShadingRateCombinerOpKHR := 3;
   VK_FRAGMENT_SHADING_RATE_COMBINER_OP_MUL_KHR : constant VkFragmentShadingRateCombinerOpKHR := 4;
   VK_FRAGMENT_SHADING_RATE_COMBINER_OP_MAX_ENUM_KHR : constant VkFragmentShadingRateCombinerOpKHR := 2147483647;  -- vulkan_core.h:7260

   type VkFragmentShadingRateAttachmentInfoKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:7269
      pNext : System.Address;  -- vulkan_core.h:7270
      pFragmentShadingRateAttachment : System.Address;  -- vulkan_core.h:7271
      shadingRateAttachmentTexelSize : aliased VkExtent2D;  -- vulkan_core.h:7272
   end record;
   pragma Convention (C_Pass_By_Copy, VkFragmentShadingRateAttachmentInfoKHR);  -- vulkan_core.h:7268

   type VkPipelineFragmentShadingRateStateCreateInfoKHR_combinerOps_array is array (0 .. 1) of aliased VkFragmentShadingRateCombinerOpKHR;
   type VkPipelineFragmentShadingRateStateCreateInfoKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:7276
      pNext : System.Address;  -- vulkan_core.h:7277
      fragmentSize : aliased VkExtent2D;  -- vulkan_core.h:7278
      combinerOps : aliased VkPipelineFragmentShadingRateStateCreateInfoKHR_combinerOps_array;  -- vulkan_core.h:7279
   end record;
   pragma Convention (C_Pass_By_Copy, VkPipelineFragmentShadingRateStateCreateInfoKHR);  -- vulkan_core.h:7275

   type VkPhysicalDeviceFragmentShadingRateFeaturesKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:7283
      pNext : System.Address;  -- vulkan_core.h:7284
      pipelineFragmentShadingRate : aliased VkBool32;  -- vulkan_core.h:7285
      primitiveFragmentShadingRate : aliased VkBool32;  -- vulkan_core.h:7286
      attachmentFragmentShadingRate : aliased VkBool32;  -- vulkan_core.h:7287
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceFragmentShadingRateFeaturesKHR);  -- vulkan_core.h:7282

   type VkPhysicalDeviceFragmentShadingRatePropertiesKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:7291
      pNext : System.Address;  -- vulkan_core.h:7292
      minFragmentShadingRateAttachmentTexelSize : aliased VkExtent2D;  -- vulkan_core.h:7293
      maxFragmentShadingRateAttachmentTexelSize : aliased VkExtent2D;  -- vulkan_core.h:7294
      maxFragmentShadingRateAttachmentTexelSizeAspectRatio : aliased stdint_h.uint32_t;  -- vulkan_core.h:7295
      primitiveFragmentShadingRateWithMultipleViewports : aliased VkBool32;  -- vulkan_core.h:7296
      layeredShadingRateAttachments : aliased VkBool32;  -- vulkan_core.h:7297
      fragmentShadingRateNonTrivialCombinerOps : aliased VkBool32;  -- vulkan_core.h:7298
      maxFragmentSize : aliased VkExtent2D;  -- vulkan_core.h:7299
      maxFragmentSizeAspectRatio : aliased stdint_h.uint32_t;  -- vulkan_core.h:7300
      maxFragmentShadingRateCoverageSamples : aliased stdint_h.uint32_t;  -- vulkan_core.h:7301
      maxFragmentShadingRateRasterizationSamples : aliased VkSampleCountFlagBits;  -- vulkan_core.h:7302
      fragmentShadingRateWithShaderDepthStencilWrites : aliased VkBool32;  -- vulkan_core.h:7303
      fragmentShadingRateWithSampleMask : aliased VkBool32;  -- vulkan_core.h:7304
      fragmentShadingRateWithShaderSampleMask : aliased VkBool32;  -- vulkan_core.h:7305
      fragmentShadingRateWithConservativeRasterization : aliased VkBool32;  -- vulkan_core.h:7306
      fragmentShadingRateWithFragmentShaderInterlock : aliased VkBool32;  -- vulkan_core.h:7307
      fragmentShadingRateWithCustomSampleLocations : aliased VkBool32;  -- vulkan_core.h:7308
      fragmentShadingRateStrictMultiplyCombiner : aliased VkBool32;  -- vulkan_core.h:7309
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceFragmentShadingRatePropertiesKHR);  -- vulkan_core.h:7290

   type VkPhysicalDeviceFragmentShadingRateKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:7313
      pNext : System.Address;  -- vulkan_core.h:7314
      sampleCounts : aliased VkSampleCountFlags;  -- vulkan_core.h:7315
      fragmentSize : aliased VkExtent2D;  -- vulkan_core.h:7316
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceFragmentShadingRateKHR);  -- vulkan_core.h:7312

   type PFN_vkGetPhysicalDeviceFragmentShadingRatesKHR is access function
        (arg1 : VkPhysicalDevice;
         arg2 : access stdint_h.uint32_t;
         arg3 : access VkPhysicalDeviceFragmentShadingRateKHR) return VkResult;
   pragma Convention (C, PFN_vkGetPhysicalDeviceFragmentShadingRatesKHR);  -- vulkan_core.h:7319

   type PFN_vkCmdSetFragmentShadingRateKHR is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : System.Address;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkCmdSetFragmentShadingRateKHR);  -- vulkan_core.h:7320

   function vkGetPhysicalDeviceFragmentShadingRatesKHR
     (physicalDevice : VkPhysicalDevice;
      pFragmentShadingRateCount : access stdint_h.uint32_t;
      pFragmentShadingRates : access VkPhysicalDeviceFragmentShadingRateKHR) return VkResult;  -- vulkan_core.h:7323
   pragma Import (C, vkGetPhysicalDeviceFragmentShadingRatesKHR, "vkGetPhysicalDeviceFragmentShadingRatesKHR");

   procedure vkCmdSetFragmentShadingRateKHR
     (commandBuffer : VkCommandBuffer;
      pFragmentSize : System.Address;
      combinerOps : System.Address);  -- vulkan_core.h:7328
   pragma Import (C, vkCmdSetFragmentShadingRateKHR, "vkCmdSetFragmentShadingRateKHR");

   type VkSurfaceProtectedCapabilitiesKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:7344
      pNext : System.Address;  -- vulkan_core.h:7345
      supportsProtected : aliased VkBool32;  -- vulkan_core.h:7346
   end record;
   pragma Convention (C_Pass_By_Copy, VkSurfaceProtectedCapabilitiesKHR);  -- vulkan_core.h:7343

   subtype VkPhysicalDeviceSeparateDepthStencilLayoutsFeaturesKHR is VkPhysicalDeviceSeparateDepthStencilLayoutsFeatures;

   subtype VkAttachmentReferenceStencilLayoutKHR is VkAttachmentReferenceStencilLayout;

   subtype VkAttachmentDescriptionStencilLayoutKHR is VkAttachmentDescriptionStencilLayout;

   subtype VkPhysicalDeviceUniformBufferStandardLayoutFeaturesKHR is VkPhysicalDeviceUniformBufferStandardLayoutFeatures;

   subtype VkPhysicalDeviceBufferDeviceAddressFeaturesKHR is VkPhysicalDeviceBufferDeviceAddressFeatures;

   subtype VkBufferDeviceAddressInfoKHR is VkBufferDeviceAddressInfo;

   subtype VkBufferOpaqueCaptureAddressCreateInfoKHR is VkBufferOpaqueCaptureAddressCreateInfo;

   subtype VkMemoryOpaqueCaptureAddressAllocateInfoKHR is VkMemoryOpaqueCaptureAddressAllocateInfo;

   subtype VkDeviceMemoryOpaqueCaptureAddressInfoKHR is VkDeviceMemoryOpaqueCaptureAddressInfo;

   type PFN_vkGetBufferDeviceAddressKHR is access function (arg1 : VkDevice; arg2 : System.Address) return VkDeviceAddress;
   pragma Convention (C, PFN_vkGetBufferDeviceAddressKHR);  -- vulkan_core.h:7382

   type PFN_vkGetBufferOpaqueCaptureAddressKHR is access function (arg1 : VkDevice; arg2 : System.Address) return stdint_h.uint64_t;
   pragma Convention (C, PFN_vkGetBufferOpaqueCaptureAddressKHR);  -- vulkan_core.h:7383

   type PFN_vkGetDeviceMemoryOpaqueCaptureAddressKHR is access function (arg1 : VkDevice; arg2 : System.Address) return stdint_h.uint64_t;
   pragma Convention (C, PFN_vkGetDeviceMemoryOpaqueCaptureAddressKHR);  -- vulkan_core.h:7384

   function vkGetBufferDeviceAddressKHR (device : VkDevice; pInfo : System.Address) return VkDeviceAddress;  -- vulkan_core.h:7387
   pragma Import (C, vkGetBufferDeviceAddressKHR, "vkGetBufferDeviceAddressKHR");

   function vkGetBufferOpaqueCaptureAddressKHR (device : VkDevice; pInfo : System.Address) return stdint_h.uint64_t;  -- vulkan_core.h:7391
   pragma Import (C, vkGetBufferOpaqueCaptureAddressKHR, "vkGetBufferOpaqueCaptureAddressKHR");

   function vkGetDeviceMemoryOpaqueCaptureAddressKHR (device : VkDevice; pInfo : System.Address) return stdint_h.uint64_t;  -- vulkan_core.h:7395
   pragma Import (C, vkGetDeviceMemoryOpaqueCaptureAddressKHR, "vkGetDeviceMemoryOpaqueCaptureAddressKHR");

   type VkDeferredOperationKHR is new System.Address;  -- vulkan_core.h:7402

   --  skipped empty struct VkDeferredOperationKHR_T

   type PFN_vkCreateDeferredOperationKHR is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateDeferredOperationKHR);  -- vulkan_core.h:7405

   type PFN_vkDestroyDeferredOperationKHR is access procedure
        (arg1 : VkDevice;
         arg2 : VkDeferredOperationKHR;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkDestroyDeferredOperationKHR);  -- vulkan_core.h:7406

   type PFN_vkGetDeferredOperationMaxConcurrencyKHR is access function (arg1 : VkDevice; arg2 : VkDeferredOperationKHR) return stdint_h.uint32_t;
   pragma Convention (C, PFN_vkGetDeferredOperationMaxConcurrencyKHR);  -- vulkan_core.h:7407

   type PFN_vkGetDeferredOperationResultKHR is access function (arg1 : VkDevice; arg2 : VkDeferredOperationKHR) return VkResult;
   pragma Convention (C, PFN_vkGetDeferredOperationResultKHR);  -- vulkan_core.h:7408

   type PFN_vkDeferredOperationJoinKHR is access function (arg1 : VkDevice; arg2 : VkDeferredOperationKHR) return VkResult;
   pragma Convention (C, PFN_vkDeferredOperationJoinKHR);  -- vulkan_core.h:7409

   function vkCreateDeferredOperationKHR
     (device : VkDevice;
      pAllocator : System.Address;
      pDeferredOperation : System.Address) return VkResult;  -- vulkan_core.h:7412
   pragma Import (C, vkCreateDeferredOperationKHR, "vkCreateDeferredOperationKHR");

   procedure vkDestroyDeferredOperationKHR
     (device : VkDevice;
      operation : VkDeferredOperationKHR;
      pAllocator : System.Address);  -- vulkan_core.h:7417
   pragma Import (C, vkDestroyDeferredOperationKHR, "vkDestroyDeferredOperationKHR");

   function vkGetDeferredOperationMaxConcurrencyKHR (device : VkDevice; operation : VkDeferredOperationKHR) return stdint_h.uint32_t;  -- vulkan_core.h:7422
   pragma Import (C, vkGetDeferredOperationMaxConcurrencyKHR, "vkGetDeferredOperationMaxConcurrencyKHR");

   function vkGetDeferredOperationResultKHR (device : VkDevice; operation : VkDeferredOperationKHR) return VkResult;  -- vulkan_core.h:7426
   pragma Import (C, vkGetDeferredOperationResultKHR, "vkGetDeferredOperationResultKHR");

   function vkDeferredOperationJoinKHR (device : VkDevice; operation : VkDeferredOperationKHR) return VkResult;  -- vulkan_core.h:7430
   pragma Import (C, vkDeferredOperationJoinKHR, "vkDeferredOperationJoinKHR");

   subtype VkPipelineExecutableStatisticFormatKHR is unsigned;
   VK_PIPELINE_EXECUTABLE_STATISTIC_FORMAT_BOOL32_KHR : constant VkPipelineExecutableStatisticFormatKHR := 0;
   VK_PIPELINE_EXECUTABLE_STATISTIC_FORMAT_INT64_KHR : constant VkPipelineExecutableStatisticFormatKHR := 1;
   VK_PIPELINE_EXECUTABLE_STATISTIC_FORMAT_UINT64_KHR : constant VkPipelineExecutableStatisticFormatKHR := 2;
   VK_PIPELINE_EXECUTABLE_STATISTIC_FORMAT_FLOAT64_KHR : constant VkPipelineExecutableStatisticFormatKHR := 3;
   VK_PIPELINE_EXECUTABLE_STATISTIC_FORMAT_MAX_ENUM_KHR : constant VkPipelineExecutableStatisticFormatKHR := 2147483647;  -- vulkan_core.h:7440

   type VkPhysicalDevicePipelineExecutablePropertiesFeaturesKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:7448
      pNext : System.Address;  -- vulkan_core.h:7449
      pipelineExecutableInfo : aliased VkBool32;  -- vulkan_core.h:7450
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDevicePipelineExecutablePropertiesFeaturesKHR);  -- vulkan_core.h:7447

   type VkPipelineInfoKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:7454
      pNext : System.Address;  -- vulkan_core.h:7455
      pipeline : VkPipeline;  -- vulkan_core.h:7456
   end record;
   pragma Convention (C_Pass_By_Copy, VkPipelineInfoKHR);  -- vulkan_core.h:7453

   subtype VkPipelineExecutablePropertiesKHR_name_array is Interfaces.C.char_array (0 .. 255);
   subtype VkPipelineExecutablePropertiesKHR_description_array is Interfaces.C.char_array (0 .. 255);
   type VkPipelineExecutablePropertiesKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:7460
      pNext : System.Address;  -- vulkan_core.h:7461
      stages : aliased VkShaderStageFlags;  -- vulkan_core.h:7462
      name : aliased VkPipelineExecutablePropertiesKHR_name_array;  -- vulkan_core.h:7463
      description : aliased VkPipelineExecutablePropertiesKHR_description_array;  -- vulkan_core.h:7464
      subgroupSize : aliased stdint_h.uint32_t;  -- vulkan_core.h:7465
   end record;
   pragma Convention (C_Pass_By_Copy, VkPipelineExecutablePropertiesKHR);  -- vulkan_core.h:7459

   type VkPipelineExecutableInfoKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:7469
      pNext : System.Address;  -- vulkan_core.h:7470
      pipeline : VkPipeline;  -- vulkan_core.h:7471
      executableIndex : aliased stdint_h.uint32_t;  -- vulkan_core.h:7472
   end record;
   pragma Convention (C_Pass_By_Copy, VkPipelineExecutableInfoKHR);  -- vulkan_core.h:7468

   type VkPipelineExecutableStatisticValueKHR (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            b32 : aliased VkBool32;  -- vulkan_core.h:7476
         when 1 =>
            i64 : aliased stdint_h.int64_t;  -- vulkan_core.h:7477
         when 2 =>
            u64 : aliased stdint_h.uint64_t;  -- vulkan_core.h:7478
         when others =>
            f64 : aliased double;  -- vulkan_core.h:7479
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, VkPipelineExecutableStatisticValueKHR);
   pragma Unchecked_Union (VkPipelineExecutableStatisticValueKHR);  -- vulkan_core.h:7475

   subtype VkPipelineExecutableStatisticKHR_name_array is Interfaces.C.char_array (0 .. 255);
   subtype VkPipelineExecutableStatisticKHR_description_array is Interfaces.C.char_array (0 .. 255);
   type VkPipelineExecutableStatisticKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:7483
      pNext : System.Address;  -- vulkan_core.h:7484
      name : aliased VkPipelineExecutableStatisticKHR_name_array;  -- vulkan_core.h:7485
      description : aliased VkPipelineExecutableStatisticKHR_description_array;  -- vulkan_core.h:7486
      format : aliased VkPipelineExecutableStatisticFormatKHR;  -- vulkan_core.h:7487
      value : VkPipelineExecutableStatisticValueKHR;  -- vulkan_core.h:7488
   end record;
   pragma Convention (C_Pass_By_Copy, VkPipelineExecutableStatisticKHR);  -- vulkan_core.h:7482

   subtype VkPipelineExecutableInternalRepresentationKHR_name_array is Interfaces.C.char_array (0 .. 255);
   subtype VkPipelineExecutableInternalRepresentationKHR_description_array is Interfaces.C.char_array (0 .. 255);
   type VkPipelineExecutableInternalRepresentationKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:7492
      pNext : System.Address;  -- vulkan_core.h:7493
      name : aliased VkPipelineExecutableInternalRepresentationKHR_name_array;  -- vulkan_core.h:7494
      description : aliased VkPipelineExecutableInternalRepresentationKHR_description_array;  -- vulkan_core.h:7495
      isText : aliased VkBool32;  -- vulkan_core.h:7496
      dataSize : aliased crtdefs_h.size_t;  -- vulkan_core.h:7497
      pData : System.Address;  -- vulkan_core.h:7498
   end record;
   pragma Convention (C_Pass_By_Copy, VkPipelineExecutableInternalRepresentationKHR);  -- vulkan_core.h:7491

   type PFN_vkGetPipelineExecutablePropertiesKHR is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : access stdint_h.uint32_t;
         arg4 : access VkPipelineExecutablePropertiesKHR) return VkResult;
   pragma Convention (C, PFN_vkGetPipelineExecutablePropertiesKHR);  -- vulkan_core.h:7501

   type PFN_vkGetPipelineExecutableStatisticsKHR is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : access stdint_h.uint32_t;
         arg4 : access VkPipelineExecutableStatisticKHR) return VkResult;
   pragma Convention (C, PFN_vkGetPipelineExecutableStatisticsKHR);  -- vulkan_core.h:7502

   type PFN_vkGetPipelineExecutableInternalRepresentationsKHR is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : access stdint_h.uint32_t;
         arg4 : access VkPipelineExecutableInternalRepresentationKHR) return VkResult;
   pragma Convention (C, PFN_vkGetPipelineExecutableInternalRepresentationsKHR);  -- vulkan_core.h:7503

   function vkGetPipelineExecutablePropertiesKHR
     (device : VkDevice;
      pPipelineInfo : System.Address;
      pExecutableCount : access stdint_h.uint32_t;
      pProperties : access VkPipelineExecutablePropertiesKHR) return VkResult;  -- vulkan_core.h:7506
   pragma Import (C, vkGetPipelineExecutablePropertiesKHR, "vkGetPipelineExecutablePropertiesKHR");

   function vkGetPipelineExecutableStatisticsKHR
     (device : VkDevice;
      pExecutableInfo : System.Address;
      pStatisticCount : access stdint_h.uint32_t;
      pStatistics : access VkPipelineExecutableStatisticKHR) return VkResult;  -- vulkan_core.h:7512
   pragma Import (C, vkGetPipelineExecutableStatisticsKHR, "vkGetPipelineExecutableStatisticsKHR");

   function vkGetPipelineExecutableInternalRepresentationsKHR
     (device : VkDevice;
      pExecutableInfo : System.Address;
      pInternalRepresentationCount : access stdint_h.uint32_t;
      pInternalRepresentations : access VkPipelineExecutableInternalRepresentationKHR) return VkResult;  -- vulkan_core.h:7518
   pragma Import (C, vkGetPipelineExecutableInternalRepresentationsKHR, "vkGetPipelineExecutableInternalRepresentationsKHR");

   type VkPipelineLibraryCreateInfoKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:7530
      pNext : System.Address;  -- vulkan_core.h:7531
      libraryCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:7532
      pLibraries : System.Address;  -- vulkan_core.h:7533
   end record;
   pragma Convention (C_Pass_By_Copy, VkPipelineLibraryCreateInfoKHR);  -- vulkan_core.h:7529

   subtype VkFlags64 is stdint_h.uint64_t;  -- vulkan_core.h:7544

   subtype VkPipelineStageFlags2KHR is VkFlags64;  -- vulkan_core.h:7547

  -- Flag bits for VkPipelineStageFlags2KHR
   VK_PIPELINE_STAGE_2_NONE_KHR : aliased VkPipelineStageFlags2KHR;  -- vulkan_core.h:7550
   pragma Import (CPP, VK_PIPELINE_STAGE_2_NONE_KHR, "_ZL28VK_PIPELINE_STAGE_2_NONE_KHR");

   VK_PIPELINE_STAGE_2_TOP_OF_PIPE_BIT_KHR : aliased VkPipelineStageFlags2KHR;  -- vulkan_core.h:7551
   pragma Import (CPP, VK_PIPELINE_STAGE_2_TOP_OF_PIPE_BIT_KHR, "_ZL39VK_PIPELINE_STAGE_2_TOP_OF_PIPE_BIT_KHR");

   VK_PIPELINE_STAGE_2_DRAW_INDIRECT_BIT_KHR : aliased VkPipelineStageFlags2KHR;  -- vulkan_core.h:7552
   pragma Import (CPP, VK_PIPELINE_STAGE_2_DRAW_INDIRECT_BIT_KHR, "_ZL41VK_PIPELINE_STAGE_2_DRAW_INDIRECT_BIT_KHR");

   VK_PIPELINE_STAGE_2_VERTEX_INPUT_BIT_KHR : aliased VkPipelineStageFlags2KHR;  -- vulkan_core.h:7553
   pragma Import (CPP, VK_PIPELINE_STAGE_2_VERTEX_INPUT_BIT_KHR, "_ZL40VK_PIPELINE_STAGE_2_VERTEX_INPUT_BIT_KHR");

   VK_PIPELINE_STAGE_2_VERTEX_SHADER_BIT_KHR : aliased VkPipelineStageFlags2KHR;  -- vulkan_core.h:7554
   pragma Import (CPP, VK_PIPELINE_STAGE_2_VERTEX_SHADER_BIT_KHR, "_ZL41VK_PIPELINE_STAGE_2_VERTEX_SHADER_BIT_KHR");

   VK_PIPELINE_STAGE_2_TESSELLATION_CONTROL_SHADER_BIT_KHR : aliased VkPipelineStageFlags2KHR;  -- vulkan_core.h:7555
   pragma Import (CPP, VK_PIPELINE_STAGE_2_TESSELLATION_CONTROL_SHADER_BIT_KHR, "_ZL55VK_PIPELINE_STAGE_2_TESSELLATION_CONTROL_SHADER_BIT_KHR");

   VK_PIPELINE_STAGE_2_TESSELLATION_EVALUATION_SHADER_BIT_KHR : aliased VkPipelineStageFlags2KHR;  -- vulkan_core.h:7556
   pragma Import (CPP, VK_PIPELINE_STAGE_2_TESSELLATION_EVALUATION_SHADER_BIT_KHR, "_ZL58VK_PIPELINE_STAGE_2_TESSELLATION_EVALUATION_SHADER_BIT_KHR");

   VK_PIPELINE_STAGE_2_GEOMETRY_SHADER_BIT_KHR : aliased VkPipelineStageFlags2KHR;  -- vulkan_core.h:7557
   pragma Import (CPP, VK_PIPELINE_STAGE_2_GEOMETRY_SHADER_BIT_KHR, "_ZL43VK_PIPELINE_STAGE_2_GEOMETRY_SHADER_BIT_KHR");

   VK_PIPELINE_STAGE_2_FRAGMENT_SHADER_BIT_KHR : aliased VkPipelineStageFlags2KHR;  -- vulkan_core.h:7558
   pragma Import (CPP, VK_PIPELINE_STAGE_2_FRAGMENT_SHADER_BIT_KHR, "_ZL43VK_PIPELINE_STAGE_2_FRAGMENT_SHADER_BIT_KHR");

   VK_PIPELINE_STAGE_2_EARLY_FRAGMENT_TESTS_BIT_KHR : aliased VkPipelineStageFlags2KHR;  -- vulkan_core.h:7559
   pragma Import (CPP, VK_PIPELINE_STAGE_2_EARLY_FRAGMENT_TESTS_BIT_KHR, "_ZL48VK_PIPELINE_STAGE_2_EARLY_FRAGMENT_TESTS_BIT_KHR");

   VK_PIPELINE_STAGE_2_LATE_FRAGMENT_TESTS_BIT_KHR : aliased VkPipelineStageFlags2KHR;  -- vulkan_core.h:7560
   pragma Import (CPP, VK_PIPELINE_STAGE_2_LATE_FRAGMENT_TESTS_BIT_KHR, "_ZL47VK_PIPELINE_STAGE_2_LATE_FRAGMENT_TESTS_BIT_KHR");

   VK_PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT_KHR : aliased VkPipelineStageFlags2KHR;  -- vulkan_core.h:7561
   pragma Import (CPP, VK_PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT_KHR, "_ZL51VK_PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT_KHR");

   VK_PIPELINE_STAGE_2_COMPUTE_SHADER_BIT_KHR : aliased VkPipelineStageFlags2KHR;  -- vulkan_core.h:7562
   pragma Import (CPP, VK_PIPELINE_STAGE_2_COMPUTE_SHADER_BIT_KHR, "_ZL42VK_PIPELINE_STAGE_2_COMPUTE_SHADER_BIT_KHR");

   VK_PIPELINE_STAGE_2_ALL_TRANSFER_BIT_KHR : aliased VkPipelineStageFlags2KHR;  -- vulkan_core.h:7563
   pragma Import (CPP, VK_PIPELINE_STAGE_2_ALL_TRANSFER_BIT_KHR, "_ZL40VK_PIPELINE_STAGE_2_ALL_TRANSFER_BIT_KHR");

   VK_PIPELINE_STAGE_2_TRANSFER_BIT_KHR : aliased VkPipelineStageFlags2KHR;  -- vulkan_core.h:7564
   pragma Import (CPP, VK_PIPELINE_STAGE_2_TRANSFER_BIT_KHR, "_ZL36VK_PIPELINE_STAGE_2_TRANSFER_BIT_KHR");

   VK_PIPELINE_STAGE_2_BOTTOM_OF_PIPE_BIT_KHR : aliased VkPipelineStageFlags2KHR;  -- vulkan_core.h:7565
   pragma Import (CPP, VK_PIPELINE_STAGE_2_BOTTOM_OF_PIPE_BIT_KHR, "_ZL42VK_PIPELINE_STAGE_2_BOTTOM_OF_PIPE_BIT_KHR");

   VK_PIPELINE_STAGE_2_HOST_BIT_KHR : aliased VkPipelineStageFlags2KHR;  -- vulkan_core.h:7566
   pragma Import (CPP, VK_PIPELINE_STAGE_2_HOST_BIT_KHR, "_ZL32VK_PIPELINE_STAGE_2_HOST_BIT_KHR");

   VK_PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR : aliased VkPipelineStageFlags2KHR;  -- vulkan_core.h:7567
   pragma Import (CPP, VK_PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR, "_ZL40VK_PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR");

   VK_PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR : aliased VkPipelineStageFlags2KHR;  -- vulkan_core.h:7568
   pragma Import (CPP, VK_PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR, "_ZL40VK_PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR");

   VK_PIPELINE_STAGE_2_COPY_BIT_KHR : aliased VkPipelineStageFlags2KHR;  -- vulkan_core.h:7569
   pragma Import (CPP, VK_PIPELINE_STAGE_2_COPY_BIT_KHR, "_ZL32VK_PIPELINE_STAGE_2_COPY_BIT_KHR");

   VK_PIPELINE_STAGE_2_RESOLVE_BIT_KHR : aliased VkPipelineStageFlags2KHR;  -- vulkan_core.h:7570
   pragma Import (CPP, VK_PIPELINE_STAGE_2_RESOLVE_BIT_KHR, "_ZL35VK_PIPELINE_STAGE_2_RESOLVE_BIT_KHR");

   VK_PIPELINE_STAGE_2_BLIT_BIT_KHR : aliased VkPipelineStageFlags2KHR;  -- vulkan_core.h:7571
   pragma Import (CPP, VK_PIPELINE_STAGE_2_BLIT_BIT_KHR, "_ZL32VK_PIPELINE_STAGE_2_BLIT_BIT_KHR");

   VK_PIPELINE_STAGE_2_CLEAR_BIT_KHR : aliased VkPipelineStageFlags2KHR;  -- vulkan_core.h:7572
   pragma Import (CPP, VK_PIPELINE_STAGE_2_CLEAR_BIT_KHR, "_ZL33VK_PIPELINE_STAGE_2_CLEAR_BIT_KHR");

   VK_PIPELINE_STAGE_2_INDEX_INPUT_BIT_KHR : aliased VkPipelineStageFlags2KHR;  -- vulkan_core.h:7573
   pragma Import (CPP, VK_PIPELINE_STAGE_2_INDEX_INPUT_BIT_KHR, "_ZL39VK_PIPELINE_STAGE_2_INDEX_INPUT_BIT_KHR");

   VK_PIPELINE_STAGE_2_VERTEX_ATTRIBUTE_INPUT_BIT_KHR : aliased VkPipelineStageFlags2KHR;  -- vulkan_core.h:7574
   pragma Import (CPP, VK_PIPELINE_STAGE_2_VERTEX_ATTRIBUTE_INPUT_BIT_KHR, "_ZL50VK_PIPELINE_STAGE_2_VERTEX_ATTRIBUTE_INPUT_BIT_KHR");

   VK_PIPELINE_STAGE_2_PRE_RASTERIZATION_SHADERS_BIT_KHR : aliased VkPipelineStageFlags2KHR;  -- vulkan_core.h:7575
   pragma Import (CPP, VK_PIPELINE_STAGE_2_PRE_RASTERIZATION_SHADERS_BIT_KHR, "_ZL53VK_PIPELINE_STAGE_2_PRE_RASTERIZATION_SHADERS_BIT_KHR");

   VK_PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT : aliased VkPipelineStageFlags2KHR;  -- vulkan_core.h:7576
   pragma Import (CPP, VK_PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT, "_ZL46VK_PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT");

   VK_PIPELINE_STAGE_2_CONDITIONAL_RENDERING_BIT_EXT : aliased VkPipelineStageFlags2KHR;  -- vulkan_core.h:7577
   pragma Import (CPP, VK_PIPELINE_STAGE_2_CONDITIONAL_RENDERING_BIT_EXT, "_ZL49VK_PIPELINE_STAGE_2_CONDITIONAL_RENDERING_BIT_EXT");

   VK_PIPELINE_STAGE_2_COMMAND_PREPROCESS_BIT_NV : aliased VkPipelineStageFlags2KHR;  -- vulkan_core.h:7578
   pragma Import (CPP, VK_PIPELINE_STAGE_2_COMMAND_PREPROCESS_BIT_NV, "_ZL45VK_PIPELINE_STAGE_2_COMMAND_PREPROCESS_BIT_NV");

   VK_PIPELINE_STAGE_2_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR : aliased VkPipelineStageFlags2KHR;  -- vulkan_core.h:7579
   pragma Import (CPP, VK_PIPELINE_STAGE_2_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR, "_ZL60VK_PIPELINE_STAGE_2_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR");

   VK_PIPELINE_STAGE_2_SHADING_RATE_IMAGE_BIT_NV : aliased VkPipelineStageFlags2KHR;  -- vulkan_core.h:7580
   pragma Import (CPP, VK_PIPELINE_STAGE_2_SHADING_RATE_IMAGE_BIT_NV, "_ZL45VK_PIPELINE_STAGE_2_SHADING_RATE_IMAGE_BIT_NV");

   VK_PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR : aliased VkPipelineStageFlags2KHR;  -- vulkan_core.h:7581
   pragma Import (CPP, VK_PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR, "_ZL56VK_PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR");

   VK_PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_KHR : aliased VkPipelineStageFlags2KHR;  -- vulkan_core.h:7582
   pragma Import (CPP, VK_PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_KHR, "_ZL46VK_PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_KHR");

   VK_PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_NV : aliased VkPipelineStageFlags2KHR;  -- vulkan_core.h:7583
   pragma Import (CPP, VK_PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_NV, "_ZL45VK_PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_NV");

   VK_PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_NV : aliased VkPipelineStageFlags2KHR;  -- vulkan_core.h:7584
   pragma Import (CPP, VK_PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_NV, "_ZL55VK_PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_NV");

   VK_PIPELINE_STAGE_2_FRAGMENT_DENSITY_PROCESS_BIT_EXT : aliased VkPipelineStageFlags2KHR;  -- vulkan_core.h:7585
   pragma Import (CPP, VK_PIPELINE_STAGE_2_FRAGMENT_DENSITY_PROCESS_BIT_EXT, "_ZL52VK_PIPELINE_STAGE_2_FRAGMENT_DENSITY_PROCESS_BIT_EXT");

   VK_PIPELINE_STAGE_2_TASK_SHADER_BIT_NV : aliased VkPipelineStageFlags2KHR;  -- vulkan_core.h:7586
   pragma Import (CPP, VK_PIPELINE_STAGE_2_TASK_SHADER_BIT_NV, "_ZL38VK_PIPELINE_STAGE_2_TASK_SHADER_BIT_NV");

   VK_PIPELINE_STAGE_2_MESH_SHADER_BIT_NV : aliased VkPipelineStageFlags2KHR;  -- vulkan_core.h:7587
   pragma Import (CPP, VK_PIPELINE_STAGE_2_MESH_SHADER_BIT_NV, "_ZL38VK_PIPELINE_STAGE_2_MESH_SHADER_BIT_NV");

   subtype VkAccessFlags2KHR is VkFlags64;  -- vulkan_core.h:7589

  -- Flag bits for VkAccessFlags2KHR
   VK_ACCESS_2_NONE_KHR : aliased VkAccessFlags2KHR;  -- vulkan_core.h:7592
   pragma Import (CPP, VK_ACCESS_2_NONE_KHR, "_ZL20VK_ACCESS_2_NONE_KHR");

   VK_ACCESS_2_INDIRECT_COMMAND_READ_BIT_KHR : aliased VkAccessFlags2KHR;  -- vulkan_core.h:7593
   pragma Import (CPP, VK_ACCESS_2_INDIRECT_COMMAND_READ_BIT_KHR, "_ZL41VK_ACCESS_2_INDIRECT_COMMAND_READ_BIT_KHR");

   VK_ACCESS_2_INDEX_READ_BIT_KHR : aliased VkAccessFlags2KHR;  -- vulkan_core.h:7594
   pragma Import (CPP, VK_ACCESS_2_INDEX_READ_BIT_KHR, "_ZL30VK_ACCESS_2_INDEX_READ_BIT_KHR");

   VK_ACCESS_2_VERTEX_ATTRIBUTE_READ_BIT_KHR : aliased VkAccessFlags2KHR;  -- vulkan_core.h:7595
   pragma Import (CPP, VK_ACCESS_2_VERTEX_ATTRIBUTE_READ_BIT_KHR, "_ZL41VK_ACCESS_2_VERTEX_ATTRIBUTE_READ_BIT_KHR");

   VK_ACCESS_2_UNIFORM_READ_BIT_KHR : aliased VkAccessFlags2KHR;  -- vulkan_core.h:7596
   pragma Import (CPP, VK_ACCESS_2_UNIFORM_READ_BIT_KHR, "_ZL32VK_ACCESS_2_UNIFORM_READ_BIT_KHR");

   VK_ACCESS_2_INPUT_ATTACHMENT_READ_BIT_KHR : aliased VkAccessFlags2KHR;  -- vulkan_core.h:7597
   pragma Import (CPP, VK_ACCESS_2_INPUT_ATTACHMENT_READ_BIT_KHR, "_ZL41VK_ACCESS_2_INPUT_ATTACHMENT_READ_BIT_KHR");

   VK_ACCESS_2_SHADER_READ_BIT_KHR : aliased VkAccessFlags2KHR;  -- vulkan_core.h:7598
   pragma Import (CPP, VK_ACCESS_2_SHADER_READ_BIT_KHR, "_ZL31VK_ACCESS_2_SHADER_READ_BIT_KHR");

   VK_ACCESS_2_SHADER_WRITE_BIT_KHR : aliased VkAccessFlags2KHR;  -- vulkan_core.h:7599
   pragma Import (CPP, VK_ACCESS_2_SHADER_WRITE_BIT_KHR, "_ZL32VK_ACCESS_2_SHADER_WRITE_BIT_KHR");

   VK_ACCESS_2_COLOR_ATTACHMENT_READ_BIT_KHR : aliased VkAccessFlags2KHR;  -- vulkan_core.h:7600
   pragma Import (CPP, VK_ACCESS_2_COLOR_ATTACHMENT_READ_BIT_KHR, "_ZL41VK_ACCESS_2_COLOR_ATTACHMENT_READ_BIT_KHR");

   VK_ACCESS_2_COLOR_ATTACHMENT_WRITE_BIT_KHR : aliased VkAccessFlags2KHR;  -- vulkan_core.h:7601
   pragma Import (CPP, VK_ACCESS_2_COLOR_ATTACHMENT_WRITE_BIT_KHR, "_ZL42VK_ACCESS_2_COLOR_ATTACHMENT_WRITE_BIT_KHR");

   VK_ACCESS_2_DEPTH_STENCIL_ATTACHMENT_READ_BIT_KHR : aliased VkAccessFlags2KHR;  -- vulkan_core.h:7602
   pragma Import (CPP, VK_ACCESS_2_DEPTH_STENCIL_ATTACHMENT_READ_BIT_KHR, "_ZL49VK_ACCESS_2_DEPTH_STENCIL_ATTACHMENT_READ_BIT_KHR");

   VK_ACCESS_2_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT_KHR : aliased VkAccessFlags2KHR;  -- vulkan_core.h:7603
   pragma Import (CPP, VK_ACCESS_2_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT_KHR, "_ZL50VK_ACCESS_2_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT_KHR");

   VK_ACCESS_2_TRANSFER_READ_BIT_KHR : aliased VkAccessFlags2KHR;  -- vulkan_core.h:7604
   pragma Import (CPP, VK_ACCESS_2_TRANSFER_READ_BIT_KHR, "_ZL33VK_ACCESS_2_TRANSFER_READ_BIT_KHR");

   VK_ACCESS_2_TRANSFER_WRITE_BIT_KHR : aliased VkAccessFlags2KHR;  -- vulkan_core.h:7605
   pragma Import (CPP, VK_ACCESS_2_TRANSFER_WRITE_BIT_KHR, "_ZL34VK_ACCESS_2_TRANSFER_WRITE_BIT_KHR");

   VK_ACCESS_2_HOST_READ_BIT_KHR : aliased VkAccessFlags2KHR;  -- vulkan_core.h:7606
   pragma Import (CPP, VK_ACCESS_2_HOST_READ_BIT_KHR, "_ZL29VK_ACCESS_2_HOST_READ_BIT_KHR");

   VK_ACCESS_2_HOST_WRITE_BIT_KHR : aliased VkAccessFlags2KHR;  -- vulkan_core.h:7607
   pragma Import (CPP, VK_ACCESS_2_HOST_WRITE_BIT_KHR, "_ZL30VK_ACCESS_2_HOST_WRITE_BIT_KHR");

   VK_ACCESS_2_MEMORY_READ_BIT_KHR : aliased VkAccessFlags2KHR;  -- vulkan_core.h:7608
   pragma Import (CPP, VK_ACCESS_2_MEMORY_READ_BIT_KHR, "_ZL31VK_ACCESS_2_MEMORY_READ_BIT_KHR");

   VK_ACCESS_2_MEMORY_WRITE_BIT_KHR : aliased VkAccessFlags2KHR;  -- vulkan_core.h:7609
   pragma Import (CPP, VK_ACCESS_2_MEMORY_WRITE_BIT_KHR, "_ZL32VK_ACCESS_2_MEMORY_WRITE_BIT_KHR");

   VK_ACCESS_2_SHADER_SAMPLED_READ_BIT_KHR : aliased VkAccessFlags2KHR;  -- vulkan_core.h:7610
   pragma Import (CPP, VK_ACCESS_2_SHADER_SAMPLED_READ_BIT_KHR, "_ZL39VK_ACCESS_2_SHADER_SAMPLED_READ_BIT_KHR");

   VK_ACCESS_2_SHADER_STORAGE_READ_BIT_KHR : aliased VkAccessFlags2KHR;  -- vulkan_core.h:7611
   pragma Import (CPP, VK_ACCESS_2_SHADER_STORAGE_READ_BIT_KHR, "_ZL39VK_ACCESS_2_SHADER_STORAGE_READ_BIT_KHR");

   VK_ACCESS_2_SHADER_STORAGE_WRITE_BIT_KHR : aliased VkAccessFlags2KHR;  -- vulkan_core.h:7612
   pragma Import (CPP, VK_ACCESS_2_SHADER_STORAGE_WRITE_BIT_KHR, "_ZL40VK_ACCESS_2_SHADER_STORAGE_WRITE_BIT_KHR");

   VK_ACCESS_2_TRANSFORM_FEEDBACK_WRITE_BIT_EXT : aliased VkAccessFlags2KHR;  -- vulkan_core.h:7613
   pragma Import (CPP, VK_ACCESS_2_TRANSFORM_FEEDBACK_WRITE_BIT_EXT, "_ZL44VK_ACCESS_2_TRANSFORM_FEEDBACK_WRITE_BIT_EXT");

   VK_ACCESS_2_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT : aliased VkAccessFlags2KHR;  -- vulkan_core.h:7614
   pragma Import (CPP, VK_ACCESS_2_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT, "_ZL51VK_ACCESS_2_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT");

   VK_ACCESS_2_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT : aliased VkAccessFlags2KHR;  -- vulkan_core.h:7615
   pragma Import (CPP, VK_ACCESS_2_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT, "_ZL52VK_ACCESS_2_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT");

   VK_ACCESS_2_CONDITIONAL_RENDERING_READ_BIT_EXT : aliased VkAccessFlags2KHR;  -- vulkan_core.h:7616
   pragma Import (CPP, VK_ACCESS_2_CONDITIONAL_RENDERING_READ_BIT_EXT, "_ZL46VK_ACCESS_2_CONDITIONAL_RENDERING_READ_BIT_EXT");

   VK_ACCESS_2_COMMAND_PREPROCESS_READ_BIT_NV : aliased VkAccessFlags2KHR;  -- vulkan_core.h:7617
   pragma Import (CPP, VK_ACCESS_2_COMMAND_PREPROCESS_READ_BIT_NV, "_ZL42VK_ACCESS_2_COMMAND_PREPROCESS_READ_BIT_NV");

   VK_ACCESS_2_COMMAND_PREPROCESS_WRITE_BIT_NV : aliased VkAccessFlags2KHR;  -- vulkan_core.h:7618
   pragma Import (CPP, VK_ACCESS_2_COMMAND_PREPROCESS_WRITE_BIT_NV, "_ZL43VK_ACCESS_2_COMMAND_PREPROCESS_WRITE_BIT_NV");

   VK_ACCESS_2_FRAGMENT_SHADING_RATE_ATTACHMENT_READ_BIT_KHR : aliased VkAccessFlags2KHR;  -- vulkan_core.h:7619
   pragma Import (CPP, VK_ACCESS_2_FRAGMENT_SHADING_RATE_ATTACHMENT_READ_BIT_KHR, "_ZL57VK_ACCESS_2_FRAGMENT_SHADING_RATE_ATTACHMENT_READ_BIT_KHR");

   VK_ACCESS_2_SHADING_RATE_IMAGE_READ_BIT_NV : aliased VkAccessFlags2KHR;  -- vulkan_core.h:7620
   pragma Import (CPP, VK_ACCESS_2_SHADING_RATE_IMAGE_READ_BIT_NV, "_ZL42VK_ACCESS_2_SHADING_RATE_IMAGE_READ_BIT_NV");

   VK_ACCESS_2_ACCELERATION_STRUCTURE_READ_BIT_KHR : aliased VkAccessFlags2KHR;  -- vulkan_core.h:7621
   pragma Import (CPP, VK_ACCESS_2_ACCELERATION_STRUCTURE_READ_BIT_KHR, "_ZL47VK_ACCESS_2_ACCELERATION_STRUCTURE_READ_BIT_KHR");

   VK_ACCESS_2_ACCELERATION_STRUCTURE_WRITE_BIT_KHR : aliased VkAccessFlags2KHR;  -- vulkan_core.h:7622
   pragma Import (CPP, VK_ACCESS_2_ACCELERATION_STRUCTURE_WRITE_BIT_KHR, "_ZL48VK_ACCESS_2_ACCELERATION_STRUCTURE_WRITE_BIT_KHR");

   VK_ACCESS_2_ACCELERATION_STRUCTURE_READ_BIT_NV : aliased VkAccessFlags2KHR;  -- vulkan_core.h:7623
   pragma Import (CPP, VK_ACCESS_2_ACCELERATION_STRUCTURE_READ_BIT_NV, "_ZL46VK_ACCESS_2_ACCELERATION_STRUCTURE_READ_BIT_NV");

   VK_ACCESS_2_ACCELERATION_STRUCTURE_WRITE_BIT_NV : aliased VkAccessFlags2KHR;  -- vulkan_core.h:7624
   pragma Import (CPP, VK_ACCESS_2_ACCELERATION_STRUCTURE_WRITE_BIT_NV, "_ZL47VK_ACCESS_2_ACCELERATION_STRUCTURE_WRITE_BIT_NV");

   VK_ACCESS_2_FRAGMENT_DENSITY_MAP_READ_BIT_EXT : aliased VkAccessFlags2KHR;  -- vulkan_core.h:7625
   pragma Import (CPP, VK_ACCESS_2_FRAGMENT_DENSITY_MAP_READ_BIT_EXT, "_ZL45VK_ACCESS_2_FRAGMENT_DENSITY_MAP_READ_BIT_EXT");

   VK_ACCESS_2_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT : aliased VkAccessFlags2KHR;  -- vulkan_core.h:7626
   pragma Import (CPP, VK_ACCESS_2_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT, "_ZL53VK_ACCESS_2_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT");

   subtype VkSubmitFlagBitsKHR is unsigned;
   VK_SUBMIT_PROTECTED_BIT_KHR : constant VkSubmitFlagBitsKHR := 1;
   VK_SUBMIT_FLAG_BITS_MAX_ENUM_KHR : constant VkSubmitFlagBitsKHR := 2147483647;  -- vulkan_core.h:7629

   subtype VkSubmitFlagsKHR is VkFlags;  -- vulkan_core.h:7633

   type VkMemoryBarrier2KHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:7635
      pNext : System.Address;  -- vulkan_core.h:7636
      srcStageMask : aliased VkPipelineStageFlags2KHR;  -- vulkan_core.h:7637
      srcAccessMask : aliased VkAccessFlags2KHR;  -- vulkan_core.h:7638
      dstStageMask : aliased VkPipelineStageFlags2KHR;  -- vulkan_core.h:7639
      dstAccessMask : aliased VkAccessFlags2KHR;  -- vulkan_core.h:7640
   end record;
   pragma Convention (C_Pass_By_Copy, VkMemoryBarrier2KHR);  -- vulkan_core.h:7634

   type VkBufferMemoryBarrier2KHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:7644
      pNext : System.Address;  -- vulkan_core.h:7645
      srcStageMask : aliased VkPipelineStageFlags2KHR;  -- vulkan_core.h:7646
      srcAccessMask : aliased VkAccessFlags2KHR;  -- vulkan_core.h:7647
      dstStageMask : aliased VkPipelineStageFlags2KHR;  -- vulkan_core.h:7648
      dstAccessMask : aliased VkAccessFlags2KHR;  -- vulkan_core.h:7649
      srcQueueFamilyIndex : aliased stdint_h.uint32_t;  -- vulkan_core.h:7650
      dstQueueFamilyIndex : aliased stdint_h.uint32_t;  -- vulkan_core.h:7651
      buffer : VkBuffer;  -- vulkan_core.h:7652
      offset : aliased VkDeviceSize;  -- vulkan_core.h:7653
      size : aliased VkDeviceSize;  -- vulkan_core.h:7654
   end record;
   pragma Convention (C_Pass_By_Copy, VkBufferMemoryBarrier2KHR);  -- vulkan_core.h:7643

   type VkImageMemoryBarrier2KHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:7658
      pNext : System.Address;  -- vulkan_core.h:7659
      srcStageMask : aliased VkPipelineStageFlags2KHR;  -- vulkan_core.h:7660
      srcAccessMask : aliased VkAccessFlags2KHR;  -- vulkan_core.h:7661
      dstStageMask : aliased VkPipelineStageFlags2KHR;  -- vulkan_core.h:7662
      dstAccessMask : aliased VkAccessFlags2KHR;  -- vulkan_core.h:7663
      oldLayout : aliased VkImageLayout;  -- vulkan_core.h:7664
      newLayout : aliased VkImageLayout;  -- vulkan_core.h:7665
      srcQueueFamilyIndex : aliased stdint_h.uint32_t;  -- vulkan_core.h:7666
      dstQueueFamilyIndex : aliased stdint_h.uint32_t;  -- vulkan_core.h:7667
      image : VkImage;  -- vulkan_core.h:7668
      subresourceRange : aliased VkImageSubresourceRange;  -- vulkan_core.h:7669
   end record;
   pragma Convention (C_Pass_By_Copy, VkImageMemoryBarrier2KHR);  -- vulkan_core.h:7657

   type VkDependencyInfoKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:7673
      pNext : System.Address;  -- vulkan_core.h:7674
      dependencyFlags : aliased VkDependencyFlags;  -- vulkan_core.h:7675
      memoryBarrierCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:7676
      pMemoryBarriers : System.Address;  -- vulkan_core.h:7677
      bufferMemoryBarrierCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:7678
      pBufferMemoryBarriers : System.Address;  -- vulkan_core.h:7679
      imageMemoryBarrierCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:7680
      pImageMemoryBarriers : System.Address;  -- vulkan_core.h:7681
   end record;
   pragma Convention (C_Pass_By_Copy, VkDependencyInfoKHR);  -- vulkan_core.h:7672

   type VkSemaphoreSubmitInfoKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:7685
      pNext : System.Address;  -- vulkan_core.h:7686
      semaphore : VkSemaphore;  -- vulkan_core.h:7687
      value : aliased stdint_h.uint64_t;  -- vulkan_core.h:7688
      stageMask : aliased VkPipelineStageFlags2KHR;  -- vulkan_core.h:7689
      deviceIndex : aliased stdint_h.uint32_t;  -- vulkan_core.h:7690
   end record;
   pragma Convention (C_Pass_By_Copy, VkSemaphoreSubmitInfoKHR);  -- vulkan_core.h:7684

   type VkCommandBufferSubmitInfoKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:7694
      pNext : System.Address;  -- vulkan_core.h:7695
      commandBuffer : VkCommandBuffer;  -- vulkan_core.h:7696
      deviceMask : aliased stdint_h.uint32_t;  -- vulkan_core.h:7697
   end record;
   pragma Convention (C_Pass_By_Copy, VkCommandBufferSubmitInfoKHR);  -- vulkan_core.h:7693

   type VkSubmitInfo2KHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:7701
      pNext : System.Address;  -- vulkan_core.h:7702
      flags : aliased VkSubmitFlagsKHR;  -- vulkan_core.h:7703
      waitSemaphoreInfoCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:7704
      pWaitSemaphoreInfos : System.Address;  -- vulkan_core.h:7705
      commandBufferInfoCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:7706
      pCommandBufferInfos : System.Address;  -- vulkan_core.h:7707
      signalSemaphoreInfoCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:7708
      pSignalSemaphoreInfos : System.Address;  -- vulkan_core.h:7709
   end record;
   pragma Convention (C_Pass_By_Copy, VkSubmitInfo2KHR);  -- vulkan_core.h:7700

   type VkPhysicalDeviceSynchronization2FeaturesKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:7713
      pNext : System.Address;  -- vulkan_core.h:7714
      synchronization2 : aliased VkBool32;  -- vulkan_core.h:7715
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceSynchronization2FeaturesKHR);  -- vulkan_core.h:7712

   type VkQueueFamilyCheckpointProperties2NV is record
      sType : aliased VkStructureType;  -- vulkan_core.h:7719
      pNext : System.Address;  -- vulkan_core.h:7720
      checkpointExecutionStageMask : aliased VkPipelineStageFlags2KHR;  -- vulkan_core.h:7721
   end record;
   pragma Convention (C_Pass_By_Copy, VkQueueFamilyCheckpointProperties2NV);  -- vulkan_core.h:7718

   type VkCheckpointData2NV is record
      sType : aliased VkStructureType;  -- vulkan_core.h:7725
      pNext : System.Address;  -- vulkan_core.h:7726
      stage : aliased VkPipelineStageFlags2KHR;  -- vulkan_core.h:7727
      pCheckpointMarker : System.Address;  -- vulkan_core.h:7728
   end record;
   pragma Convention (C_Pass_By_Copy, VkCheckpointData2NV);  -- vulkan_core.h:7724

   type PFN_vkCmdSetEvent2KHR is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkEvent;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkCmdSetEvent2KHR);  -- vulkan_core.h:7731

   type PFN_vkCmdResetEvent2KHR is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkEvent;
         arg3 : VkPipelineStageFlags2KHR);
   pragma Convention (C, PFN_vkCmdResetEvent2KHR);  -- vulkan_core.h:7732

   type PFN_vkCmdWaitEvents2KHR is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : stdint_h.uint32_t;
         arg3 : System.Address;
         arg4 : System.Address);
   pragma Convention (C, PFN_vkCmdWaitEvents2KHR);  -- vulkan_core.h:7733

   type PFN_vkCmdPipelineBarrier2KHR is access procedure (arg1 : VkCommandBuffer; arg2 : System.Address);
   pragma Convention (C, PFN_vkCmdPipelineBarrier2KHR);  -- vulkan_core.h:7734

   type PFN_vkCmdWriteTimestamp2KHR is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkPipelineStageFlags2KHR;
         arg3 : VkQueryPool;
         arg4 : stdint_h.uint32_t);
   pragma Convention (C, PFN_vkCmdWriteTimestamp2KHR);  -- vulkan_core.h:7735

   type PFN_vkQueueSubmit2KHR is access function
        (arg1 : VkQueue;
         arg2 : stdint_h.uint32_t;
         arg3 : System.Address;
         arg4 : VkFence) return VkResult;
   pragma Convention (C, PFN_vkQueueSubmit2KHR);  -- vulkan_core.h:7736

   type PFN_vkCmdWriteBufferMarker2AMD is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkPipelineStageFlags2KHR;
         arg3 : VkBuffer;
         arg4 : VkDeviceSize;
         arg5 : stdint_h.uint32_t);
   pragma Convention (C, PFN_vkCmdWriteBufferMarker2AMD);  -- vulkan_core.h:7737

   type PFN_vkGetQueueCheckpointData2NV is access procedure
        (arg1 : VkQueue;
         arg2 : access stdint_h.uint32_t;
         arg3 : access VkCheckpointData2NV);
   pragma Convention (C, PFN_vkGetQueueCheckpointData2NV);  -- vulkan_core.h:7738

   procedure vkCmdSetEvent2KHR
     (commandBuffer : VkCommandBuffer;
      event : VkEvent;
      pDependencyInfo : System.Address);  -- vulkan_core.h:7741
   pragma Import (C, vkCmdSetEvent2KHR, "vkCmdSetEvent2KHR");

   procedure vkCmdResetEvent2KHR
     (commandBuffer : VkCommandBuffer;
      event : VkEvent;
      stageMask : VkPipelineStageFlags2KHR);  -- vulkan_core.h:7746
   pragma Import (C, vkCmdResetEvent2KHR, "vkCmdResetEvent2KHR");

   procedure vkCmdWaitEvents2KHR
     (commandBuffer : VkCommandBuffer;
      eventCount : stdint_h.uint32_t;
      pEvents : System.Address;
      pDependencyInfos : System.Address);  -- vulkan_core.h:7751
   pragma Import (C, vkCmdWaitEvents2KHR, "vkCmdWaitEvents2KHR");

   procedure vkCmdPipelineBarrier2KHR (commandBuffer : VkCommandBuffer; pDependencyInfo : System.Address);  -- vulkan_core.h:7757
   pragma Import (C, vkCmdPipelineBarrier2KHR, "vkCmdPipelineBarrier2KHR");

   procedure vkCmdWriteTimestamp2KHR
     (commandBuffer : VkCommandBuffer;
      stage : VkPipelineStageFlags2KHR;
      queryPool : VkQueryPool;
      query : stdint_h.uint32_t);  -- vulkan_core.h:7761
   pragma Import (C, vkCmdWriteTimestamp2KHR, "vkCmdWriteTimestamp2KHR");

   function vkQueueSubmit2KHR
     (queue : VkQueue;
      submitCount : stdint_h.uint32_t;
      pSubmits : System.Address;
      fence : VkFence) return VkResult;  -- vulkan_core.h:7767
   pragma Import (C, vkQueueSubmit2KHR, "vkQueueSubmit2KHR");

   procedure vkCmdWriteBufferMarker2AMD
     (commandBuffer : VkCommandBuffer;
      stage : VkPipelineStageFlags2KHR;
      dstBuffer : VkBuffer;
      dstOffset : VkDeviceSize;
      marker : stdint_h.uint32_t);  -- vulkan_core.h:7773
   pragma Import (C, vkCmdWriteBufferMarker2AMD, "vkCmdWriteBufferMarker2AMD");

   procedure vkGetQueueCheckpointData2NV
     (queue : VkQueue;
      pCheckpointDataCount : access stdint_h.uint32_t;
      pCheckpointData : access VkCheckpointData2NV);  -- vulkan_core.h:7780
   pragma Import (C, vkGetQueueCheckpointData2NV, "vkGetQueueCheckpointData2NV");

   type VkPhysicalDeviceZeroInitializeWorkgroupMemoryFeaturesKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:7791
      pNext : System.Address;  -- vulkan_core.h:7792
      shaderZeroInitializeWorkgroupMemory : aliased VkBool32;  -- vulkan_core.h:7793
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceZeroInitializeWorkgroupMemoryFeaturesKHR);  -- vulkan_core.h:7790

   type VkPhysicalDeviceWorkgroupMemoryExplicitLayoutFeaturesKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:7802
      pNext : System.Address;  -- vulkan_core.h:7803
      workgroupMemoryExplicitLayout : aliased VkBool32;  -- vulkan_core.h:7804
      workgroupMemoryExplicitLayoutScalarBlockLayout : aliased VkBool32;  -- vulkan_core.h:7805
      workgroupMemoryExplicitLayout8BitAccess : aliased VkBool32;  -- vulkan_core.h:7806
      workgroupMemoryExplicitLayout16BitAccess : aliased VkBool32;  -- vulkan_core.h:7807
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceWorkgroupMemoryExplicitLayoutFeaturesKHR);  -- vulkan_core.h:7801

   type VkBufferCopy2KHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:7816
      pNext : System.Address;  -- vulkan_core.h:7817
      srcOffset : aliased VkDeviceSize;  -- vulkan_core.h:7818
      dstOffset : aliased VkDeviceSize;  -- vulkan_core.h:7819
      size : aliased VkDeviceSize;  -- vulkan_core.h:7820
   end record;
   pragma Convention (C_Pass_By_Copy, VkBufferCopy2KHR);  -- vulkan_core.h:7815

   type VkCopyBufferInfo2KHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:7824
      pNext : System.Address;  -- vulkan_core.h:7825
      srcBuffer : VkBuffer;  -- vulkan_core.h:7826
      dstBuffer : VkBuffer;  -- vulkan_core.h:7827
      regionCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:7828
      pRegions : System.Address;  -- vulkan_core.h:7829
   end record;
   pragma Convention (C_Pass_By_Copy, VkCopyBufferInfo2KHR);  -- vulkan_core.h:7823

   type VkImageCopy2KHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:7833
      pNext : System.Address;  -- vulkan_core.h:7834
      srcSubresource : aliased VkImageSubresourceLayers;  -- vulkan_core.h:7835
      srcOffset : aliased VkOffset3D;  -- vulkan_core.h:7836
      dstSubresource : aliased VkImageSubresourceLayers;  -- vulkan_core.h:7837
      dstOffset : aliased VkOffset3D;  -- vulkan_core.h:7838
      extent : aliased VkExtent3D;  -- vulkan_core.h:7839
   end record;
   pragma Convention (C_Pass_By_Copy, VkImageCopy2KHR);  -- vulkan_core.h:7832

   type VkCopyImageInfo2KHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:7843
      pNext : System.Address;  -- vulkan_core.h:7844
      srcImage : VkImage;  -- vulkan_core.h:7845
      srcImageLayout : aliased VkImageLayout;  -- vulkan_core.h:7846
      dstImage : VkImage;  -- vulkan_core.h:7847
      dstImageLayout : aliased VkImageLayout;  -- vulkan_core.h:7848
      regionCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:7849
      pRegions : System.Address;  -- vulkan_core.h:7850
   end record;
   pragma Convention (C_Pass_By_Copy, VkCopyImageInfo2KHR);  -- vulkan_core.h:7842

   type VkBufferImageCopy2KHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:7854
      pNext : System.Address;  -- vulkan_core.h:7855
      bufferOffset : aliased VkDeviceSize;  -- vulkan_core.h:7856
      bufferRowLength : aliased stdint_h.uint32_t;  -- vulkan_core.h:7857
      bufferImageHeight : aliased stdint_h.uint32_t;  -- vulkan_core.h:7858
      imageSubresource : aliased VkImageSubresourceLayers;  -- vulkan_core.h:7859
      imageOffset : aliased VkOffset3D;  -- vulkan_core.h:7860
      imageExtent : aliased VkExtent3D;  -- vulkan_core.h:7861
   end record;
   pragma Convention (C_Pass_By_Copy, VkBufferImageCopy2KHR);  -- vulkan_core.h:7853

   type VkCopyBufferToImageInfo2KHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:7865
      pNext : System.Address;  -- vulkan_core.h:7866
      srcBuffer : VkBuffer;  -- vulkan_core.h:7867
      dstImage : VkImage;  -- vulkan_core.h:7868
      dstImageLayout : aliased VkImageLayout;  -- vulkan_core.h:7869
      regionCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:7870
      pRegions : System.Address;  -- vulkan_core.h:7871
   end record;
   pragma Convention (C_Pass_By_Copy, VkCopyBufferToImageInfo2KHR);  -- vulkan_core.h:7864

   type VkCopyImageToBufferInfo2KHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:7875
      pNext : System.Address;  -- vulkan_core.h:7876
      srcImage : VkImage;  -- vulkan_core.h:7877
      srcImageLayout : aliased VkImageLayout;  -- vulkan_core.h:7878
      dstBuffer : VkBuffer;  -- vulkan_core.h:7879
      regionCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:7880
      pRegions : System.Address;  -- vulkan_core.h:7881
   end record;
   pragma Convention (C_Pass_By_Copy, VkCopyImageToBufferInfo2KHR);  -- vulkan_core.h:7874

   type VkImageBlit2KHR_srcOffsets_array is array (0 .. 1) of aliased VkOffset3D;
   type VkImageBlit2KHR_dstOffsets_array is array (0 .. 1) of aliased VkOffset3D;
   type VkImageBlit2KHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:7885
      pNext : System.Address;  -- vulkan_core.h:7886
      srcSubresource : aliased VkImageSubresourceLayers;  -- vulkan_core.h:7887
      srcOffsets : aliased VkImageBlit2KHR_srcOffsets_array;  -- vulkan_core.h:7888
      dstSubresource : aliased VkImageSubresourceLayers;  -- vulkan_core.h:7889
      dstOffsets : aliased VkImageBlit2KHR_dstOffsets_array;  -- vulkan_core.h:7890
   end record;
   pragma Convention (C_Pass_By_Copy, VkImageBlit2KHR);  -- vulkan_core.h:7884

   type VkBlitImageInfo2KHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:7894
      pNext : System.Address;  -- vulkan_core.h:7895
      srcImage : VkImage;  -- vulkan_core.h:7896
      srcImageLayout : aliased VkImageLayout;  -- vulkan_core.h:7897
      dstImage : VkImage;  -- vulkan_core.h:7898
      dstImageLayout : aliased VkImageLayout;  -- vulkan_core.h:7899
      regionCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:7900
      pRegions : System.Address;  -- vulkan_core.h:7901
      filter : aliased VkFilter;  -- vulkan_core.h:7902
   end record;
   pragma Convention (C_Pass_By_Copy, VkBlitImageInfo2KHR);  -- vulkan_core.h:7893

   type VkImageResolve2KHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:7906
      pNext : System.Address;  -- vulkan_core.h:7907
      srcSubresource : aliased VkImageSubresourceLayers;  -- vulkan_core.h:7908
      srcOffset : aliased VkOffset3D;  -- vulkan_core.h:7909
      dstSubresource : aliased VkImageSubresourceLayers;  -- vulkan_core.h:7910
      dstOffset : aliased VkOffset3D;  -- vulkan_core.h:7911
      extent : aliased VkExtent3D;  -- vulkan_core.h:7912
   end record;
   pragma Convention (C_Pass_By_Copy, VkImageResolve2KHR);  -- vulkan_core.h:7905

   type VkResolveImageInfo2KHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:7916
      pNext : System.Address;  -- vulkan_core.h:7917
      srcImage : VkImage;  -- vulkan_core.h:7918
      srcImageLayout : aliased VkImageLayout;  -- vulkan_core.h:7919
      dstImage : VkImage;  -- vulkan_core.h:7920
      dstImageLayout : aliased VkImageLayout;  -- vulkan_core.h:7921
      regionCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:7922
      pRegions : System.Address;  -- vulkan_core.h:7923
   end record;
   pragma Convention (C_Pass_By_Copy, VkResolveImageInfo2KHR);  -- vulkan_core.h:7915

   type PFN_vkCmdCopyBuffer2KHR is access procedure (arg1 : VkCommandBuffer; arg2 : System.Address);
   pragma Convention (C, PFN_vkCmdCopyBuffer2KHR);  -- vulkan_core.h:7926

   type PFN_vkCmdCopyImage2KHR is access procedure (arg1 : VkCommandBuffer; arg2 : System.Address);
   pragma Convention (C, PFN_vkCmdCopyImage2KHR);  -- vulkan_core.h:7927

   type PFN_vkCmdCopyBufferToImage2KHR is access procedure (arg1 : VkCommandBuffer; arg2 : System.Address);
   pragma Convention (C, PFN_vkCmdCopyBufferToImage2KHR);  -- vulkan_core.h:7928

   type PFN_vkCmdCopyImageToBuffer2KHR is access procedure (arg1 : VkCommandBuffer; arg2 : System.Address);
   pragma Convention (C, PFN_vkCmdCopyImageToBuffer2KHR);  -- vulkan_core.h:7929

   type PFN_vkCmdBlitImage2KHR is access procedure (arg1 : VkCommandBuffer; arg2 : System.Address);
   pragma Convention (C, PFN_vkCmdBlitImage2KHR);  -- vulkan_core.h:7930

   type PFN_vkCmdResolveImage2KHR is access procedure (arg1 : VkCommandBuffer; arg2 : System.Address);
   pragma Convention (C, PFN_vkCmdResolveImage2KHR);  -- vulkan_core.h:7931

   procedure vkCmdCopyBuffer2KHR (commandBuffer : VkCommandBuffer; pCopyBufferInfo : System.Address);  -- vulkan_core.h:7934
   pragma Import (C, vkCmdCopyBuffer2KHR, "vkCmdCopyBuffer2KHR");

   procedure vkCmdCopyImage2KHR (commandBuffer : VkCommandBuffer; pCopyImageInfo : System.Address);  -- vulkan_core.h:7938
   pragma Import (C, vkCmdCopyImage2KHR, "vkCmdCopyImage2KHR");

   procedure vkCmdCopyBufferToImage2KHR (commandBuffer : VkCommandBuffer; pCopyBufferToImageInfo : System.Address);  -- vulkan_core.h:7942
   pragma Import (C, vkCmdCopyBufferToImage2KHR, "vkCmdCopyBufferToImage2KHR");

   procedure vkCmdCopyImageToBuffer2KHR (commandBuffer : VkCommandBuffer; pCopyImageToBufferInfo : System.Address);  -- vulkan_core.h:7946
   pragma Import (C, vkCmdCopyImageToBuffer2KHR, "vkCmdCopyImageToBuffer2KHR");

   procedure vkCmdBlitImage2KHR (commandBuffer : VkCommandBuffer; pBlitImageInfo : System.Address);  -- vulkan_core.h:7950
   pragma Import (C, vkCmdBlitImage2KHR, "vkCmdBlitImage2KHR");

   procedure vkCmdResolveImage2KHR (commandBuffer : VkCommandBuffer; pResolveImageInfo : System.Address);  -- vulkan_core.h:7954
   pragma Import (C, vkCmdResolveImage2KHR, "vkCmdResolveImage2KHR");

   type VkDebugReportCallbackEXT is new System.Address;  -- vulkan_core.h:7961

   --  skipped empty struct VkDebugReportCallbackEXT_T

   subtype VkDebugReportObjectTypeEXT is unsigned;
   VK_DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT : constant VkDebugReportObjectTypeEXT := 0;
   VK_DEBUG_REPORT_OBJECT_TYPE_INSTANCE_EXT : constant VkDebugReportObjectTypeEXT := 1;
   VK_DEBUG_REPORT_OBJECT_TYPE_PHYSICAL_DEVICE_EXT : constant VkDebugReportObjectTypeEXT := 2;
   VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_EXT : constant VkDebugReportObjectTypeEXT := 3;
   VK_DEBUG_REPORT_OBJECT_TYPE_QUEUE_EXT : constant VkDebugReportObjectTypeEXT := 4;
   VK_DEBUG_REPORT_OBJECT_TYPE_SEMAPHORE_EXT : constant VkDebugReportObjectTypeEXT := 5;
   VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_BUFFER_EXT : constant VkDebugReportObjectTypeEXT := 6;
   VK_DEBUG_REPORT_OBJECT_TYPE_FENCE_EXT : constant VkDebugReportObjectTypeEXT := 7;
   VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_MEMORY_EXT : constant VkDebugReportObjectTypeEXT := 8;
   VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_EXT : constant VkDebugReportObjectTypeEXT := 9;
   VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_EXT : constant VkDebugReportObjectTypeEXT := 10;
   VK_DEBUG_REPORT_OBJECT_TYPE_EVENT_EXT : constant VkDebugReportObjectTypeEXT := 11;
   VK_DEBUG_REPORT_OBJECT_TYPE_QUERY_POOL_EXT : constant VkDebugReportObjectTypeEXT := 12;
   VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_VIEW_EXT : constant VkDebugReportObjectTypeEXT := 13;
   VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_VIEW_EXT : constant VkDebugReportObjectTypeEXT := 14;
   VK_DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT : constant VkDebugReportObjectTypeEXT := 15;
   VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_CACHE_EXT : constant VkDebugReportObjectTypeEXT := 16;
   VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_LAYOUT_EXT : constant VkDebugReportObjectTypeEXT := 17;
   VK_DEBUG_REPORT_OBJECT_TYPE_RENDER_PASS_EXT : constant VkDebugReportObjectTypeEXT := 18;
   VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_EXT : constant VkDebugReportObjectTypeEXT := 19;
   VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT_EXT : constant VkDebugReportObjectTypeEXT := 20;
   VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_EXT : constant VkDebugReportObjectTypeEXT := 21;
   VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_POOL_EXT : constant VkDebugReportObjectTypeEXT := 22;
   VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_EXT : constant VkDebugReportObjectTypeEXT := 23;
   VK_DEBUG_REPORT_OBJECT_TYPE_FRAMEBUFFER_EXT : constant VkDebugReportObjectTypeEXT := 24;
   VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_POOL_EXT : constant VkDebugReportObjectTypeEXT := 25;
   VK_DEBUG_REPORT_OBJECT_TYPE_SURFACE_KHR_EXT : constant VkDebugReportObjectTypeEXT := 26;
   VK_DEBUG_REPORT_OBJECT_TYPE_SWAPCHAIN_KHR_EXT : constant VkDebugReportObjectTypeEXT := 27;
   VK_DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT_EXT : constant VkDebugReportObjectTypeEXT := 28;
   VK_DEBUG_REPORT_OBJECT_TYPE_DISPLAY_KHR_EXT : constant VkDebugReportObjectTypeEXT := 29;
   VK_DEBUG_REPORT_OBJECT_TYPE_DISPLAY_MODE_KHR_EXT : constant VkDebugReportObjectTypeEXT := 30;
   VK_DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT_EXT : constant VkDebugReportObjectTypeEXT := 33;
   VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION_EXT : constant VkDebugReportObjectTypeEXT := 1000156000;
   VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_EXT : constant VkDebugReportObjectTypeEXT := 1000085000;
   VK_DEBUG_REPORT_OBJECT_TYPE_ACCELERATION_STRUCTURE_KHR_EXT : constant VkDebugReportObjectTypeEXT := 1000150000;
   VK_DEBUG_REPORT_OBJECT_TYPE_ACCELERATION_STRUCTURE_NV_EXT : constant VkDebugReportObjectTypeEXT := 1000165000;
   VK_DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_EXT : constant VkDebugReportObjectTypeEXT := 28;
   VK_DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT : constant VkDebugReportObjectTypeEXT := 33;
   VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR_EXT : constant VkDebugReportObjectTypeEXT := 1000085000;
   VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION_KHR_EXT : constant VkDebugReportObjectTypeEXT := 1000156000;
   VK_DEBUG_REPORT_OBJECT_TYPE_MAX_ENUM_EXT : constant VkDebugReportObjectTypeEXT := 2147483647;  -- vulkan_core.h:7965

   subtype VkDebugReportFlagBitsEXT is unsigned;
   VK_DEBUG_REPORT_INFORMATION_BIT_EXT : constant VkDebugReportFlagBitsEXT := 1;
   VK_DEBUG_REPORT_WARNING_BIT_EXT : constant VkDebugReportFlagBitsEXT := 2;
   VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT : constant VkDebugReportFlagBitsEXT := 4;
   VK_DEBUG_REPORT_ERROR_BIT_EXT : constant VkDebugReportFlagBitsEXT := 8;
   VK_DEBUG_REPORT_DEBUG_BIT_EXT : constant VkDebugReportFlagBitsEXT := 16;
   VK_DEBUG_REPORT_FLAG_BITS_MAX_ENUM_EXT : constant VkDebugReportFlagBitsEXT := 2147483647;  -- vulkan_core.h:8009

   subtype VkDebugReportFlagsEXT is VkFlags;  -- vulkan_core.h:8017

   type PFN_vkDebugReportCallbackEXT is access function
        (arg1 : VkDebugReportFlagsEXT;
         arg2 : VkDebugReportObjectTypeEXT;
         arg3 : stdint_h.uint64_t;
         arg4 : crtdefs_h.size_t;
         arg5 : stdint_h.int32_t;
         arg6 : Interfaces.C.Strings.chars_ptr;
         arg7 : Interfaces.C.Strings.chars_ptr;
         arg8 : System.Address) return VkBool32;
   pragma Convention (C, PFN_vkDebugReportCallbackEXT);  -- vulkan_core.h:8018

   type VkDebugReportCallbackCreateInfoEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:8029
      pNext : System.Address;  -- vulkan_core.h:8030
      flags : aliased VkDebugReportFlagsEXT;  -- vulkan_core.h:8031
      pfnCallback : PFN_vkDebugReportCallbackEXT;  -- vulkan_core.h:8032
      pUserData : System.Address;  -- vulkan_core.h:8033
   end record;
   pragma Convention (C_Pass_By_Copy, VkDebugReportCallbackCreateInfoEXT);  -- vulkan_core.h:8028

   type PFN_vkCreateDebugReportCallbackEXT is access function
        (arg1 : VkInstance;
         arg2 : System.Address;
         arg3 : System.Address;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateDebugReportCallbackEXT);  -- vulkan_core.h:8036

   type PFN_vkDestroyDebugReportCallbackEXT is access procedure
        (arg1 : VkInstance;
         arg2 : VkDebugReportCallbackEXT;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkDestroyDebugReportCallbackEXT);  -- vulkan_core.h:8037

   type PFN_vkDebugReportMessageEXT is access procedure
        (arg1 : VkInstance;
         arg2 : VkDebugReportFlagsEXT;
         arg3 : VkDebugReportObjectTypeEXT;
         arg4 : stdint_h.uint64_t;
         arg5 : crtdefs_h.size_t;
         arg6 : stdint_h.int32_t;
         arg7 : Interfaces.C.Strings.chars_ptr;
         arg8 : Interfaces.C.Strings.chars_ptr);
   pragma Convention (C, PFN_vkDebugReportMessageEXT);  -- vulkan_core.h:8038

   function vkCreateDebugReportCallbackEXT
     (instance : VkInstance;
      pCreateInfo : System.Address;
      pAllocator : System.Address;
      pCallback : System.Address) return VkResult;  -- vulkan_core.h:8041
   pragma Import (C, vkCreateDebugReportCallbackEXT, "vkCreateDebugReportCallbackEXT");

   procedure vkDestroyDebugReportCallbackEXT
     (instance : VkInstance;
      callback : VkDebugReportCallbackEXT;
      pAllocator : System.Address);  -- vulkan_core.h:8047
   pragma Import (C, vkDestroyDebugReportCallbackEXT, "vkDestroyDebugReportCallbackEXT");

   procedure vkDebugReportMessageEXT
     (instance : VkInstance;
      flags : VkDebugReportFlagsEXT;
      objectType : VkDebugReportObjectTypeEXT;
      object : stdint_h.uint64_t;
      location : crtdefs_h.size_t;
      messageCode : stdint_h.int32_t;
      pLayerPrefix : Interfaces.C.Strings.chars_ptr;
      pMessage : Interfaces.C.Strings.chars_ptr);  -- vulkan_core.h:8052
   pragma Import (C, vkDebugReportMessageEXT, "vkDebugReportMessageEXT");

   subtype VkRasterizationOrderAMD is unsigned;
   VK_RASTERIZATION_ORDER_STRICT_AMD : constant VkRasterizationOrderAMD := 0;
   VK_RASTERIZATION_ORDER_RELAXED_AMD : constant VkRasterizationOrderAMD := 1;
   VK_RASTERIZATION_ORDER_MAX_ENUM_AMD : constant VkRasterizationOrderAMD := 2147483647;  -- vulkan_core.h:8083

   type VkPipelineRasterizationStateRasterizationOrderAMD is record
      sType : aliased VkStructureType;  -- vulkan_core.h:8089
      pNext : System.Address;  -- vulkan_core.h:8090
      rasterizationOrder : aliased VkRasterizationOrderAMD;  -- vulkan_core.h:8091
   end record;
   pragma Convention (C_Pass_By_Copy, VkPipelineRasterizationStateRasterizationOrderAMD);  -- vulkan_core.h:8088

   type VkDebugMarkerObjectNameInfoEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:8110
      pNext : System.Address;  -- vulkan_core.h:8111
      objectType : aliased VkDebugReportObjectTypeEXT;  -- vulkan_core.h:8112
      object : aliased stdint_h.uint64_t;  -- vulkan_core.h:8113
      pObjectName : Interfaces.C.Strings.chars_ptr;  -- vulkan_core.h:8114
   end record;
   pragma Convention (C_Pass_By_Copy, VkDebugMarkerObjectNameInfoEXT);  -- vulkan_core.h:8109

   type VkDebugMarkerObjectTagInfoEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:8118
      pNext : System.Address;  -- vulkan_core.h:8119
      objectType : aliased VkDebugReportObjectTypeEXT;  -- vulkan_core.h:8120
      object : aliased stdint_h.uint64_t;  -- vulkan_core.h:8121
      tagName : aliased stdint_h.uint64_t;  -- vulkan_core.h:8122
      tagSize : aliased crtdefs_h.size_t;  -- vulkan_core.h:8123
      pTag : System.Address;  -- vulkan_core.h:8124
   end record;
   pragma Convention (C_Pass_By_Copy, VkDebugMarkerObjectTagInfoEXT);  -- vulkan_core.h:8117

   type VkDebugMarkerMarkerInfoEXT_color_array is array (0 .. 3) of aliased float;
   type VkDebugMarkerMarkerInfoEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:8128
      pNext : System.Address;  -- vulkan_core.h:8129
      pMarkerName : Interfaces.C.Strings.chars_ptr;  -- vulkan_core.h:8130
      color : aliased VkDebugMarkerMarkerInfoEXT_color_array;  -- vulkan_core.h:8131
   end record;
   pragma Convention (C_Pass_By_Copy, VkDebugMarkerMarkerInfoEXT);  -- vulkan_core.h:8127

   type PFN_vkDebugMarkerSetObjectTagEXT is access function (arg1 : VkDevice; arg2 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkDebugMarkerSetObjectTagEXT);  -- vulkan_core.h:8134

   type PFN_vkDebugMarkerSetObjectNameEXT is access function (arg1 : VkDevice; arg2 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkDebugMarkerSetObjectNameEXT);  -- vulkan_core.h:8135

   type PFN_vkCmdDebugMarkerBeginEXT is access procedure (arg1 : VkCommandBuffer; arg2 : System.Address);
   pragma Convention (C, PFN_vkCmdDebugMarkerBeginEXT);  -- vulkan_core.h:8136

   type PFN_vkCmdDebugMarkerEndEXT is access procedure (arg1 : VkCommandBuffer);
   pragma Convention (C, PFN_vkCmdDebugMarkerEndEXT);  -- vulkan_core.h:8137

   type PFN_vkCmdDebugMarkerInsertEXT is access procedure (arg1 : VkCommandBuffer; arg2 : System.Address);
   pragma Convention (C, PFN_vkCmdDebugMarkerInsertEXT);  -- vulkan_core.h:8138

   function vkDebugMarkerSetObjectTagEXT (device : VkDevice; pTagInfo : System.Address) return VkResult;  -- vulkan_core.h:8141
   pragma Import (C, vkDebugMarkerSetObjectTagEXT, "vkDebugMarkerSetObjectTagEXT");

   function vkDebugMarkerSetObjectNameEXT (device : VkDevice; pNameInfo : System.Address) return VkResult;  -- vulkan_core.h:8145
   pragma Import (C, vkDebugMarkerSetObjectNameEXT, "vkDebugMarkerSetObjectNameEXT");

   procedure vkCmdDebugMarkerBeginEXT (commandBuffer : VkCommandBuffer; pMarkerInfo : System.Address);  -- vulkan_core.h:8149
   pragma Import (C, vkCmdDebugMarkerBeginEXT, "vkCmdDebugMarkerBeginEXT");

   procedure vkCmdDebugMarkerEndEXT (commandBuffer : VkCommandBuffer);  -- vulkan_core.h:8153
   pragma Import (C, vkCmdDebugMarkerEndEXT, "vkCmdDebugMarkerEndEXT");

   procedure vkCmdDebugMarkerInsertEXT (commandBuffer : VkCommandBuffer; pMarkerInfo : System.Address);  -- vulkan_core.h:8156
   pragma Import (C, vkCmdDebugMarkerInsertEXT, "vkCmdDebugMarkerInsertEXT");

   type VkDedicatedAllocationImageCreateInfoNV is record
      sType : aliased VkStructureType;  -- vulkan_core.h:8171
      pNext : System.Address;  -- vulkan_core.h:8172
      dedicatedAllocation : aliased VkBool32;  -- vulkan_core.h:8173
   end record;
   pragma Convention (C_Pass_By_Copy, VkDedicatedAllocationImageCreateInfoNV);  -- vulkan_core.h:8170

   type VkDedicatedAllocationBufferCreateInfoNV is record
      sType : aliased VkStructureType;  -- vulkan_core.h:8177
      pNext : System.Address;  -- vulkan_core.h:8178
      dedicatedAllocation : aliased VkBool32;  -- vulkan_core.h:8179
   end record;
   pragma Convention (C_Pass_By_Copy, VkDedicatedAllocationBufferCreateInfoNV);  -- vulkan_core.h:8176

   type VkDedicatedAllocationMemoryAllocateInfoNV is record
      sType : aliased VkStructureType;  -- vulkan_core.h:8183
      pNext : System.Address;  -- vulkan_core.h:8184
      image : VkImage;  -- vulkan_core.h:8185
      buffer : VkBuffer;  -- vulkan_core.h:8186
   end record;
   pragma Convention (C_Pass_By_Copy, VkDedicatedAllocationMemoryAllocateInfoNV);  -- vulkan_core.h:8182

   subtype VkPipelineRasterizationStateStreamCreateFlagsEXT is VkFlags;  -- vulkan_core.h:8194

   type VkPhysicalDeviceTransformFeedbackFeaturesEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:8196
      pNext : System.Address;  -- vulkan_core.h:8197
      transformFeedback : aliased VkBool32;  -- vulkan_core.h:8198
      geometryStreams : aliased VkBool32;  -- vulkan_core.h:8199
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceTransformFeedbackFeaturesEXT);  -- vulkan_core.h:8195

   type VkPhysicalDeviceTransformFeedbackPropertiesEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:8203
      pNext : System.Address;  -- vulkan_core.h:8204
      maxTransformFeedbackStreams : aliased stdint_h.uint32_t;  -- vulkan_core.h:8205
      maxTransformFeedbackBuffers : aliased stdint_h.uint32_t;  -- vulkan_core.h:8206
      maxTransformFeedbackBufferSize : aliased VkDeviceSize;  -- vulkan_core.h:8207
      maxTransformFeedbackStreamDataSize : aliased stdint_h.uint32_t;  -- vulkan_core.h:8208
      maxTransformFeedbackBufferDataSize : aliased stdint_h.uint32_t;  -- vulkan_core.h:8209
      maxTransformFeedbackBufferDataStride : aliased stdint_h.uint32_t;  -- vulkan_core.h:8210
      transformFeedbackQueries : aliased VkBool32;  -- vulkan_core.h:8211
      transformFeedbackStreamsLinesTriangles : aliased VkBool32;  -- vulkan_core.h:8212
      transformFeedbackRasterizationStreamSelect : aliased VkBool32;  -- vulkan_core.h:8213
      transformFeedbackDraw : aliased VkBool32;  -- vulkan_core.h:8214
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceTransformFeedbackPropertiesEXT);  -- vulkan_core.h:8202

   type VkPipelineRasterizationStateStreamCreateInfoEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:8218
      pNext : System.Address;  -- vulkan_core.h:8219
      flags : aliased VkPipelineRasterizationStateStreamCreateFlagsEXT;  -- vulkan_core.h:8220
      rasterizationStream : aliased stdint_h.uint32_t;  -- vulkan_core.h:8221
   end record;
   pragma Convention (C_Pass_By_Copy, VkPipelineRasterizationStateStreamCreateInfoEXT);  -- vulkan_core.h:8217

   type PFN_vkCmdBindTransformFeedbackBuffersEXT is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : stdint_h.uint32_t;
         arg3 : stdint_h.uint32_t;
         arg4 : System.Address;
         arg5 : access VkDeviceSize;
         arg6 : access VkDeviceSize);
   pragma Convention (C, PFN_vkCmdBindTransformFeedbackBuffersEXT);  -- vulkan_core.h:8224

   type PFN_vkCmdBeginTransformFeedbackEXT is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : stdint_h.uint32_t;
         arg3 : stdint_h.uint32_t;
         arg4 : System.Address;
         arg5 : access VkDeviceSize);
   pragma Convention (C, PFN_vkCmdBeginTransformFeedbackEXT);  -- vulkan_core.h:8225

   type PFN_vkCmdEndTransformFeedbackEXT is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : stdint_h.uint32_t;
         arg3 : stdint_h.uint32_t;
         arg4 : System.Address;
         arg5 : access VkDeviceSize);
   pragma Convention (C, PFN_vkCmdEndTransformFeedbackEXT);  -- vulkan_core.h:8226

   type PFN_vkCmdBeginQueryIndexedEXT is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkQueryPool;
         arg3 : stdint_h.uint32_t;
         arg4 : VkQueryControlFlags;
         arg5 : stdint_h.uint32_t);
   pragma Convention (C, PFN_vkCmdBeginQueryIndexedEXT);  -- vulkan_core.h:8227

   type PFN_vkCmdEndQueryIndexedEXT is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkQueryPool;
         arg3 : stdint_h.uint32_t;
         arg4 : stdint_h.uint32_t);
   pragma Convention (C, PFN_vkCmdEndQueryIndexedEXT);  -- vulkan_core.h:8228

   type PFN_vkCmdDrawIndirectByteCountEXT is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : stdint_h.uint32_t;
         arg3 : stdint_h.uint32_t;
         arg4 : VkBuffer;
         arg5 : VkDeviceSize;
         arg6 : stdint_h.uint32_t;
         arg7 : stdint_h.uint32_t);
   pragma Convention (C, PFN_vkCmdDrawIndirectByteCountEXT);  -- vulkan_core.h:8229

   procedure vkCmdBindTransformFeedbackBuffersEXT
     (commandBuffer : VkCommandBuffer;
      firstBinding : stdint_h.uint32_t;
      bindingCount : stdint_h.uint32_t;
      pBuffers : System.Address;
      pOffsets : access VkDeviceSize;
      pSizes : access VkDeviceSize);  -- vulkan_core.h:8232
   pragma Import (C, vkCmdBindTransformFeedbackBuffersEXT, "vkCmdBindTransformFeedbackBuffersEXT");

   procedure vkCmdBeginTransformFeedbackEXT
     (commandBuffer : VkCommandBuffer;
      firstCounterBuffer : stdint_h.uint32_t;
      counterBufferCount : stdint_h.uint32_t;
      pCounterBuffers : System.Address;
      pCounterBufferOffsets : access VkDeviceSize);  -- vulkan_core.h:8240
   pragma Import (C, vkCmdBeginTransformFeedbackEXT, "vkCmdBeginTransformFeedbackEXT");

   procedure vkCmdEndTransformFeedbackEXT
     (commandBuffer : VkCommandBuffer;
      firstCounterBuffer : stdint_h.uint32_t;
      counterBufferCount : stdint_h.uint32_t;
      pCounterBuffers : System.Address;
      pCounterBufferOffsets : access VkDeviceSize);  -- vulkan_core.h:8247
   pragma Import (C, vkCmdEndTransformFeedbackEXT, "vkCmdEndTransformFeedbackEXT");

   procedure vkCmdBeginQueryIndexedEXT
     (commandBuffer : VkCommandBuffer;
      queryPool : VkQueryPool;
      query : stdint_h.uint32_t;
      flags : VkQueryControlFlags;
      index : stdint_h.uint32_t);  -- vulkan_core.h:8254
   pragma Import (C, vkCmdBeginQueryIndexedEXT, "vkCmdBeginQueryIndexedEXT");

   procedure vkCmdEndQueryIndexedEXT
     (commandBuffer : VkCommandBuffer;
      queryPool : VkQueryPool;
      query : stdint_h.uint32_t;
      index : stdint_h.uint32_t);  -- vulkan_core.h:8261
   pragma Import (C, vkCmdEndQueryIndexedEXT, "vkCmdEndQueryIndexedEXT");

   procedure vkCmdDrawIndirectByteCountEXT
     (commandBuffer : VkCommandBuffer;
      instanceCount : stdint_h.uint32_t;
      firstInstance : stdint_h.uint32_t;
      counterBuffer : VkBuffer;
      counterBufferOffset : VkDeviceSize;
      counterOffset : stdint_h.uint32_t;
      vertexStride : stdint_h.uint32_t);  -- vulkan_core.h:8267
   pragma Import (C, vkCmdDrawIndirectByteCountEXT, "vkCmdDrawIndirectByteCountEXT");

   type VkImageViewHandleInfoNVX is record
      sType : aliased VkStructureType;  -- vulkan_core.h:8282
      pNext : System.Address;  -- vulkan_core.h:8283
      imageView : VkImageView;  -- vulkan_core.h:8284
      descriptorType : aliased VkDescriptorType;  -- vulkan_core.h:8285
      sampler : VkSampler;  -- vulkan_core.h:8286
   end record;
   pragma Convention (C_Pass_By_Copy, VkImageViewHandleInfoNVX);  -- vulkan_core.h:8281

   type VkImageViewAddressPropertiesNVX is record
      sType : aliased VkStructureType;  -- vulkan_core.h:8290
      pNext : System.Address;  -- vulkan_core.h:8291
      deviceAddress : aliased VkDeviceAddress;  -- vulkan_core.h:8292
      size : aliased VkDeviceSize;  -- vulkan_core.h:8293
   end record;
   pragma Convention (C_Pass_By_Copy, VkImageViewAddressPropertiesNVX);  -- vulkan_core.h:8289

   type PFN_vkGetImageViewHandleNVX is access function (arg1 : VkDevice; arg2 : System.Address) return stdint_h.uint32_t;
   pragma Convention (C, PFN_vkGetImageViewHandleNVX);  -- vulkan_core.h:8296

   type PFN_vkGetImageViewAddressNVX is access function
        (arg1 : VkDevice;
         arg2 : VkImageView;
         arg3 : access VkImageViewAddressPropertiesNVX) return VkResult;
   pragma Convention (C, PFN_vkGetImageViewAddressNVX);  -- vulkan_core.h:8297

   function vkGetImageViewHandleNVX (device : VkDevice; pInfo : System.Address) return stdint_h.uint32_t;  -- vulkan_core.h:8300
   pragma Import (C, vkGetImageViewHandleNVX, "vkGetImageViewHandleNVX");

   function vkGetImageViewAddressNVX
     (device : VkDevice;
      imageView : VkImageView;
      pProperties : access VkImageViewAddressPropertiesNVX) return VkResult;  -- vulkan_core.h:8304
   pragma Import (C, vkGetImageViewAddressNVX, "vkGetImageViewAddressNVX");

   type PFN_vkCmdDrawIndirectCountAMD is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkBuffer;
         arg3 : VkDeviceSize;
         arg4 : VkBuffer;
         arg5 : VkDeviceSize;
         arg6 : stdint_h.uint32_t;
         arg7 : stdint_h.uint32_t);
   pragma Convention (C, PFN_vkCmdDrawIndirectCountAMD);  -- vulkan_core.h:8314

   type PFN_vkCmdDrawIndexedIndirectCountAMD is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkBuffer;
         arg3 : VkDeviceSize;
         arg4 : VkBuffer;
         arg5 : VkDeviceSize;
         arg6 : stdint_h.uint32_t;
         arg7 : stdint_h.uint32_t);
   pragma Convention (C, PFN_vkCmdDrawIndexedIndirectCountAMD);  -- vulkan_core.h:8315

   procedure vkCmdDrawIndirectCountAMD
     (commandBuffer : VkCommandBuffer;
      buffer : VkBuffer;
      offset : VkDeviceSize;
      countBuffer : VkBuffer;
      countBufferOffset : VkDeviceSize;
      maxDrawCount : stdint_h.uint32_t;
      stride : stdint_h.uint32_t);  -- vulkan_core.h:8318
   pragma Import (C, vkCmdDrawIndirectCountAMD, "vkCmdDrawIndirectCountAMD");

   procedure vkCmdDrawIndexedIndirectCountAMD
     (commandBuffer : VkCommandBuffer;
      buffer : VkBuffer;
      offset : VkDeviceSize;
      countBuffer : VkBuffer;
      countBufferOffset : VkDeviceSize;
      maxDrawCount : stdint_h.uint32_t;
      stride : stdint_h.uint32_t);  -- vulkan_core.h:8327
   pragma Import (C, vkCmdDrawIndexedIndirectCountAMD, "vkCmdDrawIndexedIndirectCountAMD");

   type VkTextureLODGatherFormatPropertiesAMD is record
      sType : aliased VkStructureType;  -- vulkan_core.h:8357
      pNext : System.Address;  -- vulkan_core.h:8358
      supportsTextureGatherLODBiasAMD : aliased VkBool32;  -- vulkan_core.h:8359
   end record;
   pragma Convention (C_Pass_By_Copy, VkTextureLODGatherFormatPropertiesAMD);  -- vulkan_core.h:8356

   subtype VkShaderInfoTypeAMD is unsigned;
   VK_SHADER_INFO_TYPE_STATISTICS_AMD : constant VkShaderInfoTypeAMD := 0;
   VK_SHADER_INFO_TYPE_BINARY_AMD : constant VkShaderInfoTypeAMD := 1;
   VK_SHADER_INFO_TYPE_DISASSEMBLY_AMD : constant VkShaderInfoTypeAMD := 2;
   VK_SHADER_INFO_TYPE_MAX_ENUM_AMD : constant VkShaderInfoTypeAMD := 2147483647;  -- vulkan_core.h:8368

   type VkShaderResourceUsageAMD is record
      numUsedVgprs : aliased stdint_h.uint32_t;  -- vulkan_core.h:8375
      numUsedSgprs : aliased stdint_h.uint32_t;  -- vulkan_core.h:8376
      ldsSizePerLocalWorkGroup : aliased stdint_h.uint32_t;  -- vulkan_core.h:8377
      ldsUsageSizeInBytes : aliased crtdefs_h.size_t;  -- vulkan_core.h:8378
      scratchMemUsageInBytes : aliased crtdefs_h.size_t;  -- vulkan_core.h:8379
   end record;
   pragma Convention (C_Pass_By_Copy, VkShaderResourceUsageAMD);  -- vulkan_core.h:8374

   type VkShaderStatisticsInfoAMD_computeWorkGroupSize_array is array (0 .. 2) of aliased stdint_h.uint32_t;
   type VkShaderStatisticsInfoAMD is record
      shaderStageMask : aliased VkShaderStageFlags;  -- vulkan_core.h:8383
      resourceUsage : aliased VkShaderResourceUsageAMD;  -- vulkan_core.h:8384
      numPhysicalVgprs : aliased stdint_h.uint32_t;  -- vulkan_core.h:8385
      numPhysicalSgprs : aliased stdint_h.uint32_t;  -- vulkan_core.h:8386
      numAvailableVgprs : aliased stdint_h.uint32_t;  -- vulkan_core.h:8387
      numAvailableSgprs : aliased stdint_h.uint32_t;  -- vulkan_core.h:8388
      computeWorkGroupSize : aliased VkShaderStatisticsInfoAMD_computeWorkGroupSize_array;  -- vulkan_core.h:8389
   end record;
   pragma Convention (C_Pass_By_Copy, VkShaderStatisticsInfoAMD);  -- vulkan_core.h:8382

   type PFN_vkGetShaderInfoAMD is access function
        (arg1 : VkDevice;
         arg2 : VkPipeline;
         arg3 : VkShaderStageFlagBits;
         arg4 : VkShaderInfoTypeAMD;
         arg5 : access crtdefs_h.size_t;
         arg6 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkGetShaderInfoAMD);  -- vulkan_core.h:8392

   function vkGetShaderInfoAMD
     (device : VkDevice;
      pipeline : VkPipeline;
      shaderStage : VkShaderStageFlagBits;
      infoType : VkShaderInfoTypeAMD;
      pInfoSize : access crtdefs_h.size_t;
      pInfo : System.Address) return VkResult;  -- vulkan_core.h:8395
   pragma Import (C, vkGetShaderInfoAMD, "vkGetShaderInfoAMD");

   type VkPhysicalDeviceCornerSampledImageFeaturesNV is record
      sType : aliased VkStructureType;  -- vulkan_core.h:8414
      pNext : System.Address;  -- vulkan_core.h:8415
      cornerSampledImage : aliased VkBool32;  -- vulkan_core.h:8416
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceCornerSampledImageFeaturesNV);  -- vulkan_core.h:8413

   subtype VkExternalMemoryHandleTypeFlagBitsNV is unsigned;
   VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV : constant VkExternalMemoryHandleTypeFlagBitsNV := 1;
   VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV : constant VkExternalMemoryHandleTypeFlagBitsNV := 2;
   VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV : constant VkExternalMemoryHandleTypeFlagBitsNV := 4;
   VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV : constant VkExternalMemoryHandleTypeFlagBitsNV := 8;
   VK_EXTERNAL_MEMORY_HANDLE_TYPE_FLAG_BITS_MAX_ENUM_NV : constant VkExternalMemoryHandleTypeFlagBitsNV := 2147483647;  -- vulkan_core.h:8430

   subtype VkExternalMemoryHandleTypeFlagsNV is VkFlags;  -- vulkan_core.h:8437

   subtype VkExternalMemoryFeatureFlagBitsNV is unsigned;
   VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV : constant VkExternalMemoryFeatureFlagBitsNV := 1;
   VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV : constant VkExternalMemoryFeatureFlagBitsNV := 2;
   VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV : constant VkExternalMemoryFeatureFlagBitsNV := 4;
   VK_EXTERNAL_MEMORY_FEATURE_FLAG_BITS_MAX_ENUM_NV : constant VkExternalMemoryFeatureFlagBitsNV := 2147483647;  -- vulkan_core.h:8439

   subtype VkExternalMemoryFeatureFlagsNV is VkFlags;  -- vulkan_core.h:8445

   type VkExternalImageFormatPropertiesNV is record
      imageFormatProperties : aliased VkImageFormatProperties;  -- vulkan_core.h:8447
      externalMemoryFeatures : aliased VkExternalMemoryFeatureFlagsNV;  -- vulkan_core.h:8448
      exportFromImportedHandleTypes : aliased VkExternalMemoryHandleTypeFlagsNV;  -- vulkan_core.h:8449
      compatibleHandleTypes : aliased VkExternalMemoryHandleTypeFlagsNV;  -- vulkan_core.h:8450
   end record;
   pragma Convention (C_Pass_By_Copy, VkExternalImageFormatPropertiesNV);  -- vulkan_core.h:8446

   type PFN_vkGetPhysicalDeviceExternalImageFormatPropertiesNV is access function
        (arg1 : VkPhysicalDevice;
         arg2 : VkFormat;
         arg3 : VkImageType;
         arg4 : VkImageTiling;
         arg5 : VkImageUsageFlags;
         arg6 : VkImageCreateFlags;
         arg7 : VkExternalMemoryHandleTypeFlagsNV;
         arg8 : access VkExternalImageFormatPropertiesNV) return VkResult;
   pragma Convention (C, PFN_vkGetPhysicalDeviceExternalImageFormatPropertiesNV);  -- vulkan_core.h:8453

   function vkGetPhysicalDeviceExternalImageFormatPropertiesNV
     (physicalDevice : VkPhysicalDevice;
      format : VkFormat;
      c_type : VkImageType;
      tiling : VkImageTiling;
      usage : VkImageUsageFlags;
      flags : VkImageCreateFlags;
      externalHandleType : VkExternalMemoryHandleTypeFlagsNV;
      pExternalImageFormatProperties : access VkExternalImageFormatPropertiesNV) return VkResult;  -- vulkan_core.h:8456
   pragma Import (C, vkGetPhysicalDeviceExternalImageFormatPropertiesNV, "vkGetPhysicalDeviceExternalImageFormatPropertiesNV");

   type VkExternalMemoryImageCreateInfoNV is record
      sType : aliased VkStructureType;  -- vulkan_core.h:8472
      pNext : System.Address;  -- vulkan_core.h:8473
      handleTypes : aliased VkExternalMemoryHandleTypeFlagsNV;  -- vulkan_core.h:8474
   end record;
   pragma Convention (C_Pass_By_Copy, VkExternalMemoryImageCreateInfoNV);  -- vulkan_core.h:8471

   type VkExportMemoryAllocateInfoNV is record
      sType : aliased VkStructureType;  -- vulkan_core.h:8478
      pNext : System.Address;  -- vulkan_core.h:8479
      handleTypes : aliased VkExternalMemoryHandleTypeFlagsNV;  -- vulkan_core.h:8480
   end record;
   pragma Convention (C_Pass_By_Copy, VkExportMemoryAllocateInfoNV);  -- vulkan_core.h:8477

   subtype VkValidationCheckEXT is unsigned;
   VK_VALIDATION_CHECK_ALL_EXT : constant VkValidationCheckEXT := 0;
   VK_VALIDATION_CHECK_SHADERS_EXT : constant VkValidationCheckEXT := 1;
   VK_VALIDATION_CHECK_MAX_ENUM_EXT : constant VkValidationCheckEXT := 2147483647;  -- vulkan_core.h:8489

   type VkValidationFlagsEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:8495
      pNext : System.Address;  -- vulkan_core.h:8496
      disabledValidationCheckCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:8497
      pDisabledValidationChecks : System.Address;  -- vulkan_core.h:8498
   end record;
   pragma Convention (C_Pass_By_Copy, VkValidationFlagsEXT);  -- vulkan_core.h:8494

   type VkPhysicalDeviceTextureCompressionASTCHDRFeaturesEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:8517
      pNext : System.Address;  -- vulkan_core.h:8518
      textureCompressionASTC_HDR : aliased VkBool32;  -- vulkan_core.h:8519
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceTextureCompressionASTCHDRFeaturesEXT);  -- vulkan_core.h:8516

   type VkImageViewASTCDecodeModeEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:8528
      pNext : System.Address;  -- vulkan_core.h:8529
      decodeMode : aliased VkFormat;  -- vulkan_core.h:8530
   end record;
   pragma Convention (C_Pass_By_Copy, VkImageViewASTCDecodeModeEXT);  -- vulkan_core.h:8527

   type VkPhysicalDeviceASTCDecodeFeaturesEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:8534
      pNext : System.Address;  -- vulkan_core.h:8535
      decodeModeSharedExponent : aliased VkBool32;  -- vulkan_core.h:8536
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceASTCDecodeFeaturesEXT);  -- vulkan_core.h:8533

   subtype VkConditionalRenderingFlagBitsEXT is unsigned;
   VK_CONDITIONAL_RENDERING_INVERTED_BIT_EXT : constant VkConditionalRenderingFlagBitsEXT := 1;
   VK_CONDITIONAL_RENDERING_FLAG_BITS_MAX_ENUM_EXT : constant VkConditionalRenderingFlagBitsEXT := 2147483647;  -- vulkan_core.h:8545

   subtype VkConditionalRenderingFlagsEXT is VkFlags;  -- vulkan_core.h:8549

   type VkConditionalRenderingBeginInfoEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:8551
      pNext : System.Address;  -- vulkan_core.h:8552
      buffer : VkBuffer;  -- vulkan_core.h:8553
      offset : aliased VkDeviceSize;  -- vulkan_core.h:8554
      flags : aliased VkConditionalRenderingFlagsEXT;  -- vulkan_core.h:8555
   end record;
   pragma Convention (C_Pass_By_Copy, VkConditionalRenderingBeginInfoEXT);  -- vulkan_core.h:8550

   type VkPhysicalDeviceConditionalRenderingFeaturesEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:8559
      pNext : System.Address;  -- vulkan_core.h:8560
      conditionalRendering : aliased VkBool32;  -- vulkan_core.h:8561
      inheritedConditionalRendering : aliased VkBool32;  -- vulkan_core.h:8562
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceConditionalRenderingFeaturesEXT);  -- vulkan_core.h:8558

   type VkCommandBufferInheritanceConditionalRenderingInfoEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:8566
      pNext : System.Address;  -- vulkan_core.h:8567
      conditionalRenderingEnable : aliased VkBool32;  -- vulkan_core.h:8568
   end record;
   pragma Convention (C_Pass_By_Copy, VkCommandBufferInheritanceConditionalRenderingInfoEXT);  -- vulkan_core.h:8565

   type PFN_vkCmdBeginConditionalRenderingEXT is access procedure (arg1 : VkCommandBuffer; arg2 : System.Address);
   pragma Convention (C, PFN_vkCmdBeginConditionalRenderingEXT);  -- vulkan_core.h:8571

   type PFN_vkCmdEndConditionalRenderingEXT is access procedure (arg1 : VkCommandBuffer);
   pragma Convention (C, PFN_vkCmdEndConditionalRenderingEXT);  -- vulkan_core.h:8572

   procedure vkCmdBeginConditionalRenderingEXT (commandBuffer : VkCommandBuffer; pConditionalRenderingBegin : System.Address);  -- vulkan_core.h:8575
   pragma Import (C, vkCmdBeginConditionalRenderingEXT, "vkCmdBeginConditionalRenderingEXT");

   procedure vkCmdEndConditionalRenderingEXT (commandBuffer : VkCommandBuffer);  -- vulkan_core.h:8579
   pragma Import (C, vkCmdEndConditionalRenderingEXT, "vkCmdEndConditionalRenderingEXT");

   type VkViewportWScalingNV is record
      xcoeff : aliased float;  -- vulkan_core.h:8588
      ycoeff : aliased float;  -- vulkan_core.h:8589
   end record;
   pragma Convention (C_Pass_By_Copy, VkViewportWScalingNV);  -- vulkan_core.h:8587

   type VkPipelineViewportWScalingStateCreateInfoNV is record
      sType : aliased VkStructureType;  -- vulkan_core.h:8593
      pNext : System.Address;  -- vulkan_core.h:8594
      viewportWScalingEnable : aliased VkBool32;  -- vulkan_core.h:8595
      viewportCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:8596
      pViewportWScalings : System.Address;  -- vulkan_core.h:8597
   end record;
   pragma Convention (C_Pass_By_Copy, VkPipelineViewportWScalingStateCreateInfoNV);  -- vulkan_core.h:8592

   type PFN_vkCmdSetViewportWScalingNV is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : stdint_h.uint32_t;
         arg3 : stdint_h.uint32_t;
         arg4 : System.Address);
   pragma Convention (C, PFN_vkCmdSetViewportWScalingNV);  -- vulkan_core.h:8600

   procedure vkCmdSetViewportWScalingNV
     (commandBuffer : VkCommandBuffer;
      firstViewport : stdint_h.uint32_t;
      viewportCount : stdint_h.uint32_t;
      pViewportWScalings : System.Address);  -- vulkan_core.h:8603
   pragma Import (C, vkCmdSetViewportWScalingNV, "vkCmdSetViewportWScalingNV");

   type PFN_vkReleaseDisplayEXT is access function (arg1 : VkPhysicalDevice; arg2 : VkDisplayKHR) return VkResult;
   pragma Convention (C, PFN_vkReleaseDisplayEXT);  -- vulkan_core.h:8614

   function vkReleaseDisplayEXT (physicalDevice : VkPhysicalDevice; display : VkDisplayKHR) return VkResult;  -- vulkan_core.h:8617
   pragma Import (C, vkReleaseDisplayEXT, "vkReleaseDisplayEXT");

   subtype VkSurfaceCounterFlagBitsEXT is unsigned;
   VK_SURFACE_COUNTER_VBLANK_BIT_EXT : constant VkSurfaceCounterFlagBitsEXT := 1;
   VK_SURFACE_COUNTER_VBLANK_EXT : constant VkSurfaceCounterFlagBitsEXT := 1;
   VK_SURFACE_COUNTER_FLAG_BITS_MAX_ENUM_EXT : constant VkSurfaceCounterFlagBitsEXT := 2147483647;  -- vulkan_core.h:8627

   subtype VkSurfaceCounterFlagsEXT is VkFlags;  -- vulkan_core.h:8632

   type VkSurfaceCapabilities2EXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:8634
      pNext : System.Address;  -- vulkan_core.h:8635
      minImageCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:8636
      maxImageCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:8637
      currentExtent : aliased VkExtent2D;  -- vulkan_core.h:8638
      minImageExtent : aliased VkExtent2D;  -- vulkan_core.h:8639
      maxImageExtent : aliased VkExtent2D;  -- vulkan_core.h:8640
      maxImageArrayLayers : aliased stdint_h.uint32_t;  -- vulkan_core.h:8641
      supportedTransforms : aliased VkSurfaceTransformFlagsKHR;  -- vulkan_core.h:8642
      currentTransform : aliased VkSurfaceTransformFlagBitsKHR;  -- vulkan_core.h:8643
      supportedCompositeAlpha : aliased VkCompositeAlphaFlagsKHR;  -- vulkan_core.h:8644
      supportedUsageFlags : aliased VkImageUsageFlags;  -- vulkan_core.h:8645
      supportedSurfaceCounters : aliased VkSurfaceCounterFlagsEXT;  -- vulkan_core.h:8646
   end record;
   pragma Convention (C_Pass_By_Copy, VkSurfaceCapabilities2EXT);  -- vulkan_core.h:8633

   type PFN_vkGetPhysicalDeviceSurfaceCapabilities2EXT is access function
        (arg1 : VkPhysicalDevice;
         arg2 : VkSurfaceKHR;
         arg3 : access VkSurfaceCapabilities2EXT) return VkResult;
   pragma Convention (C, PFN_vkGetPhysicalDeviceSurfaceCapabilities2EXT);  -- vulkan_core.h:8649

   function vkGetPhysicalDeviceSurfaceCapabilities2EXT
     (physicalDevice : VkPhysicalDevice;
      surface : VkSurfaceKHR;
      pSurfaceCapabilities : access VkSurfaceCapabilities2EXT) return VkResult;  -- vulkan_core.h:8652
   pragma Import (C, vkGetPhysicalDeviceSurfaceCapabilities2EXT, "vkGetPhysicalDeviceSurfaceCapabilities2EXT");

   subtype VkDisplayPowerStateEXT is unsigned;
   VK_DISPLAY_POWER_STATE_OFF_EXT : constant VkDisplayPowerStateEXT := 0;
   VK_DISPLAY_POWER_STATE_SUSPEND_EXT : constant VkDisplayPowerStateEXT := 1;
   VK_DISPLAY_POWER_STATE_ON_EXT : constant VkDisplayPowerStateEXT := 2;
   VK_DISPLAY_POWER_STATE_MAX_ENUM_EXT : constant VkDisplayPowerStateEXT := 2147483647;  -- vulkan_core.h:8663

   subtype VkDeviceEventTypeEXT is unsigned;
   VK_DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT : constant VkDeviceEventTypeEXT := 0;
   VK_DEVICE_EVENT_TYPE_MAX_ENUM_EXT : constant VkDeviceEventTypeEXT := 2147483647;  -- vulkan_core.h:8670

   subtype VkDisplayEventTypeEXT is unsigned;
   VK_DISPLAY_EVENT_TYPE_FIRST_PIXEL_OUT_EXT : constant VkDisplayEventTypeEXT := 0;
   VK_DISPLAY_EVENT_TYPE_MAX_ENUM_EXT : constant VkDisplayEventTypeEXT := 2147483647;  -- vulkan_core.h:8675

   type VkDisplayPowerInfoEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:8680
      pNext : System.Address;  -- vulkan_core.h:8681
      powerState : aliased VkDisplayPowerStateEXT;  -- vulkan_core.h:8682
   end record;
   pragma Convention (C_Pass_By_Copy, VkDisplayPowerInfoEXT);  -- vulkan_core.h:8679

   type VkDeviceEventInfoEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:8686
      pNext : System.Address;  -- vulkan_core.h:8687
      deviceEvent : aliased VkDeviceEventTypeEXT;  -- vulkan_core.h:8688
   end record;
   pragma Convention (C_Pass_By_Copy, VkDeviceEventInfoEXT);  -- vulkan_core.h:8685

   type VkDisplayEventInfoEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:8692
      pNext : System.Address;  -- vulkan_core.h:8693
      displayEvent : aliased VkDisplayEventTypeEXT;  -- vulkan_core.h:8694
   end record;
   pragma Convention (C_Pass_By_Copy, VkDisplayEventInfoEXT);  -- vulkan_core.h:8691

   type VkSwapchainCounterCreateInfoEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:8698
      pNext : System.Address;  -- vulkan_core.h:8699
      surfaceCounters : aliased VkSurfaceCounterFlagsEXT;  -- vulkan_core.h:8700
   end record;
   pragma Convention (C_Pass_By_Copy, VkSwapchainCounterCreateInfoEXT);  -- vulkan_core.h:8697

   type PFN_vkDisplayPowerControlEXT is access function
        (arg1 : VkDevice;
         arg2 : VkDisplayKHR;
         arg3 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkDisplayPowerControlEXT);  -- vulkan_core.h:8703

   type PFN_vkRegisterDeviceEventEXT is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : System.Address;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkRegisterDeviceEventEXT);  -- vulkan_core.h:8704

   type PFN_vkRegisterDisplayEventEXT is access function
        (arg1 : VkDevice;
         arg2 : VkDisplayKHR;
         arg3 : System.Address;
         arg4 : System.Address;
         arg5 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkRegisterDisplayEventEXT);  -- vulkan_core.h:8705

   type PFN_vkGetSwapchainCounterEXT is access function
        (arg1 : VkDevice;
         arg2 : VkSwapchainKHR;
         arg3 : VkSurfaceCounterFlagBitsEXT;
         arg4 : access stdint_h.uint64_t) return VkResult;
   pragma Convention (C, PFN_vkGetSwapchainCounterEXT);  -- vulkan_core.h:8706

   function vkDisplayPowerControlEXT
     (device : VkDevice;
      display : VkDisplayKHR;
      pDisplayPowerInfo : System.Address) return VkResult;  -- vulkan_core.h:8709
   pragma Import (C, vkDisplayPowerControlEXT, "vkDisplayPowerControlEXT");

   function vkRegisterDeviceEventEXT
     (device : VkDevice;
      pDeviceEventInfo : System.Address;
      pAllocator : System.Address;
      pFence : System.Address) return VkResult;  -- vulkan_core.h:8714
   pragma Import (C, vkRegisterDeviceEventEXT, "vkRegisterDeviceEventEXT");

   function vkRegisterDisplayEventEXT
     (device : VkDevice;
      display : VkDisplayKHR;
      pDisplayEventInfo : System.Address;
      pAllocator : System.Address;
      pFence : System.Address) return VkResult;  -- vulkan_core.h:8720
   pragma Import (C, vkRegisterDisplayEventEXT, "vkRegisterDisplayEventEXT");

   function vkGetSwapchainCounterEXT
     (device : VkDevice;
      swapchain : VkSwapchainKHR;
      counter : VkSurfaceCounterFlagBitsEXT;
      pCounterValue : access stdint_h.uint64_t) return VkResult;  -- vulkan_core.h:8727
   pragma Import (C, vkGetSwapchainCounterEXT, "vkGetSwapchainCounterEXT");

   type VkRefreshCycleDurationGOOGLE is record
      refreshDuration : aliased stdint_h.uint64_t;  -- vulkan_core.h:8739
   end record;
   pragma Convention (C_Pass_By_Copy, VkRefreshCycleDurationGOOGLE);  -- vulkan_core.h:8738

   type VkPastPresentationTimingGOOGLE is record
      presentID : aliased stdint_h.uint32_t;  -- vulkan_core.h:8743
      desiredPresentTime : aliased stdint_h.uint64_t;  -- vulkan_core.h:8744
      actualPresentTime : aliased stdint_h.uint64_t;  -- vulkan_core.h:8745
      earliestPresentTime : aliased stdint_h.uint64_t;  -- vulkan_core.h:8746
      presentMargin : aliased stdint_h.uint64_t;  -- vulkan_core.h:8747
   end record;
   pragma Convention (C_Pass_By_Copy, VkPastPresentationTimingGOOGLE);  -- vulkan_core.h:8742

   type VkPresentTimeGOOGLE is record
      presentID : aliased stdint_h.uint32_t;  -- vulkan_core.h:8751
      desiredPresentTime : aliased stdint_h.uint64_t;  -- vulkan_core.h:8752
   end record;
   pragma Convention (C_Pass_By_Copy, VkPresentTimeGOOGLE);  -- vulkan_core.h:8750

   type VkPresentTimesInfoGOOGLE is record
      sType : aliased VkStructureType;  -- vulkan_core.h:8756
      pNext : System.Address;  -- vulkan_core.h:8757
      swapchainCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:8758
      pTimes : System.Address;  -- vulkan_core.h:8759
   end record;
   pragma Convention (C_Pass_By_Copy, VkPresentTimesInfoGOOGLE);  -- vulkan_core.h:8755

   type PFN_vkGetRefreshCycleDurationGOOGLE is access function
        (arg1 : VkDevice;
         arg2 : VkSwapchainKHR;
         arg3 : access VkRefreshCycleDurationGOOGLE) return VkResult;
   pragma Convention (C, PFN_vkGetRefreshCycleDurationGOOGLE);  -- vulkan_core.h:8762

   type PFN_vkGetPastPresentationTimingGOOGLE is access function
        (arg1 : VkDevice;
         arg2 : VkSwapchainKHR;
         arg3 : access stdint_h.uint32_t;
         arg4 : access VkPastPresentationTimingGOOGLE) return VkResult;
   pragma Convention (C, PFN_vkGetPastPresentationTimingGOOGLE);  -- vulkan_core.h:8763

   function vkGetRefreshCycleDurationGOOGLE
     (device : VkDevice;
      swapchain : VkSwapchainKHR;
      pDisplayTimingProperties : access VkRefreshCycleDurationGOOGLE) return VkResult;  -- vulkan_core.h:8766
   pragma Import (C, vkGetRefreshCycleDurationGOOGLE, "vkGetRefreshCycleDurationGOOGLE");

   function vkGetPastPresentationTimingGOOGLE
     (device : VkDevice;
      swapchain : VkSwapchainKHR;
      pPresentationTimingCount : access stdint_h.uint32_t;
      pPresentationTimings : access VkPastPresentationTimingGOOGLE) return VkResult;  -- vulkan_core.h:8771
   pragma Import (C, vkGetPastPresentationTimingGOOGLE, "vkGetPastPresentationTimingGOOGLE");

   type VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX is record
      sType : aliased VkStructureType;  -- vulkan_core.h:8798
      pNext : System.Address;  -- vulkan_core.h:8799
      perViewPositionAllComponents : aliased VkBool32;  -- vulkan_core.h:8800
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX);  -- vulkan_core.h:8797

   subtype VkViewportCoordinateSwizzleNV is unsigned;
   VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_X_NV : constant VkViewportCoordinateSwizzleNV := 0;
   VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_X_NV : constant VkViewportCoordinateSwizzleNV := 1;
   VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Y_NV : constant VkViewportCoordinateSwizzleNV := 2;
   VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Y_NV : constant VkViewportCoordinateSwizzleNV := 3;
   VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Z_NV : constant VkViewportCoordinateSwizzleNV := 4;
   VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Z_NV : constant VkViewportCoordinateSwizzleNV := 5;
   VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_W_NV : constant VkViewportCoordinateSwizzleNV := 6;
   VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_W_NV : constant VkViewportCoordinateSwizzleNV := 7;
   VK_VIEWPORT_COORDINATE_SWIZZLE_MAX_ENUM_NV : constant VkViewportCoordinateSwizzleNV := 2147483647;  -- vulkan_core.h:8809

   subtype VkPipelineViewportSwizzleStateCreateFlagsNV is VkFlags;  -- vulkan_core.h:8820

   type VkViewportSwizzleNV is record
      x : aliased VkViewportCoordinateSwizzleNV;  -- vulkan_core.h:8822
      y : aliased VkViewportCoordinateSwizzleNV;  -- vulkan_core.h:8823
      z : aliased VkViewportCoordinateSwizzleNV;  -- vulkan_core.h:8824
      w : aliased VkViewportCoordinateSwizzleNV;  -- vulkan_core.h:8825
   end record;
   pragma Convention (C_Pass_By_Copy, VkViewportSwizzleNV);  -- vulkan_core.h:8821

   type VkPipelineViewportSwizzleStateCreateInfoNV is record
      sType : aliased VkStructureType;  -- vulkan_core.h:8829
      pNext : System.Address;  -- vulkan_core.h:8830
      flags : aliased VkPipelineViewportSwizzleStateCreateFlagsNV;  -- vulkan_core.h:8831
      viewportCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:8832
      pViewportSwizzles : System.Address;  -- vulkan_core.h:8833
   end record;
   pragma Convention (C_Pass_By_Copy, VkPipelineViewportSwizzleStateCreateInfoNV);  -- vulkan_core.h:8828

   subtype VkDiscardRectangleModeEXT is unsigned;
   VK_DISCARD_RECTANGLE_MODE_INCLUSIVE_EXT : constant VkDiscardRectangleModeEXT := 0;
   VK_DISCARD_RECTANGLE_MODE_EXCLUSIVE_EXT : constant VkDiscardRectangleModeEXT := 1;
   VK_DISCARD_RECTANGLE_MODE_MAX_ENUM_EXT : constant VkDiscardRectangleModeEXT := 2147483647;  -- vulkan_core.h:8842

   subtype VkPipelineDiscardRectangleStateCreateFlagsEXT is VkFlags;  -- vulkan_core.h:8847

   type VkPhysicalDeviceDiscardRectanglePropertiesEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:8849
      pNext : System.Address;  -- vulkan_core.h:8850
      maxDiscardRectangles : aliased stdint_h.uint32_t;  -- vulkan_core.h:8851
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceDiscardRectanglePropertiesEXT);  -- vulkan_core.h:8848

   type VkPipelineDiscardRectangleStateCreateInfoEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:8855
      pNext : System.Address;  -- vulkan_core.h:8856
      flags : aliased VkPipelineDiscardRectangleStateCreateFlagsEXT;  -- vulkan_core.h:8857
      discardRectangleMode : aliased VkDiscardRectangleModeEXT;  -- vulkan_core.h:8858
      discardRectangleCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:8859
      pDiscardRectangles : System.Address;  -- vulkan_core.h:8860
   end record;
   pragma Convention (C_Pass_By_Copy, VkPipelineDiscardRectangleStateCreateInfoEXT);  -- vulkan_core.h:8854

   type PFN_vkCmdSetDiscardRectangleEXT is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : stdint_h.uint32_t;
         arg3 : stdint_h.uint32_t;
         arg4 : System.Address);
   pragma Convention (C, PFN_vkCmdSetDiscardRectangleEXT);  -- vulkan_core.h:8863

   procedure vkCmdSetDiscardRectangleEXT
     (commandBuffer : VkCommandBuffer;
      firstDiscardRectangle : stdint_h.uint32_t;
      discardRectangleCount : stdint_h.uint32_t;
      pDiscardRectangles : System.Address);  -- vulkan_core.h:8866
   pragma Import (C, vkCmdSetDiscardRectangleEXT, "vkCmdSetDiscardRectangleEXT");

   subtype VkConservativeRasterizationModeEXT is unsigned;
   VK_CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT : constant VkConservativeRasterizationModeEXT := 0;
   VK_CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT : constant VkConservativeRasterizationModeEXT := 1;
   VK_CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT : constant VkConservativeRasterizationModeEXT := 2;
   VK_CONSERVATIVE_RASTERIZATION_MODE_MAX_ENUM_EXT : constant VkConservativeRasterizationModeEXT := 2147483647;  -- vulkan_core.h:8878

   subtype VkPipelineRasterizationConservativeStateCreateFlagsEXT is VkFlags;  -- vulkan_core.h:8884

   type VkPhysicalDeviceConservativeRasterizationPropertiesEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:8886
      pNext : System.Address;  -- vulkan_core.h:8887
      primitiveOverestimationSize : aliased float;  -- vulkan_core.h:8888
      maxExtraPrimitiveOverestimationSize : aliased float;  -- vulkan_core.h:8889
      extraPrimitiveOverestimationSizeGranularity : aliased float;  -- vulkan_core.h:8890
      primitiveUnderestimation : aliased VkBool32;  -- vulkan_core.h:8891
      conservativePointAndLineRasterization : aliased VkBool32;  -- vulkan_core.h:8892
      degenerateTrianglesRasterized : aliased VkBool32;  -- vulkan_core.h:8893
      degenerateLinesRasterized : aliased VkBool32;  -- vulkan_core.h:8894
      fullyCoveredFragmentShaderInputVariable : aliased VkBool32;  -- vulkan_core.h:8895
      conservativeRasterizationPostDepthCoverage : aliased VkBool32;  -- vulkan_core.h:8896
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceConservativeRasterizationPropertiesEXT);  -- vulkan_core.h:8885

   type VkPipelineRasterizationConservativeStateCreateInfoEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:8900
      pNext : System.Address;  -- vulkan_core.h:8901
      flags : aliased VkPipelineRasterizationConservativeStateCreateFlagsEXT;  -- vulkan_core.h:8902
      conservativeRasterizationMode : aliased VkConservativeRasterizationModeEXT;  -- vulkan_core.h:8903
      extraPrimitiveOverestimationSize : aliased float;  -- vulkan_core.h:8904
   end record;
   pragma Convention (C_Pass_By_Copy, VkPipelineRasterizationConservativeStateCreateInfoEXT);  -- vulkan_core.h:8899

   subtype VkPipelineRasterizationDepthClipStateCreateFlagsEXT is VkFlags;  -- vulkan_core.h:8912

   type VkPhysicalDeviceDepthClipEnableFeaturesEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:8914
      pNext : System.Address;  -- vulkan_core.h:8915
      depthClipEnable : aliased VkBool32;  -- vulkan_core.h:8916
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceDepthClipEnableFeaturesEXT);  -- vulkan_core.h:8913

   type VkPipelineRasterizationDepthClipStateCreateInfoEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:8920
      pNext : System.Address;  -- vulkan_core.h:8921
      flags : aliased VkPipelineRasterizationDepthClipStateCreateFlagsEXT;  -- vulkan_core.h:8922
      depthClipEnable : aliased VkBool32;  -- vulkan_core.h:8923
   end record;
   pragma Convention (C_Pass_By_Copy, VkPipelineRasterizationDepthClipStateCreateInfoEXT);  -- vulkan_core.h:8919

   type VkXYColorEXT is record
      x : aliased float;  -- vulkan_core.h:8937
      y : aliased float;  -- vulkan_core.h:8938
   end record;
   pragma Convention (C_Pass_By_Copy, VkXYColorEXT);  -- vulkan_core.h:8936

   type VkHdrMetadataEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:8942
      pNext : System.Address;  -- vulkan_core.h:8943
      displayPrimaryRed : aliased VkXYColorEXT;  -- vulkan_core.h:8944
      displayPrimaryGreen : aliased VkXYColorEXT;  -- vulkan_core.h:8945
      displayPrimaryBlue : aliased VkXYColorEXT;  -- vulkan_core.h:8946
      whitePoint : aliased VkXYColorEXT;  -- vulkan_core.h:8947
      maxLuminance : aliased float;  -- vulkan_core.h:8948
      minLuminance : aliased float;  -- vulkan_core.h:8949
      maxContentLightLevel : aliased float;  -- vulkan_core.h:8950
      maxFrameAverageLightLevel : aliased float;  -- vulkan_core.h:8951
   end record;
   pragma Convention (C_Pass_By_Copy, VkHdrMetadataEXT);  -- vulkan_core.h:8941

   type PFN_vkSetHdrMetadataEXT is access procedure
        (arg1 : VkDevice;
         arg2 : stdint_h.uint32_t;
         arg3 : System.Address;
         arg4 : System.Address);
   pragma Convention (C, PFN_vkSetHdrMetadataEXT);  -- vulkan_core.h:8954

   procedure vkSetHdrMetadataEXT
     (device : VkDevice;
      swapchainCount : stdint_h.uint32_t;
      pSwapchains : System.Address;
      pMetadata : System.Address);  -- vulkan_core.h:8957
   pragma Import (C, vkSetHdrMetadataEXT, "vkSetHdrMetadataEXT");

   type VkDebugUtilsMessengerEXT is new System.Address;  -- vulkan_core.h:8977

   --  skipped empty struct VkDebugUtilsMessengerEXT_T

   subtype VkDebugUtilsMessengerCallbackDataFlagsEXT is VkFlags;  -- vulkan_core.h:8980

   subtype VkDebugUtilsMessageSeverityFlagBitsEXT is unsigned;
   VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT : constant VkDebugUtilsMessageSeverityFlagBitsEXT := 1;
   VK_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT : constant VkDebugUtilsMessageSeverityFlagBitsEXT := 16;
   VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT : constant VkDebugUtilsMessageSeverityFlagBitsEXT := 256;
   VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT : constant VkDebugUtilsMessageSeverityFlagBitsEXT := 4096;
   VK_DEBUG_UTILS_MESSAGE_SEVERITY_FLAG_BITS_MAX_ENUM_EXT : constant VkDebugUtilsMessageSeverityFlagBitsEXT := 2147483647;  -- vulkan_core.h:8982

   subtype VkDebugUtilsMessageTypeFlagBitsEXT is unsigned;
   VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT : constant VkDebugUtilsMessageTypeFlagBitsEXT := 1;
   VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT : constant VkDebugUtilsMessageTypeFlagBitsEXT := 2;
   VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT : constant VkDebugUtilsMessageTypeFlagBitsEXT := 4;
   VK_DEBUG_UTILS_MESSAGE_TYPE_FLAG_BITS_MAX_ENUM_EXT : constant VkDebugUtilsMessageTypeFlagBitsEXT := 2147483647;  -- vulkan_core.h:8990

   subtype VkDebugUtilsMessageTypeFlagsEXT is VkFlags;  -- vulkan_core.h:8996

   subtype VkDebugUtilsMessageSeverityFlagsEXT is VkFlags;  -- vulkan_core.h:8997

   subtype VkDebugUtilsMessengerCreateFlagsEXT is VkFlags;  -- vulkan_core.h:8998

   type VkDebugUtilsLabelEXT_color_array is array (0 .. 3) of aliased float;
   type VkDebugUtilsLabelEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:9000
      pNext : System.Address;  -- vulkan_core.h:9001
      pLabelName : Interfaces.C.Strings.chars_ptr;  -- vulkan_core.h:9002
      color : aliased VkDebugUtilsLabelEXT_color_array;  -- vulkan_core.h:9003
   end record;
   pragma Convention (C_Pass_By_Copy, VkDebugUtilsLabelEXT);  -- vulkan_core.h:8999

   type VkDebugUtilsObjectNameInfoEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:9007
      pNext : System.Address;  -- vulkan_core.h:9008
      objectType : aliased VkObjectType;  -- vulkan_core.h:9009
      objectHandle : aliased stdint_h.uint64_t;  -- vulkan_core.h:9010
      pObjectName : Interfaces.C.Strings.chars_ptr;  -- vulkan_core.h:9011
   end record;
   pragma Convention (C_Pass_By_Copy, VkDebugUtilsObjectNameInfoEXT);  -- vulkan_core.h:9006

   type VkDebugUtilsMessengerCallbackDataEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:9015
      pNext : System.Address;  -- vulkan_core.h:9016
      flags : aliased VkDebugUtilsMessengerCallbackDataFlagsEXT;  -- vulkan_core.h:9017
      pMessageIdName : Interfaces.C.Strings.chars_ptr;  -- vulkan_core.h:9018
      messageIdNumber : aliased stdint_h.int32_t;  -- vulkan_core.h:9019
      pMessage : Interfaces.C.Strings.chars_ptr;  -- vulkan_core.h:9020
      queueLabelCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:9021
      pQueueLabels : System.Address;  -- vulkan_core.h:9022
      cmdBufLabelCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:9023
      pCmdBufLabels : System.Address;  -- vulkan_core.h:9024
      objectCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:9025
      pObjects : System.Address;  -- vulkan_core.h:9026
   end record;
   pragma Convention (C_Pass_By_Copy, VkDebugUtilsMessengerCallbackDataEXT);  -- vulkan_core.h:9014

   type PFN_vkDebugUtilsMessengerCallbackEXT is access function
        (arg1 : VkDebugUtilsMessageSeverityFlagBitsEXT;
         arg2 : VkDebugUtilsMessageTypeFlagsEXT;
         arg3 : System.Address;
         arg4 : System.Address) return VkBool32;
   pragma Convention (C, PFN_vkDebugUtilsMessengerCallbackEXT);  -- vulkan_core.h:9029

   type VkDebugUtilsMessengerCreateInfoEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:9036
      pNext : System.Address;  -- vulkan_core.h:9037
      flags : aliased VkDebugUtilsMessengerCreateFlagsEXT;  -- vulkan_core.h:9038
      messageSeverity : aliased VkDebugUtilsMessageSeverityFlagsEXT;  -- vulkan_core.h:9039
      messageType : aliased VkDebugUtilsMessageTypeFlagsEXT;  -- vulkan_core.h:9040
      pfnUserCallback : PFN_vkDebugUtilsMessengerCallbackEXT;  -- vulkan_core.h:9041
      pUserData : System.Address;  -- vulkan_core.h:9042
   end record;
   pragma Convention (C_Pass_By_Copy, VkDebugUtilsMessengerCreateInfoEXT);  -- vulkan_core.h:9035

   type VkDebugUtilsObjectTagInfoEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:9046
      pNext : System.Address;  -- vulkan_core.h:9047
      objectType : aliased VkObjectType;  -- vulkan_core.h:9048
      objectHandle : aliased stdint_h.uint64_t;  -- vulkan_core.h:9049
      tagName : aliased stdint_h.uint64_t;  -- vulkan_core.h:9050
      tagSize : aliased crtdefs_h.size_t;  -- vulkan_core.h:9051
      pTag : System.Address;  -- vulkan_core.h:9052
   end record;
   pragma Convention (C_Pass_By_Copy, VkDebugUtilsObjectTagInfoEXT);  -- vulkan_core.h:9045

   type PFN_vkSetDebugUtilsObjectNameEXT is access function (arg1 : VkDevice; arg2 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkSetDebugUtilsObjectNameEXT);  -- vulkan_core.h:9055

   type PFN_vkSetDebugUtilsObjectTagEXT is access function (arg1 : VkDevice; arg2 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkSetDebugUtilsObjectTagEXT);  -- vulkan_core.h:9056

   type PFN_vkQueueBeginDebugUtilsLabelEXT is access procedure (arg1 : VkQueue; arg2 : System.Address);
   pragma Convention (C, PFN_vkQueueBeginDebugUtilsLabelEXT);  -- vulkan_core.h:9057

   type PFN_vkQueueEndDebugUtilsLabelEXT is access procedure (arg1 : VkQueue);
   pragma Convention (C, PFN_vkQueueEndDebugUtilsLabelEXT);  -- vulkan_core.h:9058

   type PFN_vkQueueInsertDebugUtilsLabelEXT is access procedure (arg1 : VkQueue; arg2 : System.Address);
   pragma Convention (C, PFN_vkQueueInsertDebugUtilsLabelEXT);  -- vulkan_core.h:9059

   type PFN_vkCmdBeginDebugUtilsLabelEXT is access procedure (arg1 : VkCommandBuffer; arg2 : System.Address);
   pragma Convention (C, PFN_vkCmdBeginDebugUtilsLabelEXT);  -- vulkan_core.h:9060

   type PFN_vkCmdEndDebugUtilsLabelEXT is access procedure (arg1 : VkCommandBuffer);
   pragma Convention (C, PFN_vkCmdEndDebugUtilsLabelEXT);  -- vulkan_core.h:9061

   type PFN_vkCmdInsertDebugUtilsLabelEXT is access procedure (arg1 : VkCommandBuffer; arg2 : System.Address);
   pragma Convention (C, PFN_vkCmdInsertDebugUtilsLabelEXT);  -- vulkan_core.h:9062

   type PFN_vkCreateDebugUtilsMessengerEXT is access function
        (arg1 : VkInstance;
         arg2 : System.Address;
         arg3 : System.Address;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateDebugUtilsMessengerEXT);  -- vulkan_core.h:9063

   type PFN_vkDestroyDebugUtilsMessengerEXT is access procedure
        (arg1 : VkInstance;
         arg2 : VkDebugUtilsMessengerEXT;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkDestroyDebugUtilsMessengerEXT);  -- vulkan_core.h:9064

   type PFN_vkSubmitDebugUtilsMessageEXT is access procedure
        (arg1 : VkInstance;
         arg2 : VkDebugUtilsMessageSeverityFlagBitsEXT;
         arg3 : VkDebugUtilsMessageTypeFlagsEXT;
         arg4 : System.Address);
   pragma Convention (C, PFN_vkSubmitDebugUtilsMessageEXT);  -- vulkan_core.h:9065

   function vkSetDebugUtilsObjectNameEXT (device : VkDevice; pNameInfo : System.Address) return VkResult;  -- vulkan_core.h:9068
   pragma Import (C, vkSetDebugUtilsObjectNameEXT, "vkSetDebugUtilsObjectNameEXT");

   function vkSetDebugUtilsObjectTagEXT (device : VkDevice; pTagInfo : System.Address) return VkResult;  -- vulkan_core.h:9072
   pragma Import (C, vkSetDebugUtilsObjectTagEXT, "vkSetDebugUtilsObjectTagEXT");

   procedure vkQueueBeginDebugUtilsLabelEXT (queue : VkQueue; pLabelInfo : System.Address);  -- vulkan_core.h:9076
   pragma Import (C, vkQueueBeginDebugUtilsLabelEXT, "vkQueueBeginDebugUtilsLabelEXT");

   procedure vkQueueEndDebugUtilsLabelEXT (queue : VkQueue);  -- vulkan_core.h:9080
   pragma Import (C, vkQueueEndDebugUtilsLabelEXT, "vkQueueEndDebugUtilsLabelEXT");

   procedure vkQueueInsertDebugUtilsLabelEXT (queue : VkQueue; pLabelInfo : System.Address);  -- vulkan_core.h:9083
   pragma Import (C, vkQueueInsertDebugUtilsLabelEXT, "vkQueueInsertDebugUtilsLabelEXT");

   procedure vkCmdBeginDebugUtilsLabelEXT (commandBuffer : VkCommandBuffer; pLabelInfo : System.Address);  -- vulkan_core.h:9087
   pragma Import (C, vkCmdBeginDebugUtilsLabelEXT, "vkCmdBeginDebugUtilsLabelEXT");

   procedure vkCmdEndDebugUtilsLabelEXT (commandBuffer : VkCommandBuffer);  -- vulkan_core.h:9091
   pragma Import (C, vkCmdEndDebugUtilsLabelEXT, "vkCmdEndDebugUtilsLabelEXT");

   procedure vkCmdInsertDebugUtilsLabelEXT (commandBuffer : VkCommandBuffer; pLabelInfo : System.Address);  -- vulkan_core.h:9094
   pragma Import (C, vkCmdInsertDebugUtilsLabelEXT, "vkCmdInsertDebugUtilsLabelEXT");

   function vkCreateDebugUtilsMessengerEXT
     (instance : VkInstance;
      pCreateInfo : System.Address;
      pAllocator : System.Address;
      pMessenger : System.Address) return VkResult;  -- vulkan_core.h:9098
   pragma Import (C, vkCreateDebugUtilsMessengerEXT, "vkCreateDebugUtilsMessengerEXT");

   procedure vkDestroyDebugUtilsMessengerEXT
     (instance : VkInstance;
      messenger : VkDebugUtilsMessengerEXT;
      pAllocator : System.Address);  -- vulkan_core.h:9104
   pragma Import (C, vkDestroyDebugUtilsMessengerEXT, "vkDestroyDebugUtilsMessengerEXT");

   procedure vkSubmitDebugUtilsMessageEXT
     (instance : VkInstance;
      messageSeverity : VkDebugUtilsMessageSeverityFlagBitsEXT;
      messageTypes : VkDebugUtilsMessageTypeFlagsEXT;
      pCallbackData : System.Address);  -- vulkan_core.h:9109
   pragma Import (C, vkSubmitDebugUtilsMessageEXT, "vkSubmitDebugUtilsMessageEXT");

   subtype VkSamplerReductionModeEXT is VkSamplerReductionMode;

   subtype VkSamplerReductionModeCreateInfoEXT is VkSamplerReductionModeCreateInfo;

   subtype VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT is VkPhysicalDeviceSamplerFilterMinmaxProperties;

   type VkPhysicalDeviceInlineUniformBlockFeaturesEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:9147
      pNext : System.Address;  -- vulkan_core.h:9148
      inlineUniformBlock : aliased VkBool32;  -- vulkan_core.h:9149
      descriptorBindingInlineUniformBlockUpdateAfterBind : aliased VkBool32;  -- vulkan_core.h:9150
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceInlineUniformBlockFeaturesEXT);  -- vulkan_core.h:9146

   type VkPhysicalDeviceInlineUniformBlockPropertiesEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:9154
      pNext : System.Address;  -- vulkan_core.h:9155
      maxInlineUniformBlockSize : aliased stdint_h.uint32_t;  -- vulkan_core.h:9156
      maxPerStageDescriptorInlineUniformBlocks : aliased stdint_h.uint32_t;  -- vulkan_core.h:9157
      maxPerStageDescriptorUpdateAfterBindInlineUniformBlocks : aliased stdint_h.uint32_t;  -- vulkan_core.h:9158
      maxDescriptorSetInlineUniformBlocks : aliased stdint_h.uint32_t;  -- vulkan_core.h:9159
      maxDescriptorSetUpdateAfterBindInlineUniformBlocks : aliased stdint_h.uint32_t;  -- vulkan_core.h:9160
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceInlineUniformBlockPropertiesEXT);  -- vulkan_core.h:9153

   type VkWriteDescriptorSetInlineUniformBlockEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:9164
      pNext : System.Address;  -- vulkan_core.h:9165
      dataSize : aliased stdint_h.uint32_t;  -- vulkan_core.h:9166
      pData : System.Address;  -- vulkan_core.h:9167
   end record;
   pragma Convention (C_Pass_By_Copy, VkWriteDescriptorSetInlineUniformBlockEXT);  -- vulkan_core.h:9163

   type VkDescriptorPoolInlineUniformBlockCreateInfoEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:9171
      pNext : System.Address;  -- vulkan_core.h:9172
      maxInlineUniformBlockBindings : aliased stdint_h.uint32_t;  -- vulkan_core.h:9173
   end record;
   pragma Convention (C_Pass_By_Copy, VkDescriptorPoolInlineUniformBlockCreateInfoEXT);  -- vulkan_core.h:9170

   type VkSampleLocationEXT is record
      x : aliased float;  -- vulkan_core.h:9187
      y : aliased float;  -- vulkan_core.h:9188
   end record;
   pragma Convention (C_Pass_By_Copy, VkSampleLocationEXT);  -- vulkan_core.h:9186

   type VkSampleLocationsInfoEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:9192
      pNext : System.Address;  -- vulkan_core.h:9193
      sampleLocationsPerPixel : aliased VkSampleCountFlagBits;  -- vulkan_core.h:9194
      sampleLocationGridSize : aliased VkExtent2D;  -- vulkan_core.h:9195
      sampleLocationsCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:9196
      pSampleLocations : System.Address;  -- vulkan_core.h:9197
   end record;
   pragma Convention (C_Pass_By_Copy, VkSampleLocationsInfoEXT);  -- vulkan_core.h:9191

   type VkAttachmentSampleLocationsEXT is record
      attachmentIndex : aliased stdint_h.uint32_t;  -- vulkan_core.h:9201
      sampleLocationsInfo : aliased VkSampleLocationsInfoEXT;  -- vulkan_core.h:9202
   end record;
   pragma Convention (C_Pass_By_Copy, VkAttachmentSampleLocationsEXT);  -- vulkan_core.h:9200

   type VkSubpassSampleLocationsEXT is record
      subpassIndex : aliased stdint_h.uint32_t;  -- vulkan_core.h:9206
      sampleLocationsInfo : aliased VkSampleLocationsInfoEXT;  -- vulkan_core.h:9207
   end record;
   pragma Convention (C_Pass_By_Copy, VkSubpassSampleLocationsEXT);  -- vulkan_core.h:9205

   type VkRenderPassSampleLocationsBeginInfoEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:9211
      pNext : System.Address;  -- vulkan_core.h:9212
      attachmentInitialSampleLocationsCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:9213
      pAttachmentInitialSampleLocations : System.Address;  -- vulkan_core.h:9214
      postSubpassSampleLocationsCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:9215
      pPostSubpassSampleLocations : System.Address;  -- vulkan_core.h:9216
   end record;
   pragma Convention (C_Pass_By_Copy, VkRenderPassSampleLocationsBeginInfoEXT);  -- vulkan_core.h:9210

   type VkPipelineSampleLocationsStateCreateInfoEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:9220
      pNext : System.Address;  -- vulkan_core.h:9221
      sampleLocationsEnable : aliased VkBool32;  -- vulkan_core.h:9222
      sampleLocationsInfo : aliased VkSampleLocationsInfoEXT;  -- vulkan_core.h:9223
   end record;
   pragma Convention (C_Pass_By_Copy, VkPipelineSampleLocationsStateCreateInfoEXT);  -- vulkan_core.h:9219

   type VkPhysicalDeviceSampleLocationsPropertiesEXT_sampleLocationCoordinateRange_array is array (0 .. 1) of aliased float;
   type VkPhysicalDeviceSampleLocationsPropertiesEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:9227
      pNext : System.Address;  -- vulkan_core.h:9228
      sampleLocationSampleCounts : aliased VkSampleCountFlags;  -- vulkan_core.h:9229
      maxSampleLocationGridSize : aliased VkExtent2D;  -- vulkan_core.h:9230
      sampleLocationCoordinateRange : aliased VkPhysicalDeviceSampleLocationsPropertiesEXT_sampleLocationCoordinateRange_array;  -- vulkan_core.h:9231
      sampleLocationSubPixelBits : aliased stdint_h.uint32_t;  -- vulkan_core.h:9232
      variableSampleLocations : aliased VkBool32;  -- vulkan_core.h:9233
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceSampleLocationsPropertiesEXT);  -- vulkan_core.h:9226

   type VkMultisamplePropertiesEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:9237
      pNext : System.Address;  -- vulkan_core.h:9238
      maxSampleLocationGridSize : aliased VkExtent2D;  -- vulkan_core.h:9239
   end record;
   pragma Convention (C_Pass_By_Copy, VkMultisamplePropertiesEXT);  -- vulkan_core.h:9236

   type PFN_vkCmdSetSampleLocationsEXT is access procedure (arg1 : VkCommandBuffer; arg2 : System.Address);
   pragma Convention (C, PFN_vkCmdSetSampleLocationsEXT);  -- vulkan_core.h:9242

   type PFN_vkGetPhysicalDeviceMultisamplePropertiesEXT is access procedure
        (arg1 : VkPhysicalDevice;
         arg2 : VkSampleCountFlagBits;
         arg3 : access VkMultisamplePropertiesEXT);
   pragma Convention (C, PFN_vkGetPhysicalDeviceMultisamplePropertiesEXT);  -- vulkan_core.h:9243

   procedure vkCmdSetSampleLocationsEXT (commandBuffer : VkCommandBuffer; pSampleLocationsInfo : System.Address);  -- vulkan_core.h:9246
   pragma Import (C, vkCmdSetSampleLocationsEXT, "vkCmdSetSampleLocationsEXT");

   procedure vkGetPhysicalDeviceMultisamplePropertiesEXT
     (physicalDevice : VkPhysicalDevice;
      samples : VkSampleCountFlagBits;
      pMultisampleProperties : access VkMultisamplePropertiesEXT);  -- vulkan_core.h:9250
   pragma Import (C, vkGetPhysicalDeviceMultisamplePropertiesEXT, "vkGetPhysicalDeviceMultisamplePropertiesEXT");

   subtype VkBlendOverlapEXT is unsigned;
   VK_BLEND_OVERLAP_UNCORRELATED_EXT : constant VkBlendOverlapEXT := 0;
   VK_BLEND_OVERLAP_DISJOINT_EXT : constant VkBlendOverlapEXT := 1;
   VK_BLEND_OVERLAP_CONJOINT_EXT : constant VkBlendOverlapEXT := 2;
   VK_BLEND_OVERLAP_MAX_ENUM_EXT : constant VkBlendOverlapEXT := 2147483647;  -- vulkan_core.h:9261

   type VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:9268
      pNext : System.Address;  -- vulkan_core.h:9269
      advancedBlendCoherentOperations : aliased VkBool32;  -- vulkan_core.h:9270
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT);  -- vulkan_core.h:9267

   type VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:9274
      pNext : System.Address;  -- vulkan_core.h:9275
      advancedBlendMaxColorAttachments : aliased stdint_h.uint32_t;  -- vulkan_core.h:9276
      advancedBlendIndependentBlend : aliased VkBool32;  -- vulkan_core.h:9277
      advancedBlendNonPremultipliedSrcColor : aliased VkBool32;  -- vulkan_core.h:9278
      advancedBlendNonPremultipliedDstColor : aliased VkBool32;  -- vulkan_core.h:9279
      advancedBlendCorrelatedOverlap : aliased VkBool32;  -- vulkan_core.h:9280
      advancedBlendAllOperations : aliased VkBool32;  -- vulkan_core.h:9281
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT);  -- vulkan_core.h:9273

   type VkPipelineColorBlendAdvancedStateCreateInfoEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:9285
      pNext : System.Address;  -- vulkan_core.h:9286
      srcPremultiplied : aliased VkBool32;  -- vulkan_core.h:9287
      dstPremultiplied : aliased VkBool32;  -- vulkan_core.h:9288
      blendOverlap : aliased VkBlendOverlapEXT;  -- vulkan_core.h:9289
   end record;
   pragma Convention (C_Pass_By_Copy, VkPipelineColorBlendAdvancedStateCreateInfoEXT);  -- vulkan_core.h:9284

   subtype VkPipelineCoverageToColorStateCreateFlagsNV is VkFlags;  -- vulkan_core.h:9297

   type VkPipelineCoverageToColorStateCreateInfoNV is record
      sType : aliased VkStructureType;  -- vulkan_core.h:9299
      pNext : System.Address;  -- vulkan_core.h:9300
      flags : aliased VkPipelineCoverageToColorStateCreateFlagsNV;  -- vulkan_core.h:9301
      coverageToColorEnable : aliased VkBool32;  -- vulkan_core.h:9302
      coverageToColorLocation : aliased stdint_h.uint32_t;  -- vulkan_core.h:9303
   end record;
   pragma Convention (C_Pass_By_Copy, VkPipelineCoverageToColorStateCreateInfoNV);  -- vulkan_core.h:9298

   subtype VkCoverageModulationModeNV is unsigned;
   VK_COVERAGE_MODULATION_MODE_NONE_NV : constant VkCoverageModulationModeNV := 0;
   VK_COVERAGE_MODULATION_MODE_RGB_NV : constant VkCoverageModulationModeNV := 1;
   VK_COVERAGE_MODULATION_MODE_ALPHA_NV : constant VkCoverageModulationModeNV := 2;
   VK_COVERAGE_MODULATION_MODE_RGBA_NV : constant VkCoverageModulationModeNV := 3;
   VK_COVERAGE_MODULATION_MODE_MAX_ENUM_NV : constant VkCoverageModulationModeNV := 2147483647;  -- vulkan_core.h:9312

   subtype VkPipelineCoverageModulationStateCreateFlagsNV is VkFlags;  -- vulkan_core.h:9319

   type VkPipelineCoverageModulationStateCreateInfoNV is record
      sType : aliased VkStructureType;  -- vulkan_core.h:9321
      pNext : System.Address;  -- vulkan_core.h:9322
      flags : aliased VkPipelineCoverageModulationStateCreateFlagsNV;  -- vulkan_core.h:9323
      coverageModulationMode : aliased VkCoverageModulationModeNV;  -- vulkan_core.h:9324
      coverageModulationTableEnable : aliased VkBool32;  -- vulkan_core.h:9325
      coverageModulationTableCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:9326
      pCoverageModulationTable : access float;  -- vulkan_core.h:9327
   end record;
   pragma Convention (C_Pass_By_Copy, VkPipelineCoverageModulationStateCreateInfoNV);  -- vulkan_core.h:9320

   type VkPhysicalDeviceShaderSMBuiltinsPropertiesNV is record
      sType : aliased VkStructureType;  -- vulkan_core.h:9341
      pNext : System.Address;  -- vulkan_core.h:9342
      shaderSMCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:9343
      shaderWarpsPerSM : aliased stdint_h.uint32_t;  -- vulkan_core.h:9344
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceShaderSMBuiltinsPropertiesNV);  -- vulkan_core.h:9340

   type VkPhysicalDeviceShaderSMBuiltinsFeaturesNV is record
      sType : aliased VkStructureType;  -- vulkan_core.h:9348
      pNext : System.Address;  -- vulkan_core.h:9349
      shaderSMBuiltins : aliased VkBool32;  -- vulkan_core.h:9350
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceShaderSMBuiltinsFeaturesNV);  -- vulkan_core.h:9347

   type VkDrmFormatModifierPropertiesEXT is record
      drmFormatModifier : aliased stdint_h.uint64_t;  -- vulkan_core.h:9364
      drmFormatModifierPlaneCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:9365
      drmFormatModifierTilingFeatures : aliased VkFormatFeatureFlags;  -- vulkan_core.h:9366
   end record;
   pragma Convention (C_Pass_By_Copy, VkDrmFormatModifierPropertiesEXT);  -- vulkan_core.h:9363

   type VkDrmFormatModifierPropertiesListEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:9370
      pNext : System.Address;  -- vulkan_core.h:9371
      drmFormatModifierCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:9372
      pDrmFormatModifierProperties : access VkDrmFormatModifierPropertiesEXT;  -- vulkan_core.h:9373
   end record;
   pragma Convention (C_Pass_By_Copy, VkDrmFormatModifierPropertiesListEXT);  -- vulkan_core.h:9369

   type VkPhysicalDeviceImageDrmFormatModifierInfoEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:9377
      pNext : System.Address;  -- vulkan_core.h:9378
      drmFormatModifier : aliased stdint_h.uint64_t;  -- vulkan_core.h:9379
      sharingMode : aliased VkSharingMode;  -- vulkan_core.h:9380
      queueFamilyIndexCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:9381
      pQueueFamilyIndices : access stdint_h.uint32_t;  -- vulkan_core.h:9382
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceImageDrmFormatModifierInfoEXT);  -- vulkan_core.h:9376

   type VkImageDrmFormatModifierListCreateInfoEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:9386
      pNext : System.Address;  -- vulkan_core.h:9387
      drmFormatModifierCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:9388
      pDrmFormatModifiers : access stdint_h.uint64_t;  -- vulkan_core.h:9389
   end record;
   pragma Convention (C_Pass_By_Copy, VkImageDrmFormatModifierListCreateInfoEXT);  -- vulkan_core.h:9385

   type VkImageDrmFormatModifierExplicitCreateInfoEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:9393
      pNext : System.Address;  -- vulkan_core.h:9394
      drmFormatModifier : aliased stdint_h.uint64_t;  -- vulkan_core.h:9395
      drmFormatModifierPlaneCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:9396
      pPlaneLayouts : System.Address;  -- vulkan_core.h:9397
   end record;
   pragma Convention (C_Pass_By_Copy, VkImageDrmFormatModifierExplicitCreateInfoEXT);  -- vulkan_core.h:9392

   type VkImageDrmFormatModifierPropertiesEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:9401
      pNext : System.Address;  -- vulkan_core.h:9402
      drmFormatModifier : aliased stdint_h.uint64_t;  -- vulkan_core.h:9403
   end record;
   pragma Convention (C_Pass_By_Copy, VkImageDrmFormatModifierPropertiesEXT);  -- vulkan_core.h:9400

   type PFN_vkGetImageDrmFormatModifierPropertiesEXT is access function
        (arg1 : VkDevice;
         arg2 : VkImage;
         arg3 : access VkImageDrmFormatModifierPropertiesEXT) return VkResult;
   pragma Convention (C, PFN_vkGetImageDrmFormatModifierPropertiesEXT);  -- vulkan_core.h:9406

   function vkGetImageDrmFormatModifierPropertiesEXT
     (device : VkDevice;
      image : VkImage;
      pProperties : access VkImageDrmFormatModifierPropertiesEXT) return VkResult;  -- vulkan_core.h:9409
   pragma Import (C, vkGetImageDrmFormatModifierPropertiesEXT, "vkGetImageDrmFormatModifierPropertiesEXT");

   type VkValidationCacheEXT is new System.Address;  -- vulkan_core.h:9417

   --  skipped empty struct VkValidationCacheEXT_T

   subtype VkValidationCacheHeaderVersionEXT is unsigned;
   VK_VALIDATION_CACHE_HEADER_VERSION_ONE_EXT : constant VkValidationCacheHeaderVersionEXT := 1;
   VK_VALIDATION_CACHE_HEADER_VERSION_MAX_ENUM_EXT : constant VkValidationCacheHeaderVersionEXT := 2147483647;  -- vulkan_core.h:9421

   subtype VkValidationCacheCreateFlagsEXT is VkFlags;  -- vulkan_core.h:9425

   type VkValidationCacheCreateInfoEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:9427
      pNext : System.Address;  -- vulkan_core.h:9428
      flags : aliased VkValidationCacheCreateFlagsEXT;  -- vulkan_core.h:9429
      initialDataSize : aliased crtdefs_h.size_t;  -- vulkan_core.h:9430
      pInitialData : System.Address;  -- vulkan_core.h:9431
   end record;
   pragma Convention (C_Pass_By_Copy, VkValidationCacheCreateInfoEXT);  -- vulkan_core.h:9426

   type VkShaderModuleValidationCacheCreateInfoEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:9435
      pNext : System.Address;  -- vulkan_core.h:9436
      validationCache : VkValidationCacheEXT;  -- vulkan_core.h:9437
   end record;
   pragma Convention (C_Pass_By_Copy, VkShaderModuleValidationCacheCreateInfoEXT);  -- vulkan_core.h:9434

   type PFN_vkCreateValidationCacheEXT is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : System.Address;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateValidationCacheEXT);  -- vulkan_core.h:9440

   type PFN_vkDestroyValidationCacheEXT is access procedure
        (arg1 : VkDevice;
         arg2 : VkValidationCacheEXT;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkDestroyValidationCacheEXT);  -- vulkan_core.h:9441

   type PFN_vkMergeValidationCachesEXT is access function
        (arg1 : VkDevice;
         arg2 : VkValidationCacheEXT;
         arg3 : stdint_h.uint32_t;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkMergeValidationCachesEXT);  -- vulkan_core.h:9442

   type PFN_vkGetValidationCacheDataEXT is access function
        (arg1 : VkDevice;
         arg2 : VkValidationCacheEXT;
         arg3 : access crtdefs_h.size_t;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkGetValidationCacheDataEXT);  -- vulkan_core.h:9443

   function vkCreateValidationCacheEXT
     (device : VkDevice;
      pCreateInfo : System.Address;
      pAllocator : System.Address;
      pValidationCache : System.Address) return VkResult;  -- vulkan_core.h:9446
   pragma Import (C, vkCreateValidationCacheEXT, "vkCreateValidationCacheEXT");

   procedure vkDestroyValidationCacheEXT
     (device : VkDevice;
      validationCache : VkValidationCacheEXT;
      pAllocator : System.Address);  -- vulkan_core.h:9452
   pragma Import (C, vkDestroyValidationCacheEXT, "vkDestroyValidationCacheEXT");

   function vkMergeValidationCachesEXT
     (device : VkDevice;
      dstCache : VkValidationCacheEXT;
      srcCacheCount : stdint_h.uint32_t;
      pSrcCaches : System.Address) return VkResult;  -- vulkan_core.h:9457
   pragma Import (C, vkMergeValidationCachesEXT, "vkMergeValidationCachesEXT");

   function vkGetValidationCacheDataEXT
     (device : VkDevice;
      validationCache : VkValidationCacheEXT;
      pDataSize : access crtdefs_h.size_t;
      pData : System.Address) return VkResult;  -- vulkan_core.h:9463
   pragma Import (C, vkGetValidationCacheDataEXT, "vkGetValidationCacheDataEXT");

   subtype VkDescriptorBindingFlagBitsEXT is VkDescriptorBindingFlagBits;

   subtype VkDescriptorBindingFlagsEXT is VkDescriptorBindingFlags;  -- vulkan_core.h:9476

   subtype VkDescriptorSetLayoutBindingFlagsCreateInfoEXT is VkDescriptorSetLayoutBindingFlagsCreateInfo;

   subtype VkPhysicalDeviceDescriptorIndexingFeaturesEXT is VkPhysicalDeviceDescriptorIndexingFeatures;

   subtype VkPhysicalDeviceDescriptorIndexingPropertiesEXT is VkPhysicalDeviceDescriptorIndexingProperties;

   subtype VkDescriptorSetVariableDescriptorCountAllocateInfoEXT is VkDescriptorSetVariableDescriptorCountAllocateInfo;

   subtype VkDescriptorSetVariableDescriptorCountLayoutSupportEXT is VkDescriptorSetVariableDescriptorCountLayoutSupport;

   subtype VkShadingRatePaletteEntryNV is unsigned;
   VK_SHADING_RATE_PALETTE_ENTRY_NO_INVOCATIONS_NV : constant VkShadingRatePaletteEntryNV := 0;
   VK_SHADING_RATE_PALETTE_ENTRY_16_INVOCATIONS_PER_PIXEL_NV : constant VkShadingRatePaletteEntryNV := 1;
   VK_SHADING_RATE_PALETTE_ENTRY_8_INVOCATIONS_PER_PIXEL_NV : constant VkShadingRatePaletteEntryNV := 2;
   VK_SHADING_RATE_PALETTE_ENTRY_4_INVOCATIONS_PER_PIXEL_NV : constant VkShadingRatePaletteEntryNV := 3;
   VK_SHADING_RATE_PALETTE_ENTRY_2_INVOCATIONS_PER_PIXEL_NV : constant VkShadingRatePaletteEntryNV := 4;
   VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_PIXEL_NV : constant VkShadingRatePaletteEntryNV := 5;
   VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X1_PIXELS_NV : constant VkShadingRatePaletteEntryNV := 6;
   VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_1X2_PIXELS_NV : constant VkShadingRatePaletteEntryNV := 7;
   VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X2_PIXELS_NV : constant VkShadingRatePaletteEntryNV := 8;
   VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X2_PIXELS_NV : constant VkShadingRatePaletteEntryNV := 9;
   VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X4_PIXELS_NV : constant VkShadingRatePaletteEntryNV := 10;
   VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X4_PIXELS_NV : constant VkShadingRatePaletteEntryNV := 11;
   VK_SHADING_RATE_PALETTE_ENTRY_MAX_ENUM_NV : constant VkShadingRatePaletteEntryNV := 2147483647;  -- vulkan_core.h:9499

   subtype VkCoarseSampleOrderTypeNV is unsigned;
   VK_COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV : constant VkCoarseSampleOrderTypeNV := 0;
   VK_COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV : constant VkCoarseSampleOrderTypeNV := 1;
   VK_COARSE_SAMPLE_ORDER_TYPE_PIXEL_MAJOR_NV : constant VkCoarseSampleOrderTypeNV := 2;
   VK_COARSE_SAMPLE_ORDER_TYPE_SAMPLE_MAJOR_NV : constant VkCoarseSampleOrderTypeNV := 3;
   VK_COARSE_SAMPLE_ORDER_TYPE_MAX_ENUM_NV : constant VkCoarseSampleOrderTypeNV := 2147483647;  -- vulkan_core.h:9515

   type VkShadingRatePaletteNV is record
      shadingRatePaletteEntryCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:9523
      pShadingRatePaletteEntries : System.Address;  -- vulkan_core.h:9524
   end record;
   pragma Convention (C_Pass_By_Copy, VkShadingRatePaletteNV);  -- vulkan_core.h:9522

   type VkPipelineViewportShadingRateImageStateCreateInfoNV is record
      sType : aliased VkStructureType;  -- vulkan_core.h:9528
      pNext : System.Address;  -- vulkan_core.h:9529
      shadingRateImageEnable : aliased VkBool32;  -- vulkan_core.h:9530
      viewportCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:9531
      pShadingRatePalettes : System.Address;  -- vulkan_core.h:9532
   end record;
   pragma Convention (C_Pass_By_Copy, VkPipelineViewportShadingRateImageStateCreateInfoNV);  -- vulkan_core.h:9527

   type VkPhysicalDeviceShadingRateImageFeaturesNV is record
      sType : aliased VkStructureType;  -- vulkan_core.h:9536
      pNext : System.Address;  -- vulkan_core.h:9537
      shadingRateImage : aliased VkBool32;  -- vulkan_core.h:9538
      shadingRateCoarseSampleOrder : aliased VkBool32;  -- vulkan_core.h:9539
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceShadingRateImageFeaturesNV);  -- vulkan_core.h:9535

   type VkPhysicalDeviceShadingRateImagePropertiesNV is record
      sType : aliased VkStructureType;  -- vulkan_core.h:9543
      pNext : System.Address;  -- vulkan_core.h:9544
      shadingRateTexelSize : aliased VkExtent2D;  -- vulkan_core.h:9545
      shadingRatePaletteSize : aliased stdint_h.uint32_t;  -- vulkan_core.h:9546
      shadingRateMaxCoarseSamples : aliased stdint_h.uint32_t;  -- vulkan_core.h:9547
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceShadingRateImagePropertiesNV);  -- vulkan_core.h:9542

   type VkCoarseSampleLocationNV is record
      pixelX : aliased stdint_h.uint32_t;  -- vulkan_core.h:9551
      pixelY : aliased stdint_h.uint32_t;  -- vulkan_core.h:9552
      sample : aliased stdint_h.uint32_t;  -- vulkan_core.h:9553
   end record;
   pragma Convention (C_Pass_By_Copy, VkCoarseSampleLocationNV);  -- vulkan_core.h:9550

   type VkCoarseSampleOrderCustomNV is record
      shadingRate : aliased VkShadingRatePaletteEntryNV;  -- vulkan_core.h:9557
      sampleCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:9558
      sampleLocationCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:9559
      pSampleLocations : System.Address;  -- vulkan_core.h:9560
   end record;
   pragma Convention (C_Pass_By_Copy, VkCoarseSampleOrderCustomNV);  -- vulkan_core.h:9556

   type VkPipelineViewportCoarseSampleOrderStateCreateInfoNV is record
      sType : aliased VkStructureType;  -- vulkan_core.h:9564
      pNext : System.Address;  -- vulkan_core.h:9565
      sampleOrderType : aliased VkCoarseSampleOrderTypeNV;  -- vulkan_core.h:9566
      customSampleOrderCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:9567
      pCustomSampleOrders : System.Address;  -- vulkan_core.h:9568
   end record;
   pragma Convention (C_Pass_By_Copy, VkPipelineViewportCoarseSampleOrderStateCreateInfoNV);  -- vulkan_core.h:9563

   type PFN_vkCmdBindShadingRateImageNV is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkImageView;
         arg3 : VkImageLayout);
   pragma Convention (C, PFN_vkCmdBindShadingRateImageNV);  -- vulkan_core.h:9571

   type PFN_vkCmdSetViewportShadingRatePaletteNV is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : stdint_h.uint32_t;
         arg3 : stdint_h.uint32_t;
         arg4 : System.Address);
   pragma Convention (C, PFN_vkCmdSetViewportShadingRatePaletteNV);  -- vulkan_core.h:9572

   type PFN_vkCmdSetCoarseSampleOrderNV is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkCoarseSampleOrderTypeNV;
         arg3 : stdint_h.uint32_t;
         arg4 : System.Address);
   pragma Convention (C, PFN_vkCmdSetCoarseSampleOrderNV);  -- vulkan_core.h:9573

   procedure vkCmdBindShadingRateImageNV
     (commandBuffer : VkCommandBuffer;
      imageView : VkImageView;
      imageLayout : VkImageLayout);  -- vulkan_core.h:9576
   pragma Import (C, vkCmdBindShadingRateImageNV, "vkCmdBindShadingRateImageNV");

   procedure vkCmdSetViewportShadingRatePaletteNV
     (commandBuffer : VkCommandBuffer;
      firstViewport : stdint_h.uint32_t;
      viewportCount : stdint_h.uint32_t;
      pShadingRatePalettes : System.Address);  -- vulkan_core.h:9581
   pragma Import (C, vkCmdSetViewportShadingRatePaletteNV, "vkCmdSetViewportShadingRatePaletteNV");

   procedure vkCmdSetCoarseSampleOrderNV
     (commandBuffer : VkCommandBuffer;
      sampleOrderType : VkCoarseSampleOrderTypeNV;
      customSampleOrderCount : stdint_h.uint32_t;
      pCustomSampleOrders : System.Address);  -- vulkan_core.h:9587
   pragma Import (C, vkCmdSetCoarseSampleOrderNV, "vkCmdSetCoarseSampleOrderNV");

   type VkAccelerationStructureNV is new System.Address;  -- vulkan_core.h:9596

   --  skipped empty struct VkAccelerationStructureNV_T

   subtype VkRayTracingShaderGroupTypeKHR is unsigned;
   VK_RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_KHR : constant VkRayTracingShaderGroupTypeKHR := 0;
   VK_RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_KHR : constant VkRayTracingShaderGroupTypeKHR := 1;
   VK_RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_KHR : constant VkRayTracingShaderGroupTypeKHR := 2;
   VK_RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_NV : constant VkRayTracingShaderGroupTypeKHR := 0;
   VK_RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_NV : constant VkRayTracingShaderGroupTypeKHR := 1;
   VK_RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_NV : constant VkRayTracingShaderGroupTypeKHR := 2;
   VK_RAY_TRACING_SHADER_GROUP_TYPE_MAX_ENUM_KHR : constant VkRayTracingShaderGroupTypeKHR := 2147483647;  -- vulkan_core.h:9602

   subtype VkRayTracingShaderGroupTypeNV is VkRayTracingShaderGroupTypeKHR;

   subtype VkGeometryTypeKHR is unsigned;
   VK_GEOMETRY_TYPE_TRIANGLES_KHR : constant VkGeometryTypeKHR := 0;
   VK_GEOMETRY_TYPE_AABBS_KHR : constant VkGeometryTypeKHR := 1;
   VK_GEOMETRY_TYPE_INSTANCES_KHR : constant VkGeometryTypeKHR := 2;
   VK_GEOMETRY_TYPE_TRIANGLES_NV : constant VkGeometryTypeKHR := 0;
   VK_GEOMETRY_TYPE_AABBS_NV : constant VkGeometryTypeKHR := 1;
   VK_GEOMETRY_TYPE_MAX_ENUM_KHR : constant VkGeometryTypeKHR := 2147483647;  -- vulkan_core.h:9614

   subtype VkGeometryTypeNV is VkGeometryTypeKHR;

   subtype VkAccelerationStructureTypeKHR is unsigned;
   VK_ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR : constant VkAccelerationStructureTypeKHR := 0;
   VK_ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR : constant VkAccelerationStructureTypeKHR := 1;
   VK_ACCELERATION_STRUCTURE_TYPE_GENERIC_KHR : constant VkAccelerationStructureTypeKHR := 2;
   VK_ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_NV : constant VkAccelerationStructureTypeKHR := 0;
   VK_ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_NV : constant VkAccelerationStructureTypeKHR := 1;
   VK_ACCELERATION_STRUCTURE_TYPE_MAX_ENUM_KHR : constant VkAccelerationStructureTypeKHR := 2147483647;  -- vulkan_core.h:9625

   subtype VkAccelerationStructureTypeNV is VkAccelerationStructureTypeKHR;

   subtype VkCopyAccelerationStructureModeKHR is unsigned;
   VK_COPY_ACCELERATION_STRUCTURE_MODE_CLONE_KHR : constant VkCopyAccelerationStructureModeKHR := 0;
   VK_COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_KHR : constant VkCopyAccelerationStructureModeKHR := 1;
   VK_COPY_ACCELERATION_STRUCTURE_MODE_SERIALIZE_KHR : constant VkCopyAccelerationStructureModeKHR := 2;
   VK_COPY_ACCELERATION_STRUCTURE_MODE_DESERIALIZE_KHR : constant VkCopyAccelerationStructureModeKHR := 3;
   VK_COPY_ACCELERATION_STRUCTURE_MODE_CLONE_NV : constant VkCopyAccelerationStructureModeKHR := 0;
   VK_COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_NV : constant VkCopyAccelerationStructureModeKHR := 1;
   VK_COPY_ACCELERATION_STRUCTURE_MODE_MAX_ENUM_KHR : constant VkCopyAccelerationStructureModeKHR := 2147483647;  -- vulkan_core.h:9636

   subtype VkCopyAccelerationStructureModeNV is VkCopyAccelerationStructureModeKHR;

   subtype VkAccelerationStructureMemoryRequirementsTypeNV is unsigned;
   VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV : constant VkAccelerationStructureMemoryRequirementsTypeNV := 0;
   VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_NV : constant VkAccelerationStructureMemoryRequirementsTypeNV := 1;
   VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_NV : constant VkAccelerationStructureMemoryRequirementsTypeNV := 2;
   VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_MAX_ENUM_NV : constant VkAccelerationStructureMemoryRequirementsTypeNV := 2147483647;  -- vulkan_core.h:9648

   subtype VkGeometryFlagBitsKHR is unsigned;
   VK_GEOMETRY_OPAQUE_BIT_KHR : constant VkGeometryFlagBitsKHR := 1;
   VK_GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_KHR : constant VkGeometryFlagBitsKHR := 2;
   VK_GEOMETRY_OPAQUE_BIT_NV : constant VkGeometryFlagBitsKHR := 1;
   VK_GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_NV : constant VkGeometryFlagBitsKHR := 2;
   VK_GEOMETRY_FLAG_BITS_MAX_ENUM_KHR : constant VkGeometryFlagBitsKHR := 2147483647;  -- vulkan_core.h:9655

   subtype VkGeometryFlagsKHR is VkFlags;  -- vulkan_core.h:9662

   subtype VkGeometryFlagsNV is VkGeometryFlagsKHR;  -- vulkan_core.h:9663

   subtype VkGeometryFlagBitsNV is VkGeometryFlagBitsKHR;

   subtype VkGeometryInstanceFlagBitsKHR is unsigned;
   VK_GEOMETRY_INSTANCE_TRIANGLE_FACING_CULL_DISABLE_BIT_KHR : constant VkGeometryInstanceFlagBitsKHR := 1;
   VK_GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_KHR : constant VkGeometryInstanceFlagBitsKHR := 2;
   VK_GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_KHR : constant VkGeometryInstanceFlagBitsKHR := 4;
   VK_GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_KHR : constant VkGeometryInstanceFlagBitsKHR := 8;
   VK_GEOMETRY_INSTANCE_TRIANGLE_CULL_DISABLE_BIT_NV : constant VkGeometryInstanceFlagBitsKHR := 1;
   VK_GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_NV : constant VkGeometryInstanceFlagBitsKHR := 2;
   VK_GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_NV : constant VkGeometryInstanceFlagBitsKHR := 4;
   VK_GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_NV : constant VkGeometryInstanceFlagBitsKHR := 8;
   VK_GEOMETRY_INSTANCE_FLAG_BITS_MAX_ENUM_KHR : constant VkGeometryInstanceFlagBitsKHR := 2147483647;  -- vulkan_core.h:9668

   subtype VkGeometryInstanceFlagsKHR is VkFlags;  -- vulkan_core.h:9679

   subtype VkGeometryInstanceFlagsNV is VkGeometryInstanceFlagsKHR;  -- vulkan_core.h:9680

   subtype VkGeometryInstanceFlagBitsNV is VkGeometryInstanceFlagBitsKHR;

   subtype VkBuildAccelerationStructureFlagBitsKHR is unsigned;
   VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_KHR : constant VkBuildAccelerationStructureFlagBitsKHR := 1;
   VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_KHR : constant VkBuildAccelerationStructureFlagBitsKHR := 2;
   VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_KHR : constant VkBuildAccelerationStructureFlagBitsKHR := 4;
   VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_KHR : constant VkBuildAccelerationStructureFlagBitsKHR := 8;
   VK_BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_KHR : constant VkBuildAccelerationStructureFlagBitsKHR := 16;
   VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_NV : constant VkBuildAccelerationStructureFlagBitsKHR := 1;
   VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_NV : constant VkBuildAccelerationStructureFlagBitsKHR := 2;
   VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_NV : constant VkBuildAccelerationStructureFlagBitsKHR := 4;
   VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_NV : constant VkBuildAccelerationStructureFlagBitsKHR := 8;
   VK_BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_NV : constant VkBuildAccelerationStructureFlagBitsKHR := 16;
   VK_BUILD_ACCELERATION_STRUCTURE_FLAG_BITS_MAX_ENUM_KHR : constant VkBuildAccelerationStructureFlagBitsKHR := 2147483647;  -- vulkan_core.h:9685

   subtype VkBuildAccelerationStructureFlagsKHR is VkFlags;  -- vulkan_core.h:9698

   subtype VkBuildAccelerationStructureFlagsNV is VkBuildAccelerationStructureFlagsKHR;  -- vulkan_core.h:9699

   subtype VkBuildAccelerationStructureFlagBitsNV is VkBuildAccelerationStructureFlagBitsKHR;

   type VkRayTracingShaderGroupCreateInfoNV is record
      sType : aliased VkStructureType;  -- vulkan_core.h:9704
      pNext : System.Address;  -- vulkan_core.h:9705
      c_type : aliased VkRayTracingShaderGroupTypeKHR;  -- vulkan_core.h:9706
      generalShader : aliased stdint_h.uint32_t;  -- vulkan_core.h:9707
      closestHitShader : aliased stdint_h.uint32_t;  -- vulkan_core.h:9708
      anyHitShader : aliased stdint_h.uint32_t;  -- vulkan_core.h:9709
      intersectionShader : aliased stdint_h.uint32_t;  -- vulkan_core.h:9710
   end record;
   pragma Convention (C_Pass_By_Copy, VkRayTracingShaderGroupCreateInfoNV);  -- vulkan_core.h:9703

   type VkRayTracingPipelineCreateInfoNV is record
      sType : aliased VkStructureType;  -- vulkan_core.h:9714
      pNext : System.Address;  -- vulkan_core.h:9715
      flags : aliased VkPipelineCreateFlags;  -- vulkan_core.h:9716
      stageCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:9717
      pStages : System.Address;  -- vulkan_core.h:9718
      groupCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:9719
      pGroups : System.Address;  -- vulkan_core.h:9720
      maxRecursionDepth : aliased stdint_h.uint32_t;  -- vulkan_core.h:9721
      layout : VkPipelineLayout;  -- vulkan_core.h:9722
      basePipelineHandle : VkPipeline;  -- vulkan_core.h:9723
      basePipelineIndex : aliased stdint_h.int32_t;  -- vulkan_core.h:9724
   end record;
   pragma Convention (C_Pass_By_Copy, VkRayTracingPipelineCreateInfoNV);  -- vulkan_core.h:9713

   type VkGeometryTrianglesNV is record
      sType : aliased VkStructureType;  -- vulkan_core.h:9728
      pNext : System.Address;  -- vulkan_core.h:9729
      vertexData : VkBuffer;  -- vulkan_core.h:9730
      vertexOffset : aliased VkDeviceSize;  -- vulkan_core.h:9731
      vertexCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:9732
      vertexStride : aliased VkDeviceSize;  -- vulkan_core.h:9733
      vertexFormat : aliased VkFormat;  -- vulkan_core.h:9734
      indexData : VkBuffer;  -- vulkan_core.h:9735
      indexOffset : aliased VkDeviceSize;  -- vulkan_core.h:9736
      indexCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:9737
      indexType : aliased VkIndexType;  -- vulkan_core.h:9738
      transformData : VkBuffer;  -- vulkan_core.h:9739
      transformOffset : aliased VkDeviceSize;  -- vulkan_core.h:9740
   end record;
   pragma Convention (C_Pass_By_Copy, VkGeometryTrianglesNV);  -- vulkan_core.h:9727

   type VkGeometryAABBNV is record
      sType : aliased VkStructureType;  -- vulkan_core.h:9744
      pNext : System.Address;  -- vulkan_core.h:9745
      aabbData : VkBuffer;  -- vulkan_core.h:9746
      numAABBs : aliased stdint_h.uint32_t;  -- vulkan_core.h:9747
      stride : aliased stdint_h.uint32_t;  -- vulkan_core.h:9748
      offset : aliased VkDeviceSize;  -- vulkan_core.h:9749
   end record;
   pragma Convention (C_Pass_By_Copy, VkGeometryAABBNV);  -- vulkan_core.h:9743

   type VkGeometryDataNV is record
      triangles : aliased VkGeometryTrianglesNV;  -- vulkan_core.h:9753
      aabbs : aliased VkGeometryAABBNV;  -- vulkan_core.h:9754
   end record;
   pragma Convention (C_Pass_By_Copy, VkGeometryDataNV);  -- vulkan_core.h:9752

   type VkGeometryNV is record
      sType : aliased VkStructureType;  -- vulkan_core.h:9758
      pNext : System.Address;  -- vulkan_core.h:9759
      geometryType : aliased VkGeometryTypeKHR;  -- vulkan_core.h:9760
      geometry : aliased VkGeometryDataNV;  -- vulkan_core.h:9761
      flags : aliased VkGeometryFlagsKHR;  -- vulkan_core.h:9762
   end record;
   pragma Convention (C_Pass_By_Copy, VkGeometryNV);  -- vulkan_core.h:9757

   type VkAccelerationStructureInfoNV is record
      sType : aliased VkStructureType;  -- vulkan_core.h:9766
      pNext : System.Address;  -- vulkan_core.h:9767
      c_type : aliased VkAccelerationStructureTypeNV;  -- vulkan_core.h:9768
      flags : aliased VkBuildAccelerationStructureFlagsNV;  -- vulkan_core.h:9769
      instanceCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:9770
      geometryCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:9771
      pGeometries : System.Address;  -- vulkan_core.h:9772
   end record;
   pragma Convention (C_Pass_By_Copy, VkAccelerationStructureInfoNV);  -- vulkan_core.h:9765

   type VkAccelerationStructureCreateInfoNV is record
      sType : aliased VkStructureType;  -- vulkan_core.h:9776
      pNext : System.Address;  -- vulkan_core.h:9777
      compactedSize : aliased VkDeviceSize;  -- vulkan_core.h:9778
      info : aliased VkAccelerationStructureInfoNV;  -- vulkan_core.h:9779
   end record;
   pragma Convention (C_Pass_By_Copy, VkAccelerationStructureCreateInfoNV);  -- vulkan_core.h:9775

   type VkBindAccelerationStructureMemoryInfoNV is record
      sType : aliased VkStructureType;  -- vulkan_core.h:9783
      pNext : System.Address;  -- vulkan_core.h:9784
      accelerationStructure : VkAccelerationStructureNV;  -- vulkan_core.h:9785
      memory : VkDeviceMemory;  -- vulkan_core.h:9786
      memoryOffset : aliased VkDeviceSize;  -- vulkan_core.h:9787
      deviceIndexCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:9788
      pDeviceIndices : access stdint_h.uint32_t;  -- vulkan_core.h:9789
   end record;
   pragma Convention (C_Pass_By_Copy, VkBindAccelerationStructureMemoryInfoNV);  -- vulkan_core.h:9782

   type VkWriteDescriptorSetAccelerationStructureNV is record
      sType : aliased VkStructureType;  -- vulkan_core.h:9793
      pNext : System.Address;  -- vulkan_core.h:9794
      accelerationStructureCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:9795
      pAccelerationStructures : System.Address;  -- vulkan_core.h:9796
   end record;
   pragma Convention (C_Pass_By_Copy, VkWriteDescriptorSetAccelerationStructureNV);  -- vulkan_core.h:9792

   type VkAccelerationStructureMemoryRequirementsInfoNV is record
      sType : aliased VkStructureType;  -- vulkan_core.h:9800
      pNext : System.Address;  -- vulkan_core.h:9801
      c_type : aliased VkAccelerationStructureMemoryRequirementsTypeNV;  -- vulkan_core.h:9802
      accelerationStructure : VkAccelerationStructureNV;  -- vulkan_core.h:9803
   end record;
   pragma Convention (C_Pass_By_Copy, VkAccelerationStructureMemoryRequirementsInfoNV);  -- vulkan_core.h:9799

   type VkPhysicalDeviceRayTracingPropertiesNV is record
      sType : aliased VkStructureType;  -- vulkan_core.h:9807
      pNext : System.Address;  -- vulkan_core.h:9808
      shaderGroupHandleSize : aliased stdint_h.uint32_t;  -- vulkan_core.h:9809
      maxRecursionDepth : aliased stdint_h.uint32_t;  -- vulkan_core.h:9810
      maxShaderGroupStride : aliased stdint_h.uint32_t;  -- vulkan_core.h:9811
      shaderGroupBaseAlignment : aliased stdint_h.uint32_t;  -- vulkan_core.h:9812
      maxGeometryCount : aliased stdint_h.uint64_t;  -- vulkan_core.h:9813
      maxInstanceCount : aliased stdint_h.uint64_t;  -- vulkan_core.h:9814
      maxTriangleCount : aliased stdint_h.uint64_t;  -- vulkan_core.h:9815
      maxDescriptorSetAccelerationStructures : aliased stdint_h.uint32_t;  -- vulkan_core.h:9816
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceRayTracingPropertiesNV);  -- vulkan_core.h:9806

   type VkTransformMatrixKHR_matrix_array is array (0 .. 2, 0 .. 3) of aliased float;
   type VkTransformMatrixKHR is record
      matrix : aliased VkTransformMatrixKHR_matrix_array;  -- vulkan_core.h:9820
   end record;
   pragma Convention (C_Pass_By_Copy, VkTransformMatrixKHR);  -- vulkan_core.h:9819

   subtype VkTransformMatrixNV is VkTransformMatrixKHR;

   type VkAabbPositionsKHR is record
      minX : aliased float;  -- vulkan_core.h:9826
      minY : aliased float;  -- vulkan_core.h:9827
      minZ : aliased float;  -- vulkan_core.h:9828
      maxX : aliased float;  -- vulkan_core.h:9829
      maxY : aliased float;  -- vulkan_core.h:9830
      maxZ : aliased float;  -- vulkan_core.h:9831
   end record;
   pragma Convention (C_Pass_By_Copy, VkAabbPositionsKHR);  -- vulkan_core.h:9825

   subtype VkAabbPositionsNV is VkAabbPositionsKHR;

   type VkAccelerationStructureInstanceKHR is record
      transform : aliased VkTransformMatrixKHR;  -- vulkan_core.h:9837
      instanceCustomIndex : Extensions.Unsigned_24;  -- vulkan_core.h:9838
      mask : aliased unsigned_char;  -- vulkan_core.h:9839
      instanceShaderBindingTableRecordOffset : Extensions.Unsigned_24;  -- vulkan_core.h:9840
      flags : aliased unsigned_char;  -- vulkan_core.h:9841
      accelerationStructureReference : aliased stdint_h.uint64_t;  -- vulkan_core.h:9842
   end record;
   pragma Convention (C_Pass_By_Copy, VkAccelerationStructureInstanceKHR);
   pragma Pack (VkAccelerationStructureInstanceKHR);  -- vulkan_core.h:9836

   subtype VkAccelerationStructureInstanceNV is VkAccelerationStructureInstanceKHR;

   type PFN_vkCreateAccelerationStructureNV is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : System.Address;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateAccelerationStructureNV);  -- vulkan_core.h:9847

   type PFN_vkDestroyAccelerationStructureNV is access procedure
        (arg1 : VkDevice;
         arg2 : VkAccelerationStructureNV;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkDestroyAccelerationStructureNV);  -- vulkan_core.h:9848

   type PFN_vkGetAccelerationStructureMemoryRequirementsNV is access procedure
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : access VkMemoryRequirements2KHR);
   pragma Convention (C, PFN_vkGetAccelerationStructureMemoryRequirementsNV);  -- vulkan_core.h:9849

   type PFN_vkBindAccelerationStructureMemoryNV is access function
        (arg1 : VkDevice;
         arg2 : stdint_h.uint32_t;
         arg3 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkBindAccelerationStructureMemoryNV);  -- vulkan_core.h:9850

   type PFN_vkCmdBuildAccelerationStructureNV is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : System.Address;
         arg3 : VkBuffer;
         arg4 : VkDeviceSize;
         arg5 : VkBool32;
         arg6 : VkAccelerationStructureNV;
         arg7 : VkAccelerationStructureNV;
         arg8 : VkBuffer;
         arg9 : VkDeviceSize);
   pragma Convention (C, PFN_vkCmdBuildAccelerationStructureNV);  -- vulkan_core.h:9851

   type PFN_vkCmdCopyAccelerationStructureNV is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkAccelerationStructureNV;
         arg3 : VkAccelerationStructureNV;
         arg4 : VkCopyAccelerationStructureModeKHR);
   pragma Convention (C, PFN_vkCmdCopyAccelerationStructureNV);  -- vulkan_core.h:9852

   type PFN_vkCmdTraceRaysNV is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkBuffer;
         arg3 : VkDeviceSize;
         arg4 : VkBuffer;
         arg5 : VkDeviceSize;
         arg6 : VkDeviceSize;
         arg7 : VkBuffer;
         arg8 : VkDeviceSize;
         arg9 : VkDeviceSize;
         arg10 : VkBuffer;
         arg11 : VkDeviceSize;
         arg12 : VkDeviceSize;
         arg13 : stdint_h.uint32_t;
         arg14 : stdint_h.uint32_t;
         arg15 : stdint_h.uint32_t);
   pragma Convention (C, PFN_vkCmdTraceRaysNV);  -- vulkan_core.h:9853

   type PFN_vkCreateRayTracingPipelinesNV is access function
        (arg1 : VkDevice;
         arg2 : VkPipelineCache;
         arg3 : stdint_h.uint32_t;
         arg4 : System.Address;
         arg5 : System.Address;
         arg6 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateRayTracingPipelinesNV);  -- vulkan_core.h:9854

   type PFN_vkGetRayTracingShaderGroupHandlesKHR is access function
        (arg1 : VkDevice;
         arg2 : VkPipeline;
         arg3 : stdint_h.uint32_t;
         arg4 : stdint_h.uint32_t;
         arg5 : crtdefs_h.size_t;
         arg6 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkGetRayTracingShaderGroupHandlesKHR);  -- vulkan_core.h:9855

   type PFN_vkGetRayTracingShaderGroupHandlesNV is access function
        (arg1 : VkDevice;
         arg2 : VkPipeline;
         arg3 : stdint_h.uint32_t;
         arg4 : stdint_h.uint32_t;
         arg5 : crtdefs_h.size_t;
         arg6 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkGetRayTracingShaderGroupHandlesNV);  -- vulkan_core.h:9856

   type PFN_vkGetAccelerationStructureHandleNV is access function
        (arg1 : VkDevice;
         arg2 : VkAccelerationStructureNV;
         arg3 : crtdefs_h.size_t;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkGetAccelerationStructureHandleNV);  -- vulkan_core.h:9857

   type PFN_vkCmdWriteAccelerationStructuresPropertiesNV is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : stdint_h.uint32_t;
         arg3 : System.Address;
         arg4 : VkQueryType;
         arg5 : VkQueryPool;
         arg6 : stdint_h.uint32_t);
   pragma Convention (C, PFN_vkCmdWriteAccelerationStructuresPropertiesNV);  -- vulkan_core.h:9858

   type PFN_vkCompileDeferredNV is access function
        (arg1 : VkDevice;
         arg2 : VkPipeline;
         arg3 : stdint_h.uint32_t) return VkResult;
   pragma Convention (C, PFN_vkCompileDeferredNV);  -- vulkan_core.h:9859

   function vkCreateAccelerationStructureNV
     (device : VkDevice;
      pCreateInfo : System.Address;
      pAllocator : System.Address;
      pAccelerationStructure : System.Address) return VkResult;  -- vulkan_core.h:9862
   pragma Import (C, vkCreateAccelerationStructureNV, "vkCreateAccelerationStructureNV");

   procedure vkDestroyAccelerationStructureNV
     (device : VkDevice;
      accelerationStructure : VkAccelerationStructureNV;
      pAllocator : System.Address);  -- vulkan_core.h:9868
   pragma Import (C, vkDestroyAccelerationStructureNV, "vkDestroyAccelerationStructureNV");

   procedure vkGetAccelerationStructureMemoryRequirementsNV
     (device : VkDevice;
      pInfo : System.Address;
      pMemoryRequirements : access VkMemoryRequirements2KHR);  -- vulkan_core.h:9873
   pragma Import (C, vkGetAccelerationStructureMemoryRequirementsNV, "vkGetAccelerationStructureMemoryRequirementsNV");

   function vkBindAccelerationStructureMemoryNV
     (device : VkDevice;
      bindInfoCount : stdint_h.uint32_t;
      pBindInfos : System.Address) return VkResult;  -- vulkan_core.h:9878
   pragma Import (C, vkBindAccelerationStructureMemoryNV, "vkBindAccelerationStructureMemoryNV");

   procedure vkCmdBuildAccelerationStructureNV
     (commandBuffer : VkCommandBuffer;
      pInfo : System.Address;
      instanceData : VkBuffer;
      instanceOffset : VkDeviceSize;
      update : VkBool32;
      dst : VkAccelerationStructureNV;
      src : VkAccelerationStructureNV;
      scratch : VkBuffer;
      scratchOffset : VkDeviceSize);  -- vulkan_core.h:9883
   pragma Import (C, vkCmdBuildAccelerationStructureNV, "vkCmdBuildAccelerationStructureNV");

   procedure vkCmdCopyAccelerationStructureNV
     (commandBuffer : VkCommandBuffer;
      dst : VkAccelerationStructureNV;
      src : VkAccelerationStructureNV;
      mode : VkCopyAccelerationStructureModeKHR);  -- vulkan_core.h:9894
   pragma Import (C, vkCmdCopyAccelerationStructureNV, "vkCmdCopyAccelerationStructureNV");

   procedure vkCmdTraceRaysNV
     (commandBuffer : VkCommandBuffer;
      raygenShaderBindingTableBuffer : VkBuffer;
      raygenShaderBindingOffset : VkDeviceSize;
      missShaderBindingTableBuffer : VkBuffer;
      missShaderBindingOffset : VkDeviceSize;
      missShaderBindingStride : VkDeviceSize;
      hitShaderBindingTableBuffer : VkBuffer;
      hitShaderBindingOffset : VkDeviceSize;
      hitShaderBindingStride : VkDeviceSize;
      callableShaderBindingTableBuffer : VkBuffer;
      callableShaderBindingOffset : VkDeviceSize;
      callableShaderBindingStride : VkDeviceSize;
      width : stdint_h.uint32_t;
      height : stdint_h.uint32_t;
      depth : stdint_h.uint32_t);  -- vulkan_core.h:9900
   pragma Import (C, vkCmdTraceRaysNV, "vkCmdTraceRaysNV");

   function vkCreateRayTracingPipelinesNV
     (device : VkDevice;
      pipelineCache : VkPipelineCache;
      createInfoCount : stdint_h.uint32_t;
      pCreateInfos : System.Address;
      pAllocator : System.Address;
      pPipelines : System.Address) return VkResult;  -- vulkan_core.h:9917
   pragma Import (C, vkCreateRayTracingPipelinesNV, "vkCreateRayTracingPipelinesNV");

   function vkGetRayTracingShaderGroupHandlesKHR
     (device : VkDevice;
      pipeline : VkPipeline;
      firstGroup : stdint_h.uint32_t;
      groupCount : stdint_h.uint32_t;
      dataSize : crtdefs_h.size_t;
      pData : System.Address) return VkResult;  -- vulkan_core.h:9925
   pragma Import (C, vkGetRayTracingShaderGroupHandlesKHR, "vkGetRayTracingShaderGroupHandlesKHR");

   function vkGetRayTracingShaderGroupHandlesNV
     (device : VkDevice;
      pipeline : VkPipeline;
      firstGroup : stdint_h.uint32_t;
      groupCount : stdint_h.uint32_t;
      dataSize : crtdefs_h.size_t;
      pData : System.Address) return VkResult;  -- vulkan_core.h:9933
   pragma Import (C, vkGetRayTracingShaderGroupHandlesNV, "vkGetRayTracingShaderGroupHandlesNV");

   function vkGetAccelerationStructureHandleNV
     (device : VkDevice;
      accelerationStructure : VkAccelerationStructureNV;
      dataSize : crtdefs_h.size_t;
      pData : System.Address) return VkResult;  -- vulkan_core.h:9941
   pragma Import (C, vkGetAccelerationStructureHandleNV, "vkGetAccelerationStructureHandleNV");

   procedure vkCmdWriteAccelerationStructuresPropertiesNV
     (commandBuffer : VkCommandBuffer;
      accelerationStructureCount : stdint_h.uint32_t;
      pAccelerationStructures : System.Address;
      queryType : VkQueryType;
      queryPool : VkQueryPool;
      firstQuery : stdint_h.uint32_t);  -- vulkan_core.h:9947
   pragma Import (C, vkCmdWriteAccelerationStructuresPropertiesNV, "vkCmdWriteAccelerationStructuresPropertiesNV");

   function vkCompileDeferredNV
     (device : VkDevice;
      pipeline : VkPipeline;
      shader : stdint_h.uint32_t) return VkResult;  -- vulkan_core.h:9955
   pragma Import (C, vkCompileDeferredNV, "vkCompileDeferredNV");

   type VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV is record
      sType : aliased VkStructureType;  -- vulkan_core.h:9966
      pNext : System.Address;  -- vulkan_core.h:9967
      representativeFragmentTest : aliased VkBool32;  -- vulkan_core.h:9968
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV);  -- vulkan_core.h:9965

   type VkPipelineRepresentativeFragmentTestStateCreateInfoNV is record
      sType : aliased VkStructureType;  -- vulkan_core.h:9972
      pNext : System.Address;  -- vulkan_core.h:9973
      representativeFragmentTestEnable : aliased VkBool32;  -- vulkan_core.h:9974
   end record;
   pragma Convention (C_Pass_By_Copy, VkPipelineRepresentativeFragmentTestStateCreateInfoNV);  -- vulkan_core.h:9971

   type VkPhysicalDeviceImageViewImageFormatInfoEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:9983
      pNext : System.Address;  -- vulkan_core.h:9984
      imageViewType : aliased VkImageViewType;  -- vulkan_core.h:9985
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceImageViewImageFormatInfoEXT);  -- vulkan_core.h:9982

   type VkFilterCubicImageViewImageFormatPropertiesEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:9989
      pNext : System.Address;  -- vulkan_core.h:9990
      filterCubic : aliased VkBool32;  -- vulkan_core.h:9991
      filterCubicMinmax : aliased VkBool32;  -- vulkan_core.h:9992
   end record;
   pragma Convention (C_Pass_By_Copy, VkFilterCubicImageViewImageFormatPropertiesEXT);  -- vulkan_core.h:9988

   subtype VkQueueGlobalPriorityEXT is unsigned;
   VK_QUEUE_GLOBAL_PRIORITY_LOW_EXT : constant VkQueueGlobalPriorityEXT := 128;
   VK_QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT : constant VkQueueGlobalPriorityEXT := 256;
   VK_QUEUE_GLOBAL_PRIORITY_HIGH_EXT : constant VkQueueGlobalPriorityEXT := 512;
   VK_QUEUE_GLOBAL_PRIORITY_REALTIME_EXT : constant VkQueueGlobalPriorityEXT := 1024;
   VK_QUEUE_GLOBAL_PRIORITY_MAX_ENUM_EXT : constant VkQueueGlobalPriorityEXT := 2147483647;  -- vulkan_core.h:10006

   type VkDeviceQueueGlobalPriorityCreateInfoEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:10014
      pNext : System.Address;  -- vulkan_core.h:10015
      globalPriority : aliased VkQueueGlobalPriorityEXT;  -- vulkan_core.h:10016
   end record;
   pragma Convention (C_Pass_By_Copy, VkDeviceQueueGlobalPriorityCreateInfoEXT);  -- vulkan_core.h:10013

   type VkImportMemoryHostPointerInfoEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:10025
      pNext : System.Address;  -- vulkan_core.h:10026
      handleType : aliased VkExternalMemoryHandleTypeFlagBits;  -- vulkan_core.h:10027
      pHostPointer : System.Address;  -- vulkan_core.h:10028
   end record;
   pragma Convention (C_Pass_By_Copy, VkImportMemoryHostPointerInfoEXT);  -- vulkan_core.h:10024

   type VkMemoryHostPointerPropertiesEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:10032
      pNext : System.Address;  -- vulkan_core.h:10033
      memoryTypeBits : aliased stdint_h.uint32_t;  -- vulkan_core.h:10034
   end record;
   pragma Convention (C_Pass_By_Copy, VkMemoryHostPointerPropertiesEXT);  -- vulkan_core.h:10031

   type VkPhysicalDeviceExternalMemoryHostPropertiesEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:10038
      pNext : System.Address;  -- vulkan_core.h:10039
      minImportedHostPointerAlignment : aliased VkDeviceSize;  -- vulkan_core.h:10040
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceExternalMemoryHostPropertiesEXT);  -- vulkan_core.h:10037

   type PFN_vkGetMemoryHostPointerPropertiesEXT is access function
        (arg1 : VkDevice;
         arg2 : VkExternalMemoryHandleTypeFlagBits;
         arg3 : System.Address;
         arg4 : access VkMemoryHostPointerPropertiesEXT) return VkResult;
   pragma Convention (C, PFN_vkGetMemoryHostPointerPropertiesEXT);  -- vulkan_core.h:10043

   function vkGetMemoryHostPointerPropertiesEXT
     (device : VkDevice;
      handleType : VkExternalMemoryHandleTypeFlagBits;
      pHostPointer : System.Address;
      pMemoryHostPointerProperties : access VkMemoryHostPointerPropertiesEXT) return VkResult;  -- vulkan_core.h:10046
   pragma Import (C, vkGetMemoryHostPointerPropertiesEXT, "vkGetMemoryHostPointerPropertiesEXT");

   type PFN_vkCmdWriteBufferMarkerAMD is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkPipelineStageFlagBits;
         arg3 : VkBuffer;
         arg4 : VkDeviceSize;
         arg5 : stdint_h.uint32_t);
   pragma Convention (C, PFN_vkCmdWriteBufferMarkerAMD);  -- vulkan_core.h:10057

   procedure vkCmdWriteBufferMarkerAMD
     (commandBuffer : VkCommandBuffer;
      pipelineStage : VkPipelineStageFlagBits;
      dstBuffer : VkBuffer;
      dstOffset : VkDeviceSize;
      marker : stdint_h.uint32_t);  -- vulkan_core.h:10060
   pragma Import (C, vkCmdWriteBufferMarkerAMD, "vkCmdWriteBufferMarkerAMD");

   subtype VkPipelineCompilerControlFlagBitsAMD is unsigned;
   VK_PIPELINE_COMPILER_CONTROL_FLAG_BITS_MAX_ENUM_AMD : constant VkPipelineCompilerControlFlagBitsAMD := 2147483647;  -- vulkan_core.h:10073

   subtype VkPipelineCompilerControlFlagsAMD is VkFlags;  -- vulkan_core.h:10076

   type VkPipelineCompilerControlCreateInfoAMD is record
      sType : aliased VkStructureType;  -- vulkan_core.h:10078
      pNext : System.Address;  -- vulkan_core.h:10079
      compilerControlFlags : aliased VkPipelineCompilerControlFlagsAMD;  -- vulkan_core.h:10080
   end record;
   pragma Convention (C_Pass_By_Copy, VkPipelineCompilerControlCreateInfoAMD);  -- vulkan_core.h:10077

   subtype VkTimeDomainEXT is unsigned;
   VK_TIME_DOMAIN_DEVICE_EXT : constant VkTimeDomainEXT := 0;
   VK_TIME_DOMAIN_CLOCK_MONOTONIC_EXT : constant VkTimeDomainEXT := 1;
   VK_TIME_DOMAIN_CLOCK_MONOTONIC_RAW_EXT : constant VkTimeDomainEXT := 2;
   VK_TIME_DOMAIN_QUERY_PERFORMANCE_COUNTER_EXT : constant VkTimeDomainEXT := 3;
   VK_TIME_DOMAIN_MAX_ENUM_EXT : constant VkTimeDomainEXT := 2147483647;  -- vulkan_core.h:10089

   type VkCalibratedTimestampInfoEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:10097
      pNext : System.Address;  -- vulkan_core.h:10098
      timeDomain : aliased VkTimeDomainEXT;  -- vulkan_core.h:10099
   end record;
   pragma Convention (C_Pass_By_Copy, VkCalibratedTimestampInfoEXT);  -- vulkan_core.h:10096

   type PFN_vkGetPhysicalDeviceCalibrateableTimeDomainsEXT is access function
        (arg1 : VkPhysicalDevice;
         arg2 : access stdint_h.uint32_t;
         arg3 : access VkTimeDomainEXT) return VkResult;
   pragma Convention (C, PFN_vkGetPhysicalDeviceCalibrateableTimeDomainsEXT);  -- vulkan_core.h:10102

   type PFN_vkGetCalibratedTimestampsEXT is access function
        (arg1 : VkDevice;
         arg2 : stdint_h.uint32_t;
         arg3 : System.Address;
         arg4 : access stdint_h.uint64_t;
         arg5 : access stdint_h.uint64_t) return VkResult;
   pragma Convention (C, PFN_vkGetCalibratedTimestampsEXT);  -- vulkan_core.h:10103

   function vkGetPhysicalDeviceCalibrateableTimeDomainsEXT
     (physicalDevice : VkPhysicalDevice;
      pTimeDomainCount : access stdint_h.uint32_t;
      pTimeDomains : access VkTimeDomainEXT) return VkResult;  -- vulkan_core.h:10106
   pragma Import (C, vkGetPhysicalDeviceCalibrateableTimeDomainsEXT, "vkGetPhysicalDeviceCalibrateableTimeDomainsEXT");

   function vkGetCalibratedTimestampsEXT
     (device : VkDevice;
      timestampCount : stdint_h.uint32_t;
      pTimestampInfos : System.Address;
      pTimestamps : access stdint_h.uint64_t;
      pMaxDeviation : access stdint_h.uint64_t) return VkResult;  -- vulkan_core.h:10111
   pragma Import (C, vkGetCalibratedTimestampsEXT, "vkGetCalibratedTimestampsEXT");

   type VkPhysicalDeviceShaderCorePropertiesAMD is record
      sType : aliased VkStructureType;  -- vulkan_core.h:10124
      pNext : System.Address;  -- vulkan_core.h:10125
      shaderEngineCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:10126
      shaderArraysPerEngineCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:10127
      computeUnitsPerShaderArray : aliased stdint_h.uint32_t;  -- vulkan_core.h:10128
      simdPerComputeUnit : aliased stdint_h.uint32_t;  -- vulkan_core.h:10129
      wavefrontsPerSimd : aliased stdint_h.uint32_t;  -- vulkan_core.h:10130
      wavefrontSize : aliased stdint_h.uint32_t;  -- vulkan_core.h:10131
      sgprsPerSimd : aliased stdint_h.uint32_t;  -- vulkan_core.h:10132
      minSgprAllocation : aliased stdint_h.uint32_t;  -- vulkan_core.h:10133
      maxSgprAllocation : aliased stdint_h.uint32_t;  -- vulkan_core.h:10134
      sgprAllocationGranularity : aliased stdint_h.uint32_t;  -- vulkan_core.h:10135
      vgprsPerSimd : aliased stdint_h.uint32_t;  -- vulkan_core.h:10136
      minVgprAllocation : aliased stdint_h.uint32_t;  -- vulkan_core.h:10137
      maxVgprAllocation : aliased stdint_h.uint32_t;  -- vulkan_core.h:10138
      vgprAllocationGranularity : aliased stdint_h.uint32_t;  -- vulkan_core.h:10139
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceShaderCorePropertiesAMD);  -- vulkan_core.h:10123

   subtype VkMemoryOverallocationBehaviorAMD is unsigned;
   VK_MEMORY_OVERALLOCATION_BEHAVIOR_DEFAULT_AMD : constant VkMemoryOverallocationBehaviorAMD := 0;
   VK_MEMORY_OVERALLOCATION_BEHAVIOR_ALLOWED_AMD : constant VkMemoryOverallocationBehaviorAMD := 1;
   VK_MEMORY_OVERALLOCATION_BEHAVIOR_DISALLOWED_AMD : constant VkMemoryOverallocationBehaviorAMD := 2;
   VK_MEMORY_OVERALLOCATION_BEHAVIOR_MAX_ENUM_AMD : constant VkMemoryOverallocationBehaviorAMD := 2147483647;  -- vulkan_core.h:10148

   type VkDeviceMemoryOverallocationCreateInfoAMD is record
      sType : aliased VkStructureType;  -- vulkan_core.h:10155
      pNext : System.Address;  -- vulkan_core.h:10156
      overallocationBehavior : aliased VkMemoryOverallocationBehaviorAMD;  -- vulkan_core.h:10157
   end record;
   pragma Convention (C_Pass_By_Copy, VkDeviceMemoryOverallocationCreateInfoAMD);  -- vulkan_core.h:10154

   type VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:10166
      pNext : System.Address;  -- vulkan_core.h:10167
      maxVertexAttribDivisor : aliased stdint_h.uint32_t;  -- vulkan_core.h:10168
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT);  -- vulkan_core.h:10165

   type VkVertexInputBindingDivisorDescriptionEXT is record
      binding : aliased stdint_h.uint32_t;  -- vulkan_core.h:10172
      divisor : aliased stdint_h.uint32_t;  -- vulkan_core.h:10173
   end record;
   pragma Convention (C_Pass_By_Copy, VkVertexInputBindingDivisorDescriptionEXT);  -- vulkan_core.h:10171

   type VkPipelineVertexInputDivisorStateCreateInfoEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:10177
      pNext : System.Address;  -- vulkan_core.h:10178
      vertexBindingDivisorCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:10179
      pVertexBindingDivisors : System.Address;  -- vulkan_core.h:10180
   end record;
   pragma Convention (C_Pass_By_Copy, VkPipelineVertexInputDivisorStateCreateInfoEXT);  -- vulkan_core.h:10176

   type VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:10184
      pNext : System.Address;  -- vulkan_core.h:10185
      vertexAttributeInstanceRateDivisor : aliased VkBool32;  -- vulkan_core.h:10186
      vertexAttributeInstanceRateZeroDivisor : aliased VkBool32;  -- vulkan_core.h:10187
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT);  -- vulkan_core.h:10183

   subtype VkPipelineCreationFeedbackFlagBitsEXT is unsigned;
   VK_PIPELINE_CREATION_FEEDBACK_VALID_BIT_EXT : constant VkPipelineCreationFeedbackFlagBitsEXT := 1;
   VK_PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT_EXT : constant VkPipelineCreationFeedbackFlagBitsEXT := 2;
   VK_PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT_EXT : constant VkPipelineCreationFeedbackFlagBitsEXT := 4;
   VK_PIPELINE_CREATION_FEEDBACK_FLAG_BITS_MAX_ENUM_EXT : constant VkPipelineCreationFeedbackFlagBitsEXT := 2147483647;  -- vulkan_core.h:10196

   subtype VkPipelineCreationFeedbackFlagsEXT is VkFlags;  -- vulkan_core.h:10202

   type VkPipelineCreationFeedbackEXT is record
      flags : aliased VkPipelineCreationFeedbackFlagsEXT;  -- vulkan_core.h:10204
      duration : aliased stdint_h.uint64_t;  -- vulkan_core.h:10205
   end record;
   pragma Convention (C_Pass_By_Copy, VkPipelineCreationFeedbackEXT);  -- vulkan_core.h:10203

   type VkPipelineCreationFeedbackCreateInfoEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:10209
      pNext : System.Address;  -- vulkan_core.h:10210
      pPipelineCreationFeedback : access VkPipelineCreationFeedbackEXT;  -- vulkan_core.h:10211
      pipelineStageCreationFeedbackCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:10212
      pPipelineStageCreationFeedbacks : access VkPipelineCreationFeedbackEXT;  -- vulkan_core.h:10213
   end record;
   pragma Convention (C_Pass_By_Copy, VkPipelineCreationFeedbackCreateInfoEXT);  -- vulkan_core.h:10208

   type VkPhysicalDeviceComputeShaderDerivativesFeaturesNV is record
      sType : aliased VkStructureType;  -- vulkan_core.h:10227
      pNext : System.Address;  -- vulkan_core.h:10228
      computeDerivativeGroupQuads : aliased VkBool32;  -- vulkan_core.h:10229
      computeDerivativeGroupLinear : aliased VkBool32;  -- vulkan_core.h:10230
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceComputeShaderDerivativesFeaturesNV);  -- vulkan_core.h:10226

   type VkPhysicalDeviceMeshShaderFeaturesNV is record
      sType : aliased VkStructureType;  -- vulkan_core.h:10239
      pNext : System.Address;  -- vulkan_core.h:10240
      taskShader : aliased VkBool32;  -- vulkan_core.h:10241
      meshShader : aliased VkBool32;  -- vulkan_core.h:10242
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceMeshShaderFeaturesNV);  -- vulkan_core.h:10238

   type VkPhysicalDeviceMeshShaderPropertiesNV_maxTaskWorkGroupSize_array is array (0 .. 2) of aliased stdint_h.uint32_t;
   type VkPhysicalDeviceMeshShaderPropertiesNV_maxMeshWorkGroupSize_array is array (0 .. 2) of aliased stdint_h.uint32_t;
   type VkPhysicalDeviceMeshShaderPropertiesNV is record
      sType : aliased VkStructureType;  -- vulkan_core.h:10246
      pNext : System.Address;  -- vulkan_core.h:10247
      maxDrawMeshTasksCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:10248
      maxTaskWorkGroupInvocations : aliased stdint_h.uint32_t;  -- vulkan_core.h:10249
      maxTaskWorkGroupSize : aliased VkPhysicalDeviceMeshShaderPropertiesNV_maxTaskWorkGroupSize_array;  -- vulkan_core.h:10250
      maxTaskTotalMemorySize : aliased stdint_h.uint32_t;  -- vulkan_core.h:10251
      maxTaskOutputCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:10252
      maxMeshWorkGroupInvocations : aliased stdint_h.uint32_t;  -- vulkan_core.h:10253
      maxMeshWorkGroupSize : aliased VkPhysicalDeviceMeshShaderPropertiesNV_maxMeshWorkGroupSize_array;  -- vulkan_core.h:10254
      maxMeshTotalMemorySize : aliased stdint_h.uint32_t;  -- vulkan_core.h:10255
      maxMeshOutputVertices : aliased stdint_h.uint32_t;  -- vulkan_core.h:10256
      maxMeshOutputPrimitives : aliased stdint_h.uint32_t;  -- vulkan_core.h:10257
      maxMeshMultiviewViewCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:10258
      meshOutputPerVertexGranularity : aliased stdint_h.uint32_t;  -- vulkan_core.h:10259
      meshOutputPerPrimitiveGranularity : aliased stdint_h.uint32_t;  -- vulkan_core.h:10260
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceMeshShaderPropertiesNV);  -- vulkan_core.h:10245

   type VkDrawMeshTasksIndirectCommandNV is record
      taskCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:10264
      firstTask : aliased stdint_h.uint32_t;  -- vulkan_core.h:10265
   end record;
   pragma Convention (C_Pass_By_Copy, VkDrawMeshTasksIndirectCommandNV);  -- vulkan_core.h:10263

   type PFN_vkCmdDrawMeshTasksNV is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : stdint_h.uint32_t;
         arg3 : stdint_h.uint32_t);
   pragma Convention (C, PFN_vkCmdDrawMeshTasksNV);  -- vulkan_core.h:10268

   type PFN_vkCmdDrawMeshTasksIndirectNV is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkBuffer;
         arg3 : VkDeviceSize;
         arg4 : stdint_h.uint32_t;
         arg5 : stdint_h.uint32_t);
   pragma Convention (C, PFN_vkCmdDrawMeshTasksIndirectNV);  -- vulkan_core.h:10269

   type PFN_vkCmdDrawMeshTasksIndirectCountNV is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkBuffer;
         arg3 : VkDeviceSize;
         arg4 : VkBuffer;
         arg5 : VkDeviceSize;
         arg6 : stdint_h.uint32_t;
         arg7 : stdint_h.uint32_t);
   pragma Convention (C, PFN_vkCmdDrawMeshTasksIndirectCountNV);  -- vulkan_core.h:10270

   procedure vkCmdDrawMeshTasksNV
     (commandBuffer : VkCommandBuffer;
      taskCount : stdint_h.uint32_t;
      firstTask : stdint_h.uint32_t);  -- vulkan_core.h:10273
   pragma Import (C, vkCmdDrawMeshTasksNV, "vkCmdDrawMeshTasksNV");

   procedure vkCmdDrawMeshTasksIndirectNV
     (commandBuffer : VkCommandBuffer;
      buffer : VkBuffer;
      offset : VkDeviceSize;
      drawCount : stdint_h.uint32_t;
      stride : stdint_h.uint32_t);  -- vulkan_core.h:10278
   pragma Import (C, vkCmdDrawMeshTasksIndirectNV, "vkCmdDrawMeshTasksIndirectNV");

   procedure vkCmdDrawMeshTasksIndirectCountNV
     (commandBuffer : VkCommandBuffer;
      buffer : VkBuffer;
      offset : VkDeviceSize;
      countBuffer : VkBuffer;
      countBufferOffset : VkDeviceSize;
      maxDrawCount : stdint_h.uint32_t;
      stride : stdint_h.uint32_t);  -- vulkan_core.h:10285
   pragma Import (C, vkCmdDrawMeshTasksIndirectCountNV, "vkCmdDrawMeshTasksIndirectCountNV");

   type VkPhysicalDeviceFragmentShaderBarycentricFeaturesNV is record
      sType : aliased VkStructureType;  -- vulkan_core.h:10300
      pNext : System.Address;  -- vulkan_core.h:10301
      fragmentShaderBarycentric : aliased VkBool32;  -- vulkan_core.h:10302
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceFragmentShaderBarycentricFeaturesNV);  -- vulkan_core.h:10299

   type VkPhysicalDeviceShaderImageFootprintFeaturesNV is record
      sType : aliased VkStructureType;  -- vulkan_core.h:10311
      pNext : System.Address;  -- vulkan_core.h:10312
      imageFootprint : aliased VkBool32;  -- vulkan_core.h:10313
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceShaderImageFootprintFeaturesNV);  -- vulkan_core.h:10310

   type VkPipelineViewportExclusiveScissorStateCreateInfoNV is record
      sType : aliased VkStructureType;  -- vulkan_core.h:10322
      pNext : System.Address;  -- vulkan_core.h:10323
      exclusiveScissorCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:10324
      pExclusiveScissors : System.Address;  -- vulkan_core.h:10325
   end record;
   pragma Convention (C_Pass_By_Copy, VkPipelineViewportExclusiveScissorStateCreateInfoNV);  -- vulkan_core.h:10321

   type VkPhysicalDeviceExclusiveScissorFeaturesNV is record
      sType : aliased VkStructureType;  -- vulkan_core.h:10329
      pNext : System.Address;  -- vulkan_core.h:10330
      exclusiveScissor : aliased VkBool32;  -- vulkan_core.h:10331
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceExclusiveScissorFeaturesNV);  -- vulkan_core.h:10328

   type PFN_vkCmdSetExclusiveScissorNV is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : stdint_h.uint32_t;
         arg3 : stdint_h.uint32_t;
         arg4 : System.Address);
   pragma Convention (C, PFN_vkCmdSetExclusiveScissorNV);  -- vulkan_core.h:10334

   procedure vkCmdSetExclusiveScissorNV
     (commandBuffer : VkCommandBuffer;
      firstExclusiveScissor : stdint_h.uint32_t;
      exclusiveScissorCount : stdint_h.uint32_t;
      pExclusiveScissors : System.Address);  -- vulkan_core.h:10337
   pragma Import (C, vkCmdSetExclusiveScissorNV, "vkCmdSetExclusiveScissorNV");

   type VkQueueFamilyCheckpointPropertiesNV is record
      sType : aliased VkStructureType;  -- vulkan_core.h:10349
      pNext : System.Address;  -- vulkan_core.h:10350
      checkpointExecutionStageMask : aliased VkPipelineStageFlags;  -- vulkan_core.h:10351
   end record;
   pragma Convention (C_Pass_By_Copy, VkQueueFamilyCheckpointPropertiesNV);  -- vulkan_core.h:10348

   type VkCheckpointDataNV is record
      sType : aliased VkStructureType;  -- vulkan_core.h:10355
      pNext : System.Address;  -- vulkan_core.h:10356
      stage : aliased VkPipelineStageFlagBits;  -- vulkan_core.h:10357
      pCheckpointMarker : System.Address;  -- vulkan_core.h:10358
   end record;
   pragma Convention (C_Pass_By_Copy, VkCheckpointDataNV);  -- vulkan_core.h:10354

   type PFN_vkCmdSetCheckpointNV is access procedure (arg1 : VkCommandBuffer; arg2 : System.Address);
   pragma Convention (C, PFN_vkCmdSetCheckpointNV);  -- vulkan_core.h:10361

   type PFN_vkGetQueueCheckpointDataNV is access procedure
        (arg1 : VkQueue;
         arg2 : access stdint_h.uint32_t;
         arg3 : access VkCheckpointDataNV);
   pragma Convention (C, PFN_vkGetQueueCheckpointDataNV);  -- vulkan_core.h:10362

   procedure vkCmdSetCheckpointNV (commandBuffer : VkCommandBuffer; pCheckpointMarker : System.Address);  -- vulkan_core.h:10365
   pragma Import (C, vkCmdSetCheckpointNV, "vkCmdSetCheckpointNV");

   procedure vkGetQueueCheckpointDataNV
     (queue : VkQueue;
      pCheckpointDataCount : access stdint_h.uint32_t;
      pCheckpointData : access VkCheckpointDataNV);  -- vulkan_core.h:10369
   pragma Import (C, vkGetQueueCheckpointDataNV, "vkGetQueueCheckpointDataNV");

   type VkPhysicalDeviceShaderIntegerFunctions2FeaturesINTEL is record
      sType : aliased VkStructureType;  -- vulkan_core.h:10380
      pNext : System.Address;  -- vulkan_core.h:10381
      shaderIntegerFunctions2 : aliased VkBool32;  -- vulkan_core.h:10382
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceShaderIntegerFunctions2FeaturesINTEL);  -- vulkan_core.h:10379

   type VkPerformanceConfigurationINTEL is new System.Address;  -- vulkan_core.h:10388

   --  skipped empty struct VkPerformanceConfigurationINTEL_T

   subtype VkPerformanceConfigurationTypeINTEL is unsigned;
   VK_PERFORMANCE_CONFIGURATION_TYPE_COMMAND_QUEUE_METRICS_DISCOVERY_ACTIVATED_INTEL : constant VkPerformanceConfigurationTypeINTEL := 0;
   VK_PERFORMANCE_CONFIGURATION_TYPE_MAX_ENUM_INTEL : constant VkPerformanceConfigurationTypeINTEL := 2147483647;  -- vulkan_core.h:10392

   subtype VkQueryPoolSamplingModeINTEL is unsigned;
   VK_QUERY_POOL_SAMPLING_MODE_MANUAL_INTEL : constant VkQueryPoolSamplingModeINTEL := 0;
   VK_QUERY_POOL_SAMPLING_MODE_MAX_ENUM_INTEL : constant VkQueryPoolSamplingModeINTEL := 2147483647;  -- vulkan_core.h:10397

   subtype VkPerformanceOverrideTypeINTEL is unsigned;
   VK_PERFORMANCE_OVERRIDE_TYPE_NULL_HARDWARE_INTEL : constant VkPerformanceOverrideTypeINTEL := 0;
   VK_PERFORMANCE_OVERRIDE_TYPE_FLUSH_GPU_CACHES_INTEL : constant VkPerformanceOverrideTypeINTEL := 1;
   VK_PERFORMANCE_OVERRIDE_TYPE_MAX_ENUM_INTEL : constant VkPerformanceOverrideTypeINTEL := 2147483647;  -- vulkan_core.h:10402

   subtype VkPerformanceParameterTypeINTEL is unsigned;
   VK_PERFORMANCE_PARAMETER_TYPE_HW_COUNTERS_SUPPORTED_INTEL : constant VkPerformanceParameterTypeINTEL := 0;
   VK_PERFORMANCE_PARAMETER_TYPE_STREAM_MARKER_VALID_BITS_INTEL : constant VkPerformanceParameterTypeINTEL := 1;
   VK_PERFORMANCE_PARAMETER_TYPE_MAX_ENUM_INTEL : constant VkPerformanceParameterTypeINTEL := 2147483647;  -- vulkan_core.h:10408

   subtype VkPerformanceValueTypeINTEL is unsigned;
   VK_PERFORMANCE_VALUE_TYPE_UINT32_INTEL : constant VkPerformanceValueTypeINTEL := 0;
   VK_PERFORMANCE_VALUE_TYPE_UINT64_INTEL : constant VkPerformanceValueTypeINTEL := 1;
   VK_PERFORMANCE_VALUE_TYPE_FLOAT_INTEL : constant VkPerformanceValueTypeINTEL := 2;
   VK_PERFORMANCE_VALUE_TYPE_BOOL_INTEL : constant VkPerformanceValueTypeINTEL := 3;
   VK_PERFORMANCE_VALUE_TYPE_STRING_INTEL : constant VkPerformanceValueTypeINTEL := 4;
   VK_PERFORMANCE_VALUE_TYPE_MAX_ENUM_INTEL : constant VkPerformanceValueTypeINTEL := 2147483647;  -- vulkan_core.h:10414

   type VkPerformanceValueDataINTEL (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            value32 : aliased stdint_h.uint32_t;  -- vulkan_core.h:10423
         when 1 =>
            value64 : aliased stdint_h.uint64_t;  -- vulkan_core.h:10424
         when 2 =>
            valueFloat : aliased float;  -- vulkan_core.h:10425
         when 3 =>
            valueBool : aliased VkBool32;  -- vulkan_core.h:10426
         when others =>
            valueString : Interfaces.C.Strings.chars_ptr;  -- vulkan_core.h:10427
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, VkPerformanceValueDataINTEL);
   pragma Unchecked_Union (VkPerformanceValueDataINTEL);  -- vulkan_core.h:10422

   type VkPerformanceValueINTEL is record
      c_type : aliased VkPerformanceValueTypeINTEL;  -- vulkan_core.h:10431
      data : VkPerformanceValueDataINTEL;  -- vulkan_core.h:10432
   end record;
   pragma Convention (C_Pass_By_Copy, VkPerformanceValueINTEL);  -- vulkan_core.h:10430

   type VkInitializePerformanceApiInfoINTEL is record
      sType : aliased VkStructureType;  -- vulkan_core.h:10436
      pNext : System.Address;  -- vulkan_core.h:10437
      pUserData : System.Address;  -- vulkan_core.h:10438
   end record;
   pragma Convention (C_Pass_By_Copy, VkInitializePerformanceApiInfoINTEL);  -- vulkan_core.h:10435

   type VkQueryPoolPerformanceQueryCreateInfoINTEL is record
      sType : aliased VkStructureType;  -- vulkan_core.h:10442
      pNext : System.Address;  -- vulkan_core.h:10443
      performanceCountersSampling : aliased VkQueryPoolSamplingModeINTEL;  -- vulkan_core.h:10444
   end record;
   pragma Convention (C_Pass_By_Copy, VkQueryPoolPerformanceQueryCreateInfoINTEL);  -- vulkan_core.h:10441

   subtype VkQueryPoolCreateInfoINTEL is VkQueryPoolPerformanceQueryCreateInfoINTEL;

   type VkPerformanceMarkerInfoINTEL is record
      sType : aliased VkStructureType;  -- vulkan_core.h:10450
      pNext : System.Address;  -- vulkan_core.h:10451
      marker : aliased stdint_h.uint64_t;  -- vulkan_core.h:10452
   end record;
   pragma Convention (C_Pass_By_Copy, VkPerformanceMarkerInfoINTEL);  -- vulkan_core.h:10449

   type VkPerformanceStreamMarkerInfoINTEL is record
      sType : aliased VkStructureType;  -- vulkan_core.h:10456
      pNext : System.Address;  -- vulkan_core.h:10457
      marker : aliased stdint_h.uint32_t;  -- vulkan_core.h:10458
   end record;
   pragma Convention (C_Pass_By_Copy, VkPerformanceStreamMarkerInfoINTEL);  -- vulkan_core.h:10455

   type VkPerformanceOverrideInfoINTEL is record
      sType : aliased VkStructureType;  -- vulkan_core.h:10462
      pNext : System.Address;  -- vulkan_core.h:10463
      c_type : aliased VkPerformanceOverrideTypeINTEL;  -- vulkan_core.h:10464
      enable : aliased VkBool32;  -- vulkan_core.h:10465
      parameter : aliased stdint_h.uint64_t;  -- vulkan_core.h:10466
   end record;
   pragma Convention (C_Pass_By_Copy, VkPerformanceOverrideInfoINTEL);  -- vulkan_core.h:10461

   type VkPerformanceConfigurationAcquireInfoINTEL is record
      sType : aliased VkStructureType;  -- vulkan_core.h:10470
      pNext : System.Address;  -- vulkan_core.h:10471
      c_type : aliased VkPerformanceConfigurationTypeINTEL;  -- vulkan_core.h:10472
   end record;
   pragma Convention (C_Pass_By_Copy, VkPerformanceConfigurationAcquireInfoINTEL);  -- vulkan_core.h:10469

   type PFN_vkInitializePerformanceApiINTEL is access function (arg1 : VkDevice; arg2 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkInitializePerformanceApiINTEL);  -- vulkan_core.h:10475

   type PFN_vkUninitializePerformanceApiINTEL is access procedure (arg1 : VkDevice);
   pragma Convention (C, PFN_vkUninitializePerformanceApiINTEL);  -- vulkan_core.h:10476

   type PFN_vkCmdSetPerformanceMarkerINTEL is access function (arg1 : VkCommandBuffer; arg2 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCmdSetPerformanceMarkerINTEL);  -- vulkan_core.h:10477

   type PFN_vkCmdSetPerformanceStreamMarkerINTEL is access function (arg1 : VkCommandBuffer; arg2 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCmdSetPerformanceStreamMarkerINTEL);  -- vulkan_core.h:10478

   type PFN_vkCmdSetPerformanceOverrideINTEL is access function (arg1 : VkCommandBuffer; arg2 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCmdSetPerformanceOverrideINTEL);  -- vulkan_core.h:10479

   type PFN_vkAcquirePerformanceConfigurationINTEL is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkAcquirePerformanceConfigurationINTEL);  -- vulkan_core.h:10480

   type PFN_vkReleasePerformanceConfigurationINTEL is access function (arg1 : VkDevice; arg2 : VkPerformanceConfigurationINTEL) return VkResult;
   pragma Convention (C, PFN_vkReleasePerformanceConfigurationINTEL);  -- vulkan_core.h:10481

   type PFN_vkQueueSetPerformanceConfigurationINTEL is access function (arg1 : VkQueue; arg2 : VkPerformanceConfigurationINTEL) return VkResult;
   pragma Convention (C, PFN_vkQueueSetPerformanceConfigurationINTEL);  -- vulkan_core.h:10482

   type PFN_vkGetPerformanceParameterINTEL is access function
        (arg1 : VkDevice;
         arg2 : VkPerformanceParameterTypeINTEL;
         arg3 : access VkPerformanceValueINTEL) return VkResult;
   pragma Convention (C, PFN_vkGetPerformanceParameterINTEL);  -- vulkan_core.h:10483

   function vkInitializePerformanceApiINTEL (device : VkDevice; pInitializeInfo : System.Address) return VkResult;  -- vulkan_core.h:10486
   pragma Import (C, vkInitializePerformanceApiINTEL, "vkInitializePerformanceApiINTEL");

   procedure vkUninitializePerformanceApiINTEL (device : VkDevice);  -- vulkan_core.h:10490
   pragma Import (C, vkUninitializePerformanceApiINTEL, "vkUninitializePerformanceApiINTEL");

   function vkCmdSetPerformanceMarkerINTEL (commandBuffer : VkCommandBuffer; pMarkerInfo : System.Address) return VkResult;  -- vulkan_core.h:10493
   pragma Import (C, vkCmdSetPerformanceMarkerINTEL, "vkCmdSetPerformanceMarkerINTEL");

   function vkCmdSetPerformanceStreamMarkerINTEL (commandBuffer : VkCommandBuffer; pMarkerInfo : System.Address) return VkResult;  -- vulkan_core.h:10497
   pragma Import (C, vkCmdSetPerformanceStreamMarkerINTEL, "vkCmdSetPerformanceStreamMarkerINTEL");

   function vkCmdSetPerformanceOverrideINTEL (commandBuffer : VkCommandBuffer; pOverrideInfo : System.Address) return VkResult;  -- vulkan_core.h:10501
   pragma Import (C, vkCmdSetPerformanceOverrideINTEL, "vkCmdSetPerformanceOverrideINTEL");

   function vkAcquirePerformanceConfigurationINTEL
     (device : VkDevice;
      pAcquireInfo : System.Address;
      pConfiguration : System.Address) return VkResult;  -- vulkan_core.h:10505
   pragma Import (C, vkAcquirePerformanceConfigurationINTEL, "vkAcquirePerformanceConfigurationINTEL");

   function vkReleasePerformanceConfigurationINTEL (device : VkDevice; configuration : VkPerformanceConfigurationINTEL) return VkResult;  -- vulkan_core.h:10510
   pragma Import (C, vkReleasePerformanceConfigurationINTEL, "vkReleasePerformanceConfigurationINTEL");

   function vkQueueSetPerformanceConfigurationINTEL (queue : VkQueue; configuration : VkPerformanceConfigurationINTEL) return VkResult;  -- vulkan_core.h:10514
   pragma Import (C, vkQueueSetPerformanceConfigurationINTEL, "vkQueueSetPerformanceConfigurationINTEL");

   function vkGetPerformanceParameterINTEL
     (device : VkDevice;
      parameter : VkPerformanceParameterTypeINTEL;
      pValue : access VkPerformanceValueINTEL) return VkResult;  -- vulkan_core.h:10518
   pragma Import (C, vkGetPerformanceParameterINTEL, "vkGetPerformanceParameterINTEL");

   type VkPhysicalDevicePCIBusInfoPropertiesEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:10529
      pNext : System.Address;  -- vulkan_core.h:10530
      pciDomain : aliased stdint_h.uint32_t;  -- vulkan_core.h:10531
      pciBus : aliased stdint_h.uint32_t;  -- vulkan_core.h:10532
      pciDevice : aliased stdint_h.uint32_t;  -- vulkan_core.h:10533
      pciFunction : aliased stdint_h.uint32_t;  -- vulkan_core.h:10534
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDevicePCIBusInfoPropertiesEXT);  -- vulkan_core.h:10528

   type VkDisplayNativeHdrSurfaceCapabilitiesAMD is record
      sType : aliased VkStructureType;  -- vulkan_core.h:10543
      pNext : System.Address;  -- vulkan_core.h:10544
      localDimmingSupport : aliased VkBool32;  -- vulkan_core.h:10545
   end record;
   pragma Convention (C_Pass_By_Copy, VkDisplayNativeHdrSurfaceCapabilitiesAMD);  -- vulkan_core.h:10542

   type VkSwapchainDisplayNativeHdrCreateInfoAMD is record
      sType : aliased VkStructureType;  -- vulkan_core.h:10549
      pNext : System.Address;  -- vulkan_core.h:10550
      localDimmingEnable : aliased VkBool32;  -- vulkan_core.h:10551
   end record;
   pragma Convention (C_Pass_By_Copy, VkSwapchainDisplayNativeHdrCreateInfoAMD);  -- vulkan_core.h:10548

   type PFN_vkSetLocalDimmingAMD is access procedure
        (arg1 : VkDevice;
         arg2 : VkSwapchainKHR;
         arg3 : VkBool32);
   pragma Convention (C, PFN_vkSetLocalDimmingAMD);  -- vulkan_core.h:10554

   procedure vkSetLocalDimmingAMD
     (device : VkDevice;
      swapChain : VkSwapchainKHR;
      localDimmingEnable : VkBool32);  -- vulkan_core.h:10557
   pragma Import (C, vkSetLocalDimmingAMD, "vkSetLocalDimmingAMD");

   type VkPhysicalDeviceFragmentDensityMapFeaturesEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:10568
      pNext : System.Address;  -- vulkan_core.h:10569
      fragmentDensityMap : aliased VkBool32;  -- vulkan_core.h:10570
      fragmentDensityMapDynamic : aliased VkBool32;  -- vulkan_core.h:10571
      fragmentDensityMapNonSubsampledImages : aliased VkBool32;  -- vulkan_core.h:10572
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceFragmentDensityMapFeaturesEXT);  -- vulkan_core.h:10567

   type VkPhysicalDeviceFragmentDensityMapPropertiesEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:10576
      pNext : System.Address;  -- vulkan_core.h:10577
      minFragmentDensityTexelSize : aliased VkExtent2D;  -- vulkan_core.h:10578
      maxFragmentDensityTexelSize : aliased VkExtent2D;  -- vulkan_core.h:10579
      fragmentDensityInvocations : aliased VkBool32;  -- vulkan_core.h:10580
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceFragmentDensityMapPropertiesEXT);  -- vulkan_core.h:10575

   type VkRenderPassFragmentDensityMapCreateInfoEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:10584
      pNext : System.Address;  -- vulkan_core.h:10585
      fragmentDensityMapAttachment : aliased VkAttachmentReference;  -- vulkan_core.h:10586
   end record;
   pragma Convention (C_Pass_By_Copy, VkRenderPassFragmentDensityMapCreateInfoEXT);  -- vulkan_core.h:10583

   subtype VkPhysicalDeviceScalarBlockLayoutFeaturesEXT is VkPhysicalDeviceScalarBlockLayoutFeatures;

   type VkPhysicalDeviceSubgroupSizeControlFeaturesEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:10612
      pNext : System.Address;  -- vulkan_core.h:10613
      subgroupSizeControl : aliased VkBool32;  -- vulkan_core.h:10614
      computeFullSubgroups : aliased VkBool32;  -- vulkan_core.h:10615
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceSubgroupSizeControlFeaturesEXT);  -- vulkan_core.h:10611

   type VkPhysicalDeviceSubgroupSizeControlPropertiesEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:10619
      pNext : System.Address;  -- vulkan_core.h:10620
      minSubgroupSize : aliased stdint_h.uint32_t;  -- vulkan_core.h:10621
      maxSubgroupSize : aliased stdint_h.uint32_t;  -- vulkan_core.h:10622
      maxComputeWorkgroupSubgroups : aliased stdint_h.uint32_t;  -- vulkan_core.h:10623
      requiredSubgroupSizeStages : aliased VkShaderStageFlags;  -- vulkan_core.h:10624
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceSubgroupSizeControlPropertiesEXT);  -- vulkan_core.h:10618

   type VkPipelineShaderStageRequiredSubgroupSizeCreateInfoEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:10628
      pNext : System.Address;  -- vulkan_core.h:10629
      requiredSubgroupSize : aliased stdint_h.uint32_t;  -- vulkan_core.h:10630
   end record;
   pragma Convention (C_Pass_By_Copy, VkPipelineShaderStageRequiredSubgroupSizeCreateInfoEXT);  -- vulkan_core.h:10627

   subtype VkShaderCorePropertiesFlagBitsAMD is unsigned;
   VK_SHADER_CORE_PROPERTIES_FLAG_BITS_MAX_ENUM_AMD : constant VkShaderCorePropertiesFlagBitsAMD := 2147483647;  -- vulkan_core.h:10639

   subtype VkShaderCorePropertiesFlagsAMD is VkFlags;  -- vulkan_core.h:10642

   type VkPhysicalDeviceShaderCoreProperties2AMD is record
      sType : aliased VkStructureType;  -- vulkan_core.h:10644
      pNext : System.Address;  -- vulkan_core.h:10645
      shaderCoreFeatures : aliased VkShaderCorePropertiesFlagsAMD;  -- vulkan_core.h:10646
      activeComputeUnitCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:10647
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceShaderCoreProperties2AMD);  -- vulkan_core.h:10643

   type VkPhysicalDeviceCoherentMemoryFeaturesAMD is record
      sType : aliased VkStructureType;  -- vulkan_core.h:10656
      pNext : System.Address;  -- vulkan_core.h:10657
      deviceCoherentMemory : aliased VkBool32;  -- vulkan_core.h:10658
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceCoherentMemoryFeaturesAMD);  -- vulkan_core.h:10655

   type VkPhysicalDeviceShaderImageAtomicInt64FeaturesEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:10667
      pNext : System.Address;  -- vulkan_core.h:10668
      shaderImageInt64Atomics : aliased VkBool32;  -- vulkan_core.h:10669
      sparseImageInt64Atomics : aliased VkBool32;  -- vulkan_core.h:10670
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceShaderImageAtomicInt64FeaturesEXT);  -- vulkan_core.h:10666

   type VkPhysicalDeviceMemoryBudgetPropertiesEXT_heapBudget_array is array (0 .. 15) of aliased VkDeviceSize;
   type VkPhysicalDeviceMemoryBudgetPropertiesEXT_heapUsage_array is array (0 .. 15) of aliased VkDeviceSize;
   type VkPhysicalDeviceMemoryBudgetPropertiesEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:10679
      pNext : System.Address;  -- vulkan_core.h:10680
      heapBudget : aliased VkPhysicalDeviceMemoryBudgetPropertiesEXT_heapBudget_array;  -- vulkan_core.h:10681
      heapUsage : aliased VkPhysicalDeviceMemoryBudgetPropertiesEXT_heapUsage_array;  -- vulkan_core.h:10682
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceMemoryBudgetPropertiesEXT);  -- vulkan_core.h:10678

   type VkPhysicalDeviceMemoryPriorityFeaturesEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:10691
      pNext : System.Address;  -- vulkan_core.h:10692
      memoryPriority : aliased VkBool32;  -- vulkan_core.h:10693
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceMemoryPriorityFeaturesEXT);  -- vulkan_core.h:10690

   type VkMemoryPriorityAllocateInfoEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:10697
      pNext : System.Address;  -- vulkan_core.h:10698
      priority : aliased float;  -- vulkan_core.h:10699
   end record;
   pragma Convention (C_Pass_By_Copy, VkMemoryPriorityAllocateInfoEXT);  -- vulkan_core.h:10696

   type VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV is record
      sType : aliased VkStructureType;  -- vulkan_core.h:10708
      pNext : System.Address;  -- vulkan_core.h:10709
      dedicatedAllocationImageAliasing : aliased VkBool32;  -- vulkan_core.h:10710
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV);  -- vulkan_core.h:10707

   type VkPhysicalDeviceBufferDeviceAddressFeaturesEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:10719
      pNext : System.Address;  -- vulkan_core.h:10720
      bufferDeviceAddress : aliased VkBool32;  -- vulkan_core.h:10721
      bufferDeviceAddressCaptureReplay : aliased VkBool32;  -- vulkan_core.h:10722
      bufferDeviceAddressMultiDevice : aliased VkBool32;  -- vulkan_core.h:10723
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceBufferDeviceAddressFeaturesEXT);  -- vulkan_core.h:10718

   subtype VkPhysicalDeviceBufferAddressFeaturesEXT is VkPhysicalDeviceBufferDeviceAddressFeaturesEXT;

   subtype VkBufferDeviceAddressInfoEXT is VkBufferDeviceAddressInfo;

   type VkBufferDeviceAddressCreateInfoEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:10731
      pNext : System.Address;  -- vulkan_core.h:10732
      deviceAddress : aliased VkDeviceAddress;  -- vulkan_core.h:10733
   end record;
   pragma Convention (C_Pass_By_Copy, VkBufferDeviceAddressCreateInfoEXT);  -- vulkan_core.h:10730

   type PFN_vkGetBufferDeviceAddressEXT is access function (arg1 : VkDevice; arg2 : System.Address) return VkDeviceAddress;
   pragma Convention (C, PFN_vkGetBufferDeviceAddressEXT);  -- vulkan_core.h:10736

   function vkGetBufferDeviceAddressEXT (device : VkDevice; pInfo : System.Address) return VkDeviceAddress;  -- vulkan_core.h:10739
   pragma Import (C, vkGetBufferDeviceAddressEXT, "vkGetBufferDeviceAddressEXT");

   subtype VkToolPurposeFlagBitsEXT is unsigned;
   VK_TOOL_PURPOSE_VALIDATION_BIT_EXT : constant VkToolPurposeFlagBitsEXT := 1;
   VK_TOOL_PURPOSE_PROFILING_BIT_EXT : constant VkToolPurposeFlagBitsEXT := 2;
   VK_TOOL_PURPOSE_TRACING_BIT_EXT : constant VkToolPurposeFlagBitsEXT := 4;
   VK_TOOL_PURPOSE_ADDITIONAL_FEATURES_BIT_EXT : constant VkToolPurposeFlagBitsEXT := 8;
   VK_TOOL_PURPOSE_MODIFYING_FEATURES_BIT_EXT : constant VkToolPurposeFlagBitsEXT := 16;
   VK_TOOL_PURPOSE_DEBUG_REPORTING_BIT_EXT : constant VkToolPurposeFlagBitsEXT := 32;
   VK_TOOL_PURPOSE_DEBUG_MARKERS_BIT_EXT : constant VkToolPurposeFlagBitsEXT := 64;
   VK_TOOL_PURPOSE_FLAG_BITS_MAX_ENUM_EXT : constant VkToolPurposeFlagBitsEXT := 2147483647;  -- vulkan_core.h:10749

   subtype VkToolPurposeFlagsEXT is VkFlags;  -- vulkan_core.h:10759

   subtype VkPhysicalDeviceToolPropertiesEXT_name_array is Interfaces.C.char_array (0 .. 255);
   subtype VkPhysicalDeviceToolPropertiesEXT_version_array is Interfaces.C.char_array (0 .. 255);
   subtype VkPhysicalDeviceToolPropertiesEXT_description_array is Interfaces.C.char_array (0 .. 255);
   subtype VkPhysicalDeviceToolPropertiesEXT_layer_array is Interfaces.C.char_array (0 .. 255);
   type VkPhysicalDeviceToolPropertiesEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:10761
      pNext : System.Address;  -- vulkan_core.h:10762
      name : aliased VkPhysicalDeviceToolPropertiesEXT_name_array;  -- vulkan_core.h:10763
      version : aliased VkPhysicalDeviceToolPropertiesEXT_version_array;  -- vulkan_core.h:10764
      purposes : aliased VkToolPurposeFlagsEXT;  -- vulkan_core.h:10765
      description : aliased VkPhysicalDeviceToolPropertiesEXT_description_array;  -- vulkan_core.h:10766
      layer : aliased VkPhysicalDeviceToolPropertiesEXT_layer_array;  -- vulkan_core.h:10767
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceToolPropertiesEXT);  -- vulkan_core.h:10760

   type PFN_vkGetPhysicalDeviceToolPropertiesEXT is access function
        (arg1 : VkPhysicalDevice;
         arg2 : access stdint_h.uint32_t;
         arg3 : access VkPhysicalDeviceToolPropertiesEXT) return VkResult;
   pragma Convention (C, PFN_vkGetPhysicalDeviceToolPropertiesEXT);  -- vulkan_core.h:10770

   function vkGetPhysicalDeviceToolPropertiesEXT
     (physicalDevice : VkPhysicalDevice;
      pToolCount : access stdint_h.uint32_t;
      pToolProperties : access VkPhysicalDeviceToolPropertiesEXT) return VkResult;  -- vulkan_core.h:10773
   pragma Import (C, vkGetPhysicalDeviceToolPropertiesEXT, "vkGetPhysicalDeviceToolPropertiesEXT");

   subtype VkImageStencilUsageCreateInfoEXT is VkImageStencilUsageCreateInfo;

   subtype VkValidationFeatureEnableEXT is unsigned;
   VK_VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_EXT : constant VkValidationFeatureEnableEXT := 0;
   VK_VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_RESERVE_BINDING_SLOT_EXT : constant VkValidationFeatureEnableEXT := 1;
   VK_VALIDATION_FEATURE_ENABLE_BEST_PRACTICES_EXT : constant VkValidationFeatureEnableEXT := 2;
   VK_VALIDATION_FEATURE_ENABLE_DEBUG_PRINTF_EXT : constant VkValidationFeatureEnableEXT := 3;
   VK_VALIDATION_FEATURE_ENABLE_SYNCHRONIZATION_VALIDATION_EXT : constant VkValidationFeatureEnableEXT := 4;
   VK_VALIDATION_FEATURE_ENABLE_MAX_ENUM_EXT : constant VkValidationFeatureEnableEXT := 2147483647;  -- vulkan_core.h:10791

   subtype VkValidationFeatureDisableEXT is unsigned;
   VK_VALIDATION_FEATURE_DISABLE_ALL_EXT : constant VkValidationFeatureDisableEXT := 0;
   VK_VALIDATION_FEATURE_DISABLE_SHADERS_EXT : constant VkValidationFeatureDisableEXT := 1;
   VK_VALIDATION_FEATURE_DISABLE_THREAD_SAFETY_EXT : constant VkValidationFeatureDisableEXT := 2;
   VK_VALIDATION_FEATURE_DISABLE_API_PARAMETERS_EXT : constant VkValidationFeatureDisableEXT := 3;
   VK_VALIDATION_FEATURE_DISABLE_OBJECT_LIFETIMES_EXT : constant VkValidationFeatureDisableEXT := 4;
   VK_VALIDATION_FEATURE_DISABLE_CORE_CHECKS_EXT : constant VkValidationFeatureDisableEXT := 5;
   VK_VALIDATION_FEATURE_DISABLE_UNIQUE_HANDLES_EXT : constant VkValidationFeatureDisableEXT := 6;
   VK_VALIDATION_FEATURE_DISABLE_MAX_ENUM_EXT : constant VkValidationFeatureDisableEXT := 2147483647;  -- vulkan_core.h:10800

   type VkValidationFeaturesEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:10811
      pNext : System.Address;  -- vulkan_core.h:10812
      enabledValidationFeatureCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:10813
      pEnabledValidationFeatures : System.Address;  -- vulkan_core.h:10814
      disabledValidationFeatureCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:10815
      pDisabledValidationFeatures : System.Address;  -- vulkan_core.h:10816
   end record;
   pragma Convention (C_Pass_By_Copy, VkValidationFeaturesEXT);  -- vulkan_core.h:10810

   subtype VkComponentTypeNV is unsigned;
   VK_COMPONENT_TYPE_FLOAT16_NV : constant VkComponentTypeNV := 0;
   VK_COMPONENT_TYPE_FLOAT32_NV : constant VkComponentTypeNV := 1;
   VK_COMPONENT_TYPE_FLOAT64_NV : constant VkComponentTypeNV := 2;
   VK_COMPONENT_TYPE_SINT8_NV : constant VkComponentTypeNV := 3;
   VK_COMPONENT_TYPE_SINT16_NV : constant VkComponentTypeNV := 4;
   VK_COMPONENT_TYPE_SINT32_NV : constant VkComponentTypeNV := 5;
   VK_COMPONENT_TYPE_SINT64_NV : constant VkComponentTypeNV := 6;
   VK_COMPONENT_TYPE_UINT8_NV : constant VkComponentTypeNV := 7;
   VK_COMPONENT_TYPE_UINT16_NV : constant VkComponentTypeNV := 8;
   VK_COMPONENT_TYPE_UINT32_NV : constant VkComponentTypeNV := 9;
   VK_COMPONENT_TYPE_UINT64_NV : constant VkComponentTypeNV := 10;
   VK_COMPONENT_TYPE_MAX_ENUM_NV : constant VkComponentTypeNV := 2147483647;  -- vulkan_core.h:10825

   subtype VkScopeNV is unsigned;
   VK_SCOPE_DEVICE_NV : constant VkScopeNV := 1;
   VK_SCOPE_WORKGROUP_NV : constant VkScopeNV := 2;
   VK_SCOPE_SUBGROUP_NV : constant VkScopeNV := 3;
   VK_SCOPE_QUEUE_FAMILY_NV : constant VkScopeNV := 5;
   VK_SCOPE_MAX_ENUM_NV : constant VkScopeNV := 2147483647;  -- vulkan_core.h:10840

   type VkCooperativeMatrixPropertiesNV is record
      sType : aliased VkStructureType;  -- vulkan_core.h:10848
      pNext : System.Address;  -- vulkan_core.h:10849
      MSize : aliased stdint_h.uint32_t;  -- vulkan_core.h:10850
      NSize : aliased stdint_h.uint32_t;  -- vulkan_core.h:10851
      KSize : aliased stdint_h.uint32_t;  -- vulkan_core.h:10852
      AType : aliased VkComponentTypeNV;  -- vulkan_core.h:10853
      BType : aliased VkComponentTypeNV;  -- vulkan_core.h:10854
      CType : aliased VkComponentTypeNV;  -- vulkan_core.h:10855
      DType : aliased VkComponentTypeNV;  -- vulkan_core.h:10856
      scope : aliased VkScopeNV;  -- vulkan_core.h:10857
   end record;
   pragma Convention (C_Pass_By_Copy, VkCooperativeMatrixPropertiesNV);  -- vulkan_core.h:10847

   type VkPhysicalDeviceCooperativeMatrixFeaturesNV is record
      sType : aliased VkStructureType;  -- vulkan_core.h:10861
      pNext : System.Address;  -- vulkan_core.h:10862
      cooperativeMatrix : aliased VkBool32;  -- vulkan_core.h:10863
      cooperativeMatrixRobustBufferAccess : aliased VkBool32;  -- vulkan_core.h:10864
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceCooperativeMatrixFeaturesNV);  -- vulkan_core.h:10860

   type VkPhysicalDeviceCooperativeMatrixPropertiesNV is record
      sType : aliased VkStructureType;  -- vulkan_core.h:10868
      pNext : System.Address;  -- vulkan_core.h:10869
      cooperativeMatrixSupportedStages : aliased VkShaderStageFlags;  -- vulkan_core.h:10870
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceCooperativeMatrixPropertiesNV);  -- vulkan_core.h:10867

   type PFN_vkGetPhysicalDeviceCooperativeMatrixPropertiesNV is access function
        (arg1 : VkPhysicalDevice;
         arg2 : access stdint_h.uint32_t;
         arg3 : access VkCooperativeMatrixPropertiesNV) return VkResult;
   pragma Convention (C, PFN_vkGetPhysicalDeviceCooperativeMatrixPropertiesNV);  -- vulkan_core.h:10873

   function vkGetPhysicalDeviceCooperativeMatrixPropertiesNV
     (physicalDevice : VkPhysicalDevice;
      pPropertyCount : access stdint_h.uint32_t;
      pProperties : access VkCooperativeMatrixPropertiesNV) return VkResult;  -- vulkan_core.h:10876
   pragma Import (C, vkGetPhysicalDeviceCooperativeMatrixPropertiesNV, "vkGetPhysicalDeviceCooperativeMatrixPropertiesNV");

   subtype VkCoverageReductionModeNV is unsigned;
   VK_COVERAGE_REDUCTION_MODE_MERGE_NV : constant VkCoverageReductionModeNV := 0;
   VK_COVERAGE_REDUCTION_MODE_TRUNCATE_NV : constant VkCoverageReductionModeNV := 1;
   VK_COVERAGE_REDUCTION_MODE_MAX_ENUM_NV : constant VkCoverageReductionModeNV := 2147483647;  -- vulkan_core.h:10887

   subtype VkPipelineCoverageReductionStateCreateFlagsNV is VkFlags;  -- vulkan_core.h:10892

   type VkPhysicalDeviceCoverageReductionModeFeaturesNV is record
      sType : aliased VkStructureType;  -- vulkan_core.h:10894
      pNext : System.Address;  -- vulkan_core.h:10895
      coverageReductionMode : aliased VkBool32;  -- vulkan_core.h:10896
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceCoverageReductionModeFeaturesNV);  -- vulkan_core.h:10893

   type VkPipelineCoverageReductionStateCreateInfoNV is record
      sType : aliased VkStructureType;  -- vulkan_core.h:10900
      pNext : System.Address;  -- vulkan_core.h:10901
      flags : aliased VkPipelineCoverageReductionStateCreateFlagsNV;  -- vulkan_core.h:10902
      coverageReductionMode : aliased VkCoverageReductionModeNV;  -- vulkan_core.h:10903
   end record;
   pragma Convention (C_Pass_By_Copy, VkPipelineCoverageReductionStateCreateInfoNV);  -- vulkan_core.h:10899

   type VkFramebufferMixedSamplesCombinationNV is record
      sType : aliased VkStructureType;  -- vulkan_core.h:10907
      pNext : System.Address;  -- vulkan_core.h:10908
      coverageReductionMode : aliased VkCoverageReductionModeNV;  -- vulkan_core.h:10909
      rasterizationSamples : aliased VkSampleCountFlagBits;  -- vulkan_core.h:10910
      depthStencilSamples : aliased VkSampleCountFlags;  -- vulkan_core.h:10911
      colorSamples : aliased VkSampleCountFlags;  -- vulkan_core.h:10912
   end record;
   pragma Convention (C_Pass_By_Copy, VkFramebufferMixedSamplesCombinationNV);  -- vulkan_core.h:10906

   type PFN_vkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV is access function
        (arg1 : VkPhysicalDevice;
         arg2 : access stdint_h.uint32_t;
         arg3 : access VkFramebufferMixedSamplesCombinationNV) return VkResult;
   pragma Convention (C, PFN_vkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV);  -- vulkan_core.h:10915

   function vkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV
     (physicalDevice : VkPhysicalDevice;
      pCombinationCount : access stdint_h.uint32_t;
      pCombinations : access VkFramebufferMixedSamplesCombinationNV) return VkResult;  -- vulkan_core.h:10918
   pragma Import (C, vkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV, "vkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV");

   type VkPhysicalDeviceFragmentShaderInterlockFeaturesEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:10929
      pNext : System.Address;  -- vulkan_core.h:10930
      fragmentShaderSampleInterlock : aliased VkBool32;  -- vulkan_core.h:10931
      fragmentShaderPixelInterlock : aliased VkBool32;  -- vulkan_core.h:10932
      fragmentShaderShadingRateInterlock : aliased VkBool32;  -- vulkan_core.h:10933
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceFragmentShaderInterlockFeaturesEXT);  -- vulkan_core.h:10928

   type VkPhysicalDeviceYcbcrImageArraysFeaturesEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:10942
      pNext : System.Address;  -- vulkan_core.h:10943
      ycbcrImageArrays : aliased VkBool32;  -- vulkan_core.h:10944
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceYcbcrImageArraysFeaturesEXT);  -- vulkan_core.h:10941

   subtype VkHeadlessSurfaceCreateFlagsEXT is VkFlags;  -- vulkan_core.h:10952

   type VkHeadlessSurfaceCreateInfoEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:10954
      pNext : System.Address;  -- vulkan_core.h:10955
      flags : aliased VkHeadlessSurfaceCreateFlagsEXT;  -- vulkan_core.h:10956
   end record;
   pragma Convention (C_Pass_By_Copy, VkHeadlessSurfaceCreateInfoEXT);  -- vulkan_core.h:10953

   type PFN_vkCreateHeadlessSurfaceEXT is access function
        (arg1 : VkInstance;
         arg2 : System.Address;
         arg3 : System.Address;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateHeadlessSurfaceEXT);  -- vulkan_core.h:10959

   function vkCreateHeadlessSurfaceEXT
     (instance : VkInstance;
      pCreateInfo : System.Address;
      pAllocator : System.Address;
      pSurface : System.Address) return VkResult;  -- vulkan_core.h:10962
   pragma Import (C, vkCreateHeadlessSurfaceEXT, "vkCreateHeadlessSurfaceEXT");

   subtype VkLineRasterizationModeEXT is unsigned;
   VK_LINE_RASTERIZATION_MODE_DEFAULT_EXT : constant VkLineRasterizationModeEXT := 0;
   VK_LINE_RASTERIZATION_MODE_RECTANGULAR_EXT : constant VkLineRasterizationModeEXT := 1;
   VK_LINE_RASTERIZATION_MODE_BRESENHAM_EXT : constant VkLineRasterizationModeEXT := 2;
   VK_LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH_EXT : constant VkLineRasterizationModeEXT := 3;
   VK_LINE_RASTERIZATION_MODE_MAX_ENUM_EXT : constant VkLineRasterizationModeEXT := 2147483647;  -- vulkan_core.h:10974

   type VkPhysicalDeviceLineRasterizationFeaturesEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:10982
      pNext : System.Address;  -- vulkan_core.h:10983
      rectangularLines : aliased VkBool32;  -- vulkan_core.h:10984
      bresenhamLines : aliased VkBool32;  -- vulkan_core.h:10985
      smoothLines : aliased VkBool32;  -- vulkan_core.h:10986
      stippledRectangularLines : aliased VkBool32;  -- vulkan_core.h:10987
      stippledBresenhamLines : aliased VkBool32;  -- vulkan_core.h:10988
      stippledSmoothLines : aliased VkBool32;  -- vulkan_core.h:10989
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceLineRasterizationFeaturesEXT);  -- vulkan_core.h:10981

   type VkPhysicalDeviceLineRasterizationPropertiesEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:10993
      pNext : System.Address;  -- vulkan_core.h:10994
      lineSubPixelPrecisionBits : aliased stdint_h.uint32_t;  -- vulkan_core.h:10995
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceLineRasterizationPropertiesEXT);  -- vulkan_core.h:10992

   type VkPipelineRasterizationLineStateCreateInfoEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:10999
      pNext : System.Address;  -- vulkan_core.h:11000
      lineRasterizationMode : aliased VkLineRasterizationModeEXT;  -- vulkan_core.h:11001
      stippledLineEnable : aliased VkBool32;  -- vulkan_core.h:11002
      lineStippleFactor : aliased stdint_h.uint32_t;  -- vulkan_core.h:11003
      lineStipplePattern : aliased stdint_h.uint16_t;  -- vulkan_core.h:11004
   end record;
   pragma Convention (C_Pass_By_Copy, VkPipelineRasterizationLineStateCreateInfoEXT);  -- vulkan_core.h:10998

   type PFN_vkCmdSetLineStippleEXT is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : stdint_h.uint32_t;
         arg3 : stdint_h.uint16_t);
   pragma Convention (C, PFN_vkCmdSetLineStippleEXT);  -- vulkan_core.h:11007

   procedure vkCmdSetLineStippleEXT
     (commandBuffer : VkCommandBuffer;
      lineStippleFactor : stdint_h.uint32_t;
      lineStipplePattern : stdint_h.uint16_t);  -- vulkan_core.h:11010
   pragma Import (C, vkCmdSetLineStippleEXT, "vkCmdSetLineStippleEXT");

   type VkPhysicalDeviceShaderAtomicFloatFeaturesEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:11021
      pNext : System.Address;  -- vulkan_core.h:11022
      shaderBufferFloat32Atomics : aliased VkBool32;  -- vulkan_core.h:11023
      shaderBufferFloat32AtomicAdd : aliased VkBool32;  -- vulkan_core.h:11024
      shaderBufferFloat64Atomics : aliased VkBool32;  -- vulkan_core.h:11025
      shaderBufferFloat64AtomicAdd : aliased VkBool32;  -- vulkan_core.h:11026
      shaderSharedFloat32Atomics : aliased VkBool32;  -- vulkan_core.h:11027
      shaderSharedFloat32AtomicAdd : aliased VkBool32;  -- vulkan_core.h:11028
      shaderSharedFloat64Atomics : aliased VkBool32;  -- vulkan_core.h:11029
      shaderSharedFloat64AtomicAdd : aliased VkBool32;  -- vulkan_core.h:11030
      shaderImageFloat32Atomics : aliased VkBool32;  -- vulkan_core.h:11031
      shaderImageFloat32AtomicAdd : aliased VkBool32;  -- vulkan_core.h:11032
      sparseImageFloat32Atomics : aliased VkBool32;  -- vulkan_core.h:11033
      sparseImageFloat32AtomicAdd : aliased VkBool32;  -- vulkan_core.h:11034
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceShaderAtomicFloatFeaturesEXT);  -- vulkan_core.h:11020

   subtype VkPhysicalDeviceHostQueryResetFeaturesEXT is VkPhysicalDeviceHostQueryResetFeatures;

   type PFN_vkResetQueryPoolEXT is access procedure
        (arg1 : VkDevice;
         arg2 : VkQueryPool;
         arg3 : stdint_h.uint32_t;
         arg4 : stdint_h.uint32_t);
   pragma Convention (C, PFN_vkResetQueryPoolEXT);  -- vulkan_core.h:11044

   procedure vkResetQueryPoolEXT
     (device : VkDevice;
      queryPool : VkQueryPool;
      firstQuery : stdint_h.uint32_t;
      queryCount : stdint_h.uint32_t);  -- vulkan_core.h:11047
   pragma Import (C, vkResetQueryPoolEXT, "vkResetQueryPoolEXT");

   type VkPhysicalDeviceIndexTypeUint8FeaturesEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:11059
      pNext : System.Address;  -- vulkan_core.h:11060
      indexTypeUint8 : aliased VkBool32;  -- vulkan_core.h:11061
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceIndexTypeUint8FeaturesEXT);  -- vulkan_core.h:11058

   type VkPhysicalDeviceExtendedDynamicStateFeaturesEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:11070
      pNext : System.Address;  -- vulkan_core.h:11071
      extendedDynamicState : aliased VkBool32;  -- vulkan_core.h:11072
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceExtendedDynamicStateFeaturesEXT);  -- vulkan_core.h:11069

   type PFN_vkCmdSetCullModeEXT is access procedure (arg1 : VkCommandBuffer; arg2 : VkCullModeFlags);
   pragma Convention (C, PFN_vkCmdSetCullModeEXT);  -- vulkan_core.h:11075

   type PFN_vkCmdSetFrontFaceEXT is access procedure (arg1 : VkCommandBuffer; arg2 : VkFrontFace);
   pragma Convention (C, PFN_vkCmdSetFrontFaceEXT);  -- vulkan_core.h:11076

   type PFN_vkCmdSetPrimitiveTopologyEXT is access procedure (arg1 : VkCommandBuffer; arg2 : VkPrimitiveTopology);
   pragma Convention (C, PFN_vkCmdSetPrimitiveTopologyEXT);  -- vulkan_core.h:11077

   type PFN_vkCmdSetViewportWithCountEXT is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : stdint_h.uint32_t;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkCmdSetViewportWithCountEXT);  -- vulkan_core.h:11078

   type PFN_vkCmdSetScissorWithCountEXT is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : stdint_h.uint32_t;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkCmdSetScissorWithCountEXT);  -- vulkan_core.h:11079

   type PFN_vkCmdBindVertexBuffers2EXT is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : stdint_h.uint32_t;
         arg3 : stdint_h.uint32_t;
         arg4 : System.Address;
         arg5 : access VkDeviceSize;
         arg6 : access VkDeviceSize;
         arg7 : access VkDeviceSize);
   pragma Convention (C, PFN_vkCmdBindVertexBuffers2EXT);  -- vulkan_core.h:11080

   type PFN_vkCmdSetDepthTestEnableEXT is access procedure (arg1 : VkCommandBuffer; arg2 : VkBool32);
   pragma Convention (C, PFN_vkCmdSetDepthTestEnableEXT);  -- vulkan_core.h:11081

   type PFN_vkCmdSetDepthWriteEnableEXT is access procedure (arg1 : VkCommandBuffer; arg2 : VkBool32);
   pragma Convention (C, PFN_vkCmdSetDepthWriteEnableEXT);  -- vulkan_core.h:11082

   type PFN_vkCmdSetDepthCompareOpEXT is access procedure (arg1 : VkCommandBuffer; arg2 : VkCompareOp);
   pragma Convention (C, PFN_vkCmdSetDepthCompareOpEXT);  -- vulkan_core.h:11083

   type PFN_vkCmdSetDepthBoundsTestEnableEXT is access procedure (arg1 : VkCommandBuffer; arg2 : VkBool32);
   pragma Convention (C, PFN_vkCmdSetDepthBoundsTestEnableEXT);  -- vulkan_core.h:11084

   type PFN_vkCmdSetStencilTestEnableEXT is access procedure (arg1 : VkCommandBuffer; arg2 : VkBool32);
   pragma Convention (C, PFN_vkCmdSetStencilTestEnableEXT);  -- vulkan_core.h:11085

   type PFN_vkCmdSetStencilOpEXT is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkStencilFaceFlags;
         arg3 : VkStencilOp;
         arg4 : VkStencilOp;
         arg5 : VkStencilOp;
         arg6 : VkCompareOp);
   pragma Convention (C, PFN_vkCmdSetStencilOpEXT);  -- vulkan_core.h:11086

   procedure vkCmdSetCullModeEXT (commandBuffer : VkCommandBuffer; cullMode : VkCullModeFlags);  -- vulkan_core.h:11089
   pragma Import (C, vkCmdSetCullModeEXT, "vkCmdSetCullModeEXT");

   procedure vkCmdSetFrontFaceEXT (commandBuffer : VkCommandBuffer; frontFace : VkFrontFace);  -- vulkan_core.h:11093
   pragma Import (C, vkCmdSetFrontFaceEXT, "vkCmdSetFrontFaceEXT");

   procedure vkCmdSetPrimitiveTopologyEXT (commandBuffer : VkCommandBuffer; primitiveTopology : VkPrimitiveTopology);  -- vulkan_core.h:11097
   pragma Import (C, vkCmdSetPrimitiveTopologyEXT, "vkCmdSetPrimitiveTopologyEXT");

   procedure vkCmdSetViewportWithCountEXT
     (commandBuffer : VkCommandBuffer;
      viewportCount : stdint_h.uint32_t;
      pViewports : System.Address);  -- vulkan_core.h:11101
   pragma Import (C, vkCmdSetViewportWithCountEXT, "vkCmdSetViewportWithCountEXT");

   procedure vkCmdSetScissorWithCountEXT
     (commandBuffer : VkCommandBuffer;
      scissorCount : stdint_h.uint32_t;
      pScissors : System.Address);  -- vulkan_core.h:11106
   pragma Import (C, vkCmdSetScissorWithCountEXT, "vkCmdSetScissorWithCountEXT");

   procedure vkCmdBindVertexBuffers2EXT
     (commandBuffer : VkCommandBuffer;
      firstBinding : stdint_h.uint32_t;
      bindingCount : stdint_h.uint32_t;
      pBuffers : System.Address;
      pOffsets : access VkDeviceSize;
      pSizes : access VkDeviceSize;
      pStrides : access VkDeviceSize);  -- vulkan_core.h:11111
   pragma Import (C, vkCmdBindVertexBuffers2EXT, "vkCmdBindVertexBuffers2EXT");

   procedure vkCmdSetDepthTestEnableEXT (commandBuffer : VkCommandBuffer; depthTestEnable : VkBool32);  -- vulkan_core.h:11120
   pragma Import (C, vkCmdSetDepthTestEnableEXT, "vkCmdSetDepthTestEnableEXT");

   procedure vkCmdSetDepthWriteEnableEXT (commandBuffer : VkCommandBuffer; depthWriteEnable : VkBool32);  -- vulkan_core.h:11124
   pragma Import (C, vkCmdSetDepthWriteEnableEXT, "vkCmdSetDepthWriteEnableEXT");

   procedure vkCmdSetDepthCompareOpEXT (commandBuffer : VkCommandBuffer; depthCompareOp : VkCompareOp);  -- vulkan_core.h:11128
   pragma Import (C, vkCmdSetDepthCompareOpEXT, "vkCmdSetDepthCompareOpEXT");

   procedure vkCmdSetDepthBoundsTestEnableEXT (commandBuffer : VkCommandBuffer; depthBoundsTestEnable : VkBool32);  -- vulkan_core.h:11132
   pragma Import (C, vkCmdSetDepthBoundsTestEnableEXT, "vkCmdSetDepthBoundsTestEnableEXT");

   procedure vkCmdSetStencilTestEnableEXT (commandBuffer : VkCommandBuffer; stencilTestEnable : VkBool32);  -- vulkan_core.h:11136
   pragma Import (C, vkCmdSetStencilTestEnableEXT, "vkCmdSetStencilTestEnableEXT");

   procedure vkCmdSetStencilOpEXT
     (commandBuffer : VkCommandBuffer;
      faceMask : VkStencilFaceFlags;
      failOp : VkStencilOp;
      passOp : VkStencilOp;
      depthFailOp : VkStencilOp;
      compareOp : VkCompareOp);  -- vulkan_core.h:11140
   pragma Import (C, vkCmdSetStencilOpEXT, "vkCmdSetStencilOpEXT");

   type VkPhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:11154
      pNext : System.Address;  -- vulkan_core.h:11155
      shaderDemoteToHelperInvocation : aliased VkBool32;  -- vulkan_core.h:11156
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT);  -- vulkan_core.h:11153

   type VkIndirectCommandsLayoutNV is new System.Address;  -- vulkan_core.h:11162

   --  skipped empty struct VkIndirectCommandsLayoutNV_T

   subtype VkIndirectCommandsTokenTypeNV is unsigned;
   VK_INDIRECT_COMMANDS_TOKEN_TYPE_SHADER_GROUP_NV : constant VkIndirectCommandsTokenTypeNV := 0;
   VK_INDIRECT_COMMANDS_TOKEN_TYPE_STATE_FLAGS_NV : constant VkIndirectCommandsTokenTypeNV := 1;
   VK_INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NV : constant VkIndirectCommandsTokenTypeNV := 2;
   VK_INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NV : constant VkIndirectCommandsTokenTypeNV := 3;
   VK_INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NV : constant VkIndirectCommandsTokenTypeNV := 4;
   VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NV : constant VkIndirectCommandsTokenTypeNV := 5;
   VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NV : constant VkIndirectCommandsTokenTypeNV := 6;
   VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_TASKS_NV : constant VkIndirectCommandsTokenTypeNV := 7;
   VK_INDIRECT_COMMANDS_TOKEN_TYPE_MAX_ENUM_NV : constant VkIndirectCommandsTokenTypeNV := 2147483647;  -- vulkan_core.h:11166

   subtype VkIndirectStateFlagBitsNV is unsigned;
   VK_INDIRECT_STATE_FLAG_FRONTFACE_BIT_NV : constant VkIndirectStateFlagBitsNV := 1;
   VK_INDIRECT_STATE_FLAG_BITS_MAX_ENUM_NV : constant VkIndirectStateFlagBitsNV := 2147483647;  -- vulkan_core.h:11178

   subtype VkIndirectStateFlagsNV is VkFlags;  -- vulkan_core.h:11182

   subtype VkIndirectCommandsLayoutUsageFlagBitsNV is unsigned;
   VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EXPLICIT_PREPROCESS_BIT_NV : constant VkIndirectCommandsLayoutUsageFlagBitsNV := 1;
   VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NV : constant VkIndirectCommandsLayoutUsageFlagBitsNV := 2;
   VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NV : constant VkIndirectCommandsLayoutUsageFlagBitsNV := 4;
   VK_INDIRECT_COMMANDS_LAYOUT_USAGE_FLAG_BITS_MAX_ENUM_NV : constant VkIndirectCommandsLayoutUsageFlagBitsNV := 2147483647;  -- vulkan_core.h:11184

   subtype VkIndirectCommandsLayoutUsageFlagsNV is VkFlags;  -- vulkan_core.h:11190

   type VkPhysicalDeviceDeviceGeneratedCommandsPropertiesNV is record
      sType : aliased VkStructureType;  -- vulkan_core.h:11192
      pNext : System.Address;  -- vulkan_core.h:11193
      maxGraphicsShaderGroupCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:11194
      maxIndirectSequenceCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:11195
      maxIndirectCommandsTokenCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:11196
      maxIndirectCommandsStreamCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:11197
      maxIndirectCommandsTokenOffset : aliased stdint_h.uint32_t;  -- vulkan_core.h:11198
      maxIndirectCommandsStreamStride : aliased stdint_h.uint32_t;  -- vulkan_core.h:11199
      minSequencesCountBufferOffsetAlignment : aliased stdint_h.uint32_t;  -- vulkan_core.h:11200
      minSequencesIndexBufferOffsetAlignment : aliased stdint_h.uint32_t;  -- vulkan_core.h:11201
      minIndirectCommandsBufferOffsetAlignment : aliased stdint_h.uint32_t;  -- vulkan_core.h:11202
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceDeviceGeneratedCommandsPropertiesNV);  -- vulkan_core.h:11191

   type VkPhysicalDeviceDeviceGeneratedCommandsFeaturesNV is record
      sType : aliased VkStructureType;  -- vulkan_core.h:11206
      pNext : System.Address;  -- vulkan_core.h:11207
      deviceGeneratedCommands : aliased VkBool32;  -- vulkan_core.h:11208
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceDeviceGeneratedCommandsFeaturesNV);  -- vulkan_core.h:11205

   type VkGraphicsShaderGroupCreateInfoNV is record
      sType : aliased VkStructureType;  -- vulkan_core.h:11212
      pNext : System.Address;  -- vulkan_core.h:11213
      stageCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:11214
      pStages : System.Address;  -- vulkan_core.h:11215
      pVertexInputState : System.Address;  -- vulkan_core.h:11216
      pTessellationState : System.Address;  -- vulkan_core.h:11217
   end record;
   pragma Convention (C_Pass_By_Copy, VkGraphicsShaderGroupCreateInfoNV);  -- vulkan_core.h:11211

   type VkGraphicsPipelineShaderGroupsCreateInfoNV is record
      sType : aliased VkStructureType;  -- vulkan_core.h:11221
      pNext : System.Address;  -- vulkan_core.h:11222
      groupCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:11223
      pGroups : System.Address;  -- vulkan_core.h:11224
      pipelineCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:11225
      pPipelines : System.Address;  -- vulkan_core.h:11226
   end record;
   pragma Convention (C_Pass_By_Copy, VkGraphicsPipelineShaderGroupsCreateInfoNV);  -- vulkan_core.h:11220

   type VkBindShaderGroupIndirectCommandNV is record
      groupIndex : aliased stdint_h.uint32_t;  -- vulkan_core.h:11230
   end record;
   pragma Convention (C_Pass_By_Copy, VkBindShaderGroupIndirectCommandNV);  -- vulkan_core.h:11229

   type VkBindIndexBufferIndirectCommandNV is record
      bufferAddress : aliased VkDeviceAddress;  -- vulkan_core.h:11234
      size : aliased stdint_h.uint32_t;  -- vulkan_core.h:11235
      indexType : aliased VkIndexType;  -- vulkan_core.h:11236
   end record;
   pragma Convention (C_Pass_By_Copy, VkBindIndexBufferIndirectCommandNV);  -- vulkan_core.h:11233

   type VkBindVertexBufferIndirectCommandNV is record
      bufferAddress : aliased VkDeviceAddress;  -- vulkan_core.h:11240
      size : aliased stdint_h.uint32_t;  -- vulkan_core.h:11241
      stride : aliased stdint_h.uint32_t;  -- vulkan_core.h:11242
   end record;
   pragma Convention (C_Pass_By_Copy, VkBindVertexBufferIndirectCommandNV);  -- vulkan_core.h:11239

   type VkSetStateFlagsIndirectCommandNV is record
      data : aliased stdint_h.uint32_t;  -- vulkan_core.h:11246
   end record;
   pragma Convention (C_Pass_By_Copy, VkSetStateFlagsIndirectCommandNV);  -- vulkan_core.h:11245

   type VkIndirectCommandsStreamNV is record
      buffer : VkBuffer;  -- vulkan_core.h:11250
      offset : aliased VkDeviceSize;  -- vulkan_core.h:11251
   end record;
   pragma Convention (C_Pass_By_Copy, VkIndirectCommandsStreamNV);  -- vulkan_core.h:11249

   type VkIndirectCommandsLayoutTokenNV is record
      sType : aliased VkStructureType;  -- vulkan_core.h:11255
      pNext : System.Address;  -- vulkan_core.h:11256
      tokenType : aliased VkIndirectCommandsTokenTypeNV;  -- vulkan_core.h:11257
      stream : aliased stdint_h.uint32_t;  -- vulkan_core.h:11258
      offset : aliased stdint_h.uint32_t;  -- vulkan_core.h:11259
      vertexBindingUnit : aliased stdint_h.uint32_t;  -- vulkan_core.h:11260
      vertexDynamicStride : aliased VkBool32;  -- vulkan_core.h:11261
      pushconstantPipelineLayout : VkPipelineLayout;  -- vulkan_core.h:11262
      pushconstantShaderStageFlags : aliased VkShaderStageFlags;  -- vulkan_core.h:11263
      pushconstantOffset : aliased stdint_h.uint32_t;  -- vulkan_core.h:11264
      pushconstantSize : aliased stdint_h.uint32_t;  -- vulkan_core.h:11265
      indirectStateFlags : aliased VkIndirectStateFlagsNV;  -- vulkan_core.h:11266
      indexTypeCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:11267
      pIndexTypes : System.Address;  -- vulkan_core.h:11268
      pIndexTypeValues : access stdint_h.uint32_t;  -- vulkan_core.h:11269
   end record;
   pragma Convention (C_Pass_By_Copy, VkIndirectCommandsLayoutTokenNV);  -- vulkan_core.h:11254

   type VkIndirectCommandsLayoutCreateInfoNV is record
      sType : aliased VkStructureType;  -- vulkan_core.h:11273
      pNext : System.Address;  -- vulkan_core.h:11274
      flags : aliased VkIndirectCommandsLayoutUsageFlagsNV;  -- vulkan_core.h:11275
      pipelineBindPoint : aliased VkPipelineBindPoint;  -- vulkan_core.h:11276
      tokenCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:11277
      pTokens : System.Address;  -- vulkan_core.h:11278
      streamCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:11279
      pStreamStrides : access stdint_h.uint32_t;  -- vulkan_core.h:11280
   end record;
   pragma Convention (C_Pass_By_Copy, VkIndirectCommandsLayoutCreateInfoNV);  -- vulkan_core.h:11272

   type VkGeneratedCommandsInfoNV is record
      sType : aliased VkStructureType;  -- vulkan_core.h:11284
      pNext : System.Address;  -- vulkan_core.h:11285
      pipelineBindPoint : aliased VkPipelineBindPoint;  -- vulkan_core.h:11286
      pipeline : VkPipeline;  -- vulkan_core.h:11287
      indirectCommandsLayout : VkIndirectCommandsLayoutNV;  -- vulkan_core.h:11288
      streamCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:11289
      pStreams : System.Address;  -- vulkan_core.h:11290
      sequencesCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:11291
      preprocessBuffer : VkBuffer;  -- vulkan_core.h:11292
      preprocessOffset : aliased VkDeviceSize;  -- vulkan_core.h:11293
      preprocessSize : aliased VkDeviceSize;  -- vulkan_core.h:11294
      sequencesCountBuffer : VkBuffer;  -- vulkan_core.h:11295
      sequencesCountOffset : aliased VkDeviceSize;  -- vulkan_core.h:11296
      sequencesIndexBuffer : VkBuffer;  -- vulkan_core.h:11297
      sequencesIndexOffset : aliased VkDeviceSize;  -- vulkan_core.h:11298
   end record;
   pragma Convention (C_Pass_By_Copy, VkGeneratedCommandsInfoNV);  -- vulkan_core.h:11283

   type VkGeneratedCommandsMemoryRequirementsInfoNV is record
      sType : aliased VkStructureType;  -- vulkan_core.h:11302
      pNext : System.Address;  -- vulkan_core.h:11303
      pipelineBindPoint : aliased VkPipelineBindPoint;  -- vulkan_core.h:11304
      pipeline : VkPipeline;  -- vulkan_core.h:11305
      indirectCommandsLayout : VkIndirectCommandsLayoutNV;  -- vulkan_core.h:11306
      maxSequencesCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:11307
   end record;
   pragma Convention (C_Pass_By_Copy, VkGeneratedCommandsMemoryRequirementsInfoNV);  -- vulkan_core.h:11301

   type PFN_vkGetGeneratedCommandsMemoryRequirementsNV is access procedure
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : access VkMemoryRequirements2);
   pragma Convention (C, PFN_vkGetGeneratedCommandsMemoryRequirementsNV);  -- vulkan_core.h:11310

   type PFN_vkCmdPreprocessGeneratedCommandsNV is access procedure (arg1 : VkCommandBuffer; arg2 : System.Address);
   pragma Convention (C, PFN_vkCmdPreprocessGeneratedCommandsNV);  -- vulkan_core.h:11311

   type PFN_vkCmdExecuteGeneratedCommandsNV is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkBool32;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkCmdExecuteGeneratedCommandsNV);  -- vulkan_core.h:11312

   type PFN_vkCmdBindPipelineShaderGroupNV is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkPipelineBindPoint;
         arg3 : VkPipeline;
         arg4 : stdint_h.uint32_t);
   pragma Convention (C, PFN_vkCmdBindPipelineShaderGroupNV);  -- vulkan_core.h:11313

   type PFN_vkCreateIndirectCommandsLayoutNV is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : System.Address;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateIndirectCommandsLayoutNV);  -- vulkan_core.h:11314

   type PFN_vkDestroyIndirectCommandsLayoutNV is access procedure
        (arg1 : VkDevice;
         arg2 : VkIndirectCommandsLayoutNV;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkDestroyIndirectCommandsLayoutNV);  -- vulkan_core.h:11315

   procedure vkGetGeneratedCommandsMemoryRequirementsNV
     (device : VkDevice;
      pInfo : System.Address;
      pMemoryRequirements : access VkMemoryRequirements2);  -- vulkan_core.h:11318
   pragma Import (C, vkGetGeneratedCommandsMemoryRequirementsNV, "vkGetGeneratedCommandsMemoryRequirementsNV");

   procedure vkCmdPreprocessGeneratedCommandsNV (commandBuffer : VkCommandBuffer; pGeneratedCommandsInfo : System.Address);  -- vulkan_core.h:11323
   pragma Import (C, vkCmdPreprocessGeneratedCommandsNV, "vkCmdPreprocessGeneratedCommandsNV");

   procedure vkCmdExecuteGeneratedCommandsNV
     (commandBuffer : VkCommandBuffer;
      isPreprocessed : VkBool32;
      pGeneratedCommandsInfo : System.Address);  -- vulkan_core.h:11327
   pragma Import (C, vkCmdExecuteGeneratedCommandsNV, "vkCmdExecuteGeneratedCommandsNV");

   procedure vkCmdBindPipelineShaderGroupNV
     (commandBuffer : VkCommandBuffer;
      pipelineBindPoint : VkPipelineBindPoint;
      pipeline : VkPipeline;
      groupIndex : stdint_h.uint32_t);  -- vulkan_core.h:11332
   pragma Import (C, vkCmdBindPipelineShaderGroupNV, "vkCmdBindPipelineShaderGroupNV");

   function vkCreateIndirectCommandsLayoutNV
     (device : VkDevice;
      pCreateInfo : System.Address;
      pAllocator : System.Address;
      pIndirectCommandsLayout : System.Address) return VkResult;  -- vulkan_core.h:11338
   pragma Import (C, vkCreateIndirectCommandsLayoutNV, "vkCreateIndirectCommandsLayoutNV");

   procedure vkDestroyIndirectCommandsLayoutNV
     (device : VkDevice;
      indirectCommandsLayout : VkIndirectCommandsLayoutNV;
      pAllocator : System.Address);  -- vulkan_core.h:11344
   pragma Import (C, vkDestroyIndirectCommandsLayoutNV, "vkDestroyIndirectCommandsLayoutNV");

   type VkPhysicalDeviceTexelBufferAlignmentFeaturesEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:11355
      pNext : System.Address;  -- vulkan_core.h:11356
      texelBufferAlignment : aliased VkBool32;  -- vulkan_core.h:11357
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceTexelBufferAlignmentFeaturesEXT);  -- vulkan_core.h:11354

   type VkPhysicalDeviceTexelBufferAlignmentPropertiesEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:11361
      pNext : System.Address;  -- vulkan_core.h:11362
      storageTexelBufferOffsetAlignmentBytes : aliased VkDeviceSize;  -- vulkan_core.h:11363
      storageTexelBufferOffsetSingleTexelAlignment : aliased VkBool32;  -- vulkan_core.h:11364
      uniformTexelBufferOffsetAlignmentBytes : aliased VkDeviceSize;  -- vulkan_core.h:11365
      uniformTexelBufferOffsetSingleTexelAlignment : aliased VkBool32;  -- vulkan_core.h:11366
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceTexelBufferAlignmentPropertiesEXT);  -- vulkan_core.h:11360

   type VkRenderPassTransformBeginInfoQCOM is record
      sType : aliased VkStructureType;  -- vulkan_core.h:11375
      pNext : System.Address;  -- vulkan_core.h:11376
      transform : aliased VkSurfaceTransformFlagBitsKHR;  -- vulkan_core.h:11377
   end record;
   pragma Convention (C_Pass_By_Copy, VkRenderPassTransformBeginInfoQCOM);  -- vulkan_core.h:11374

   type VkCommandBufferInheritanceRenderPassTransformInfoQCOM is record
      sType : aliased VkStructureType;  -- vulkan_core.h:11381
      pNext : System.Address;  -- vulkan_core.h:11382
      transform : aliased VkSurfaceTransformFlagBitsKHR;  -- vulkan_core.h:11383
      renderArea : aliased VkRect2D;  -- vulkan_core.h:11384
   end record;
   pragma Convention (C_Pass_By_Copy, VkCommandBufferInheritanceRenderPassTransformInfoQCOM);  -- vulkan_core.h:11380

   subtype VkDeviceMemoryReportEventTypeEXT is unsigned;
   VK_DEVICE_MEMORY_REPORT_EVENT_TYPE_ALLOCATE_EXT : constant VkDeviceMemoryReportEventTypeEXT := 0;
   VK_DEVICE_MEMORY_REPORT_EVENT_TYPE_FREE_EXT : constant VkDeviceMemoryReportEventTypeEXT := 1;
   VK_DEVICE_MEMORY_REPORT_EVENT_TYPE_IMPORT_EXT : constant VkDeviceMemoryReportEventTypeEXT := 2;
   VK_DEVICE_MEMORY_REPORT_EVENT_TYPE_UNIMPORT_EXT : constant VkDeviceMemoryReportEventTypeEXT := 3;
   VK_DEVICE_MEMORY_REPORT_EVENT_TYPE_ALLOCATION_FAILED_EXT : constant VkDeviceMemoryReportEventTypeEXT := 4;
   VK_DEVICE_MEMORY_REPORT_EVENT_TYPE_MAX_ENUM_EXT : constant VkDeviceMemoryReportEventTypeEXT := 2147483647;  -- vulkan_core.h:11393

   subtype VkDeviceMemoryReportFlagsEXT is VkFlags;  -- vulkan_core.h:11401

   type VkPhysicalDeviceDeviceMemoryReportFeaturesEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:11403
      pNext : System.Address;  -- vulkan_core.h:11404
      deviceMemoryReport : aliased VkBool32;  -- vulkan_core.h:11405
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceDeviceMemoryReportFeaturesEXT);  -- vulkan_core.h:11402

   type VkDeviceMemoryReportCallbackDataEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:11409
      pNext : System.Address;  -- vulkan_core.h:11410
      flags : aliased VkDeviceMemoryReportFlagsEXT;  -- vulkan_core.h:11411
      c_type : aliased VkDeviceMemoryReportEventTypeEXT;  -- vulkan_core.h:11412
      memoryObjectId : aliased stdint_h.uint64_t;  -- vulkan_core.h:11413
      size : aliased VkDeviceSize;  -- vulkan_core.h:11414
      objectType : aliased VkObjectType;  -- vulkan_core.h:11415
      objectHandle : aliased stdint_h.uint64_t;  -- vulkan_core.h:11416
      heapIndex : aliased stdint_h.uint32_t;  -- vulkan_core.h:11417
   end record;
   pragma Convention (C_Pass_By_Copy, VkDeviceMemoryReportCallbackDataEXT);  -- vulkan_core.h:11408

   type PFN_vkDeviceMemoryReportCallbackEXT is access procedure (arg1 : System.Address; arg2 : System.Address);
   pragma Convention (C, PFN_vkDeviceMemoryReportCallbackEXT);  -- vulkan_core.h:11420

   type VkDeviceDeviceMemoryReportCreateInfoEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:11425
      pNext : System.Address;  -- vulkan_core.h:11426
      flags : aliased VkDeviceMemoryReportFlagsEXT;  -- vulkan_core.h:11427
      pfnUserCallback : PFN_vkDeviceMemoryReportCallbackEXT;  -- vulkan_core.h:11428
      pUserData : System.Address;  -- vulkan_core.h:11429
   end record;
   pragma Convention (C_Pass_By_Copy, VkDeviceDeviceMemoryReportCreateInfoEXT);  -- vulkan_core.h:11424

   type VkPhysicalDeviceRobustness2FeaturesEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:11438
      pNext : System.Address;  -- vulkan_core.h:11439
      robustBufferAccess2 : aliased VkBool32;  -- vulkan_core.h:11440
      robustImageAccess2 : aliased VkBool32;  -- vulkan_core.h:11441
      nullDescriptor : aliased VkBool32;  -- vulkan_core.h:11442
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceRobustness2FeaturesEXT);  -- vulkan_core.h:11437

   type VkPhysicalDeviceRobustness2PropertiesEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:11446
      pNext : System.Address;  -- vulkan_core.h:11447
      robustStorageBufferAccessSizeAlignment : aliased VkDeviceSize;  -- vulkan_core.h:11448
      robustUniformBufferAccessSizeAlignment : aliased VkDeviceSize;  -- vulkan_core.h:11449
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceRobustness2PropertiesEXT);  -- vulkan_core.h:11445

   type VkSamplerCustomBorderColorCreateInfoEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:11458
      pNext : System.Address;  -- vulkan_core.h:11459
      customBorderColor : VkClearColorValue;  -- vulkan_core.h:11460
      format : aliased VkFormat;  -- vulkan_core.h:11461
   end record;
   pragma Convention (C_Pass_By_Copy, VkSamplerCustomBorderColorCreateInfoEXT);  -- vulkan_core.h:11457

   type VkPhysicalDeviceCustomBorderColorPropertiesEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:11465
      pNext : System.Address;  -- vulkan_core.h:11466
      maxCustomBorderColorSamplers : aliased stdint_h.uint32_t;  -- vulkan_core.h:11467
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceCustomBorderColorPropertiesEXT);  -- vulkan_core.h:11464

   type VkPhysicalDeviceCustomBorderColorFeaturesEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:11471
      pNext : System.Address;  -- vulkan_core.h:11472
      customBorderColors : aliased VkBool32;  -- vulkan_core.h:11473
      customBorderColorWithoutFormat : aliased VkBool32;  -- vulkan_core.h:11474
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceCustomBorderColorFeaturesEXT);  -- vulkan_core.h:11470

   type VkPrivateDataSlotEXT is new System.Address;  -- vulkan_core.h:11485

   --  skipped empty struct VkPrivateDataSlotEXT_T

   subtype VkPrivateDataSlotCreateFlagBitsEXT is unsigned;
   VK_PRIVATE_DATA_SLOT_CREATE_FLAG_BITS_MAX_ENUM_EXT : constant VkPrivateDataSlotCreateFlagBitsEXT := 2147483647;  -- vulkan_core.h:11489

   subtype VkPrivateDataSlotCreateFlagsEXT is VkFlags;  -- vulkan_core.h:11492

   type VkPhysicalDevicePrivateDataFeaturesEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:11494
      pNext : System.Address;  -- vulkan_core.h:11495
      privateData : aliased VkBool32;  -- vulkan_core.h:11496
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDevicePrivateDataFeaturesEXT);  -- vulkan_core.h:11493

   type VkDevicePrivateDataCreateInfoEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:11500
      pNext : System.Address;  -- vulkan_core.h:11501
      privateDataSlotRequestCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:11502
   end record;
   pragma Convention (C_Pass_By_Copy, VkDevicePrivateDataCreateInfoEXT);  -- vulkan_core.h:11499

   type VkPrivateDataSlotCreateInfoEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:11506
      pNext : System.Address;  -- vulkan_core.h:11507
      flags : aliased VkPrivateDataSlotCreateFlagsEXT;  -- vulkan_core.h:11508
   end record;
   pragma Convention (C_Pass_By_Copy, VkPrivateDataSlotCreateInfoEXT);  -- vulkan_core.h:11505

   type PFN_vkCreatePrivateDataSlotEXT is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : System.Address;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreatePrivateDataSlotEXT);  -- vulkan_core.h:11511

   type PFN_vkDestroyPrivateDataSlotEXT is access procedure
        (arg1 : VkDevice;
         arg2 : VkPrivateDataSlotEXT;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkDestroyPrivateDataSlotEXT);  -- vulkan_core.h:11512

   type PFN_vkSetPrivateDataEXT is access function
        (arg1 : VkDevice;
         arg2 : VkObjectType;
         arg3 : stdint_h.uint64_t;
         arg4 : VkPrivateDataSlotEXT;
         arg5 : stdint_h.uint64_t) return VkResult;
   pragma Convention (C, PFN_vkSetPrivateDataEXT);  -- vulkan_core.h:11513

   type PFN_vkGetPrivateDataEXT is access procedure
        (arg1 : VkDevice;
         arg2 : VkObjectType;
         arg3 : stdint_h.uint64_t;
         arg4 : VkPrivateDataSlotEXT;
         arg5 : access stdint_h.uint64_t);
   pragma Convention (C, PFN_vkGetPrivateDataEXT);  -- vulkan_core.h:11514

   function vkCreatePrivateDataSlotEXT
     (device : VkDevice;
      pCreateInfo : System.Address;
      pAllocator : System.Address;
      pPrivateDataSlot : System.Address) return VkResult;  -- vulkan_core.h:11517
   pragma Import (C, vkCreatePrivateDataSlotEXT, "vkCreatePrivateDataSlotEXT");

   procedure vkDestroyPrivateDataSlotEXT
     (device : VkDevice;
      privateDataSlot : VkPrivateDataSlotEXT;
      pAllocator : System.Address);  -- vulkan_core.h:11523
   pragma Import (C, vkDestroyPrivateDataSlotEXT, "vkDestroyPrivateDataSlotEXT");

   function vkSetPrivateDataEXT
     (device : VkDevice;
      objectType : VkObjectType;
      objectHandle : stdint_h.uint64_t;
      privateDataSlot : VkPrivateDataSlotEXT;
      data : stdint_h.uint64_t) return VkResult;  -- vulkan_core.h:11528
   pragma Import (C, vkSetPrivateDataEXT, "vkSetPrivateDataEXT");

   procedure vkGetPrivateDataEXT
     (device : VkDevice;
      objectType : VkObjectType;
      objectHandle : stdint_h.uint64_t;
      privateDataSlot : VkPrivateDataSlotEXT;
      pData : access stdint_h.uint64_t);  -- vulkan_core.h:11535
   pragma Import (C, vkGetPrivateDataEXT, "vkGetPrivateDataEXT");

   type VkPhysicalDevicePipelineCreationCacheControlFeaturesEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:11548
      pNext : System.Address;  -- vulkan_core.h:11549
      pipelineCreationCacheControl : aliased VkBool32;  -- vulkan_core.h:11550
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDevicePipelineCreationCacheControlFeaturesEXT);  -- vulkan_core.h:11547

   subtype VkDeviceDiagnosticsConfigFlagBitsNV is unsigned;
   VK_DEVICE_DIAGNOSTICS_CONFIG_ENABLE_SHADER_DEBUG_INFO_BIT_NV : constant VkDeviceDiagnosticsConfigFlagBitsNV := 1;
   VK_DEVICE_DIAGNOSTICS_CONFIG_ENABLE_RESOURCE_TRACKING_BIT_NV : constant VkDeviceDiagnosticsConfigFlagBitsNV := 2;
   VK_DEVICE_DIAGNOSTICS_CONFIG_ENABLE_AUTOMATIC_CHECKPOINTS_BIT_NV : constant VkDeviceDiagnosticsConfigFlagBitsNV := 4;
   VK_DEVICE_DIAGNOSTICS_CONFIG_FLAG_BITS_MAX_ENUM_NV : constant VkDeviceDiagnosticsConfigFlagBitsNV := 2147483647;  -- vulkan_core.h:11559

   subtype VkDeviceDiagnosticsConfigFlagsNV is VkFlags;  -- vulkan_core.h:11565

   type VkPhysicalDeviceDiagnosticsConfigFeaturesNV is record
      sType : aliased VkStructureType;  -- vulkan_core.h:11567
      pNext : System.Address;  -- vulkan_core.h:11568
      diagnosticsConfig : aliased VkBool32;  -- vulkan_core.h:11569
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceDiagnosticsConfigFeaturesNV);  -- vulkan_core.h:11566

   type VkDeviceDiagnosticsConfigCreateInfoNV is record
      sType : aliased VkStructureType;  -- vulkan_core.h:11573
      pNext : System.Address;  -- vulkan_core.h:11574
      flags : aliased VkDeviceDiagnosticsConfigFlagsNV;  -- vulkan_core.h:11575
   end record;
   pragma Convention (C_Pass_By_Copy, VkDeviceDiagnosticsConfigCreateInfoNV);  -- vulkan_core.h:11572

   subtype VkFragmentShadingRateTypeNV is unsigned;
   VK_FRAGMENT_SHADING_RATE_TYPE_FRAGMENT_SIZE_NV : constant VkFragmentShadingRateTypeNV := 0;
   VK_FRAGMENT_SHADING_RATE_TYPE_ENUMS_NV : constant VkFragmentShadingRateTypeNV := 1;
   VK_FRAGMENT_SHADING_RATE_TYPE_MAX_ENUM_NV : constant VkFragmentShadingRateTypeNV := 2147483647;  -- vulkan_core.h:11589

   subtype VkFragmentShadingRateNV is unsigned;
   VK_FRAGMENT_SHADING_RATE_1_INVOCATION_PER_PIXEL_NV : constant VkFragmentShadingRateNV := 0;
   VK_FRAGMENT_SHADING_RATE_1_INVOCATION_PER_1X2_PIXELS_NV : constant VkFragmentShadingRateNV := 1;
   VK_FRAGMENT_SHADING_RATE_1_INVOCATION_PER_2X1_PIXELS_NV : constant VkFragmentShadingRateNV := 4;
   VK_FRAGMENT_SHADING_RATE_1_INVOCATION_PER_2X2_PIXELS_NV : constant VkFragmentShadingRateNV := 5;
   VK_FRAGMENT_SHADING_RATE_1_INVOCATION_PER_2X4_PIXELS_NV : constant VkFragmentShadingRateNV := 6;
   VK_FRAGMENT_SHADING_RATE_1_INVOCATION_PER_4X2_PIXELS_NV : constant VkFragmentShadingRateNV := 9;
   VK_FRAGMENT_SHADING_RATE_1_INVOCATION_PER_4X4_PIXELS_NV : constant VkFragmentShadingRateNV := 10;
   VK_FRAGMENT_SHADING_RATE_2_INVOCATIONS_PER_PIXEL_NV : constant VkFragmentShadingRateNV := 11;
   VK_FRAGMENT_SHADING_RATE_4_INVOCATIONS_PER_PIXEL_NV : constant VkFragmentShadingRateNV := 12;
   VK_FRAGMENT_SHADING_RATE_8_INVOCATIONS_PER_PIXEL_NV : constant VkFragmentShadingRateNV := 13;
   VK_FRAGMENT_SHADING_RATE_16_INVOCATIONS_PER_PIXEL_NV : constant VkFragmentShadingRateNV := 14;
   VK_FRAGMENT_SHADING_RATE_NO_INVOCATIONS_NV : constant VkFragmentShadingRateNV := 15;
   VK_FRAGMENT_SHADING_RATE_MAX_ENUM_NV : constant VkFragmentShadingRateNV := 2147483647;  -- vulkan_core.h:11595

   type VkPhysicalDeviceFragmentShadingRateEnumsFeaturesNV is record
      sType : aliased VkStructureType;  -- vulkan_core.h:11611
      pNext : System.Address;  -- vulkan_core.h:11612
      fragmentShadingRateEnums : aliased VkBool32;  -- vulkan_core.h:11613
      supersampleFragmentShadingRates : aliased VkBool32;  -- vulkan_core.h:11614
      noInvocationFragmentShadingRates : aliased VkBool32;  -- vulkan_core.h:11615
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceFragmentShadingRateEnumsFeaturesNV);  -- vulkan_core.h:11610

   type VkPhysicalDeviceFragmentShadingRateEnumsPropertiesNV is record
      sType : aliased VkStructureType;  -- vulkan_core.h:11619
      pNext : System.Address;  -- vulkan_core.h:11620
      maxFragmentShadingRateInvocationCount : aliased VkSampleCountFlagBits;  -- vulkan_core.h:11621
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceFragmentShadingRateEnumsPropertiesNV);  -- vulkan_core.h:11618

   type VkPipelineFragmentShadingRateEnumStateCreateInfoNV_combinerOps_array is array (0 .. 1) of aliased VkFragmentShadingRateCombinerOpKHR;
   type VkPipelineFragmentShadingRateEnumStateCreateInfoNV is record
      sType : aliased VkStructureType;  -- vulkan_core.h:11625
      pNext : System.Address;  -- vulkan_core.h:11626
      shadingRateType : aliased VkFragmentShadingRateTypeNV;  -- vulkan_core.h:11627
      shadingRate : aliased VkFragmentShadingRateNV;  -- vulkan_core.h:11628
      combinerOps : aliased VkPipelineFragmentShadingRateEnumStateCreateInfoNV_combinerOps_array;  -- vulkan_core.h:11629
   end record;
   pragma Convention (C_Pass_By_Copy, VkPipelineFragmentShadingRateEnumStateCreateInfoNV);  -- vulkan_core.h:11624

   type PFN_vkCmdSetFragmentShadingRateEnumNV is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : VkFragmentShadingRateNV;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkCmdSetFragmentShadingRateEnumNV);  -- vulkan_core.h:11632

   procedure vkCmdSetFragmentShadingRateEnumNV
     (commandBuffer : VkCommandBuffer;
      shadingRate : VkFragmentShadingRateNV;
      combinerOps : System.Address);  -- vulkan_core.h:11635
   pragma Import (C, vkCmdSetFragmentShadingRateEnumNV, "vkCmdSetFragmentShadingRateEnumNV");

   type VkPhysicalDeviceFragmentDensityMap2FeaturesEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:11646
      pNext : System.Address;  -- vulkan_core.h:11647
      fragmentDensityMapDeferred : aliased VkBool32;  -- vulkan_core.h:11648
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceFragmentDensityMap2FeaturesEXT);  -- vulkan_core.h:11645

   type VkPhysicalDeviceFragmentDensityMap2PropertiesEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:11652
      pNext : System.Address;  -- vulkan_core.h:11653
      subsampledLoads : aliased VkBool32;  -- vulkan_core.h:11654
      subsampledCoarseReconstructionEarlyAccess : aliased VkBool32;  -- vulkan_core.h:11655
      maxSubsampledArrayLayers : aliased stdint_h.uint32_t;  -- vulkan_core.h:11656
      maxDescriptorSetSubsampledSamplers : aliased stdint_h.uint32_t;  -- vulkan_core.h:11657
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceFragmentDensityMap2PropertiesEXT);  -- vulkan_core.h:11651

   type VkCopyCommandTransformInfoQCOM is record
      sType : aliased VkStructureType;  -- vulkan_core.h:11666
      pNext : System.Address;  -- vulkan_core.h:11667
      transform : aliased VkSurfaceTransformFlagBitsKHR;  -- vulkan_core.h:11668
   end record;
   pragma Convention (C_Pass_By_Copy, VkCopyCommandTransformInfoQCOM);  -- vulkan_core.h:11665

   type VkPhysicalDeviceImageRobustnessFeaturesEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:11677
      pNext : System.Address;  -- vulkan_core.h:11678
      robustImageAccess : aliased VkBool32;  -- vulkan_core.h:11679
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceImageRobustnessFeaturesEXT);  -- vulkan_core.h:11676

   type VkPhysicalDevice4444FormatsFeaturesEXT is record
      sType : aliased VkStructureType;  -- vulkan_core.h:11688
      pNext : System.Address;  -- vulkan_core.h:11689
      formatA4R4G4B4 : aliased VkBool32;  -- vulkan_core.h:11690
      formatA4B4G4R4 : aliased VkBool32;  -- vulkan_core.h:11691
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDevice4444FormatsFeaturesEXT);  -- vulkan_core.h:11687

   type PFN_vkAcquireWinrtDisplayNV is access function (arg1 : VkPhysicalDevice; arg2 : VkDisplayKHR) return VkResult;
   pragma Convention (C, PFN_vkAcquireWinrtDisplayNV);  -- vulkan_core.h:11699

   type PFN_vkGetWinrtDisplayNV is access function
        (arg1 : VkPhysicalDevice;
         arg2 : stdint_h.uint32_t;
         arg3 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkGetWinrtDisplayNV);  -- vulkan_core.h:11700

   function vkAcquireWinrtDisplayNV (physicalDevice : VkPhysicalDevice; display : VkDisplayKHR) return VkResult;  -- vulkan_core.h:11703
   pragma Import (C, vkAcquireWinrtDisplayNV, "vkAcquireWinrtDisplayNV");

   function vkGetWinrtDisplayNV
     (physicalDevice : VkPhysicalDevice;
      deviceRelativeId : stdint_h.uint32_t;
      pDisplay : System.Address) return VkResult;  -- vulkan_core.h:11707
   pragma Import (C, vkGetWinrtDisplayNV, "vkGetWinrtDisplayNV");

   type VkPhysicalDeviceMutableDescriptorTypeFeaturesVALVE is record
      sType : aliased VkStructureType;  -- vulkan_core.h:11718
      pNext : System.Address;  -- vulkan_core.h:11719
      mutableDescriptorType : aliased VkBool32;  -- vulkan_core.h:11720
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceMutableDescriptorTypeFeaturesVALVE);  -- vulkan_core.h:11717

   type VkMutableDescriptorTypeListVALVE is record
      descriptorTypeCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:11724
      pDescriptorTypes : System.Address;  -- vulkan_core.h:11725
   end record;
   pragma Convention (C_Pass_By_Copy, VkMutableDescriptorTypeListVALVE);  -- vulkan_core.h:11723

   type VkMutableDescriptorTypeCreateInfoVALVE is record
      sType : aliased VkStructureType;  -- vulkan_core.h:11729
      pNext : System.Address;  -- vulkan_core.h:11730
      mutableDescriptorTypeListCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:11731
      pMutableDescriptorTypeLists : System.Address;  -- vulkan_core.h:11732
   end record;
   pragma Convention (C_Pass_By_Copy, VkMutableDescriptorTypeCreateInfoVALVE);  -- vulkan_core.h:11728

   --  skipped empty struct VkAccelerationStructureKHR_T

   type VkAccelerationStructureKHR is new System.Address;  -- vulkan_core.h:11738

   subtype VkBuildAccelerationStructureModeKHR is unsigned;
   VK_BUILD_ACCELERATION_STRUCTURE_MODE_BUILD_KHR : constant VkBuildAccelerationStructureModeKHR := 0;
   VK_BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR : constant VkBuildAccelerationStructureModeKHR := 1;
   VK_BUILD_ACCELERATION_STRUCTURE_MODE_MAX_ENUM_KHR : constant VkBuildAccelerationStructureModeKHR := 2147483647;  -- vulkan_core.h:11742

   subtype VkAccelerationStructureBuildTypeKHR is unsigned;
   VK_ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_KHR : constant VkAccelerationStructureBuildTypeKHR := 0;
   VK_ACCELERATION_STRUCTURE_BUILD_TYPE_DEVICE_KHR : constant VkAccelerationStructureBuildTypeKHR := 1;
   VK_ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_OR_DEVICE_KHR : constant VkAccelerationStructureBuildTypeKHR := 2;
   VK_ACCELERATION_STRUCTURE_BUILD_TYPE_MAX_ENUM_KHR : constant VkAccelerationStructureBuildTypeKHR := 2147483647;  -- vulkan_core.h:11748

   subtype VkAccelerationStructureCompatibilityKHR is unsigned;
   VK_ACCELERATION_STRUCTURE_COMPATIBILITY_COMPATIBLE_KHR : constant VkAccelerationStructureCompatibilityKHR := 0;
   VK_ACCELERATION_STRUCTURE_COMPATIBILITY_INCOMPATIBLE_KHR : constant VkAccelerationStructureCompatibilityKHR := 1;
   VK_ACCELERATION_STRUCTURE_COMPATIBILITY_MAX_ENUM_KHR : constant VkAccelerationStructureCompatibilityKHR := 2147483647;  -- vulkan_core.h:11755

   subtype VkAccelerationStructureCreateFlagBitsKHR is unsigned;
   VK_ACCELERATION_STRUCTURE_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_KHR : constant VkAccelerationStructureCreateFlagBitsKHR := 1;
   VK_ACCELERATION_STRUCTURE_CREATE_FLAG_BITS_MAX_ENUM_KHR : constant VkAccelerationStructureCreateFlagBitsKHR := 2147483647;  -- vulkan_core.h:11761

   subtype VkAccelerationStructureCreateFlagsKHR is VkFlags;  -- vulkan_core.h:11765

   type VkDeviceOrHostAddressKHR (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            deviceAddress : aliased VkDeviceAddress;  -- vulkan_core.h:11767
         when others =>
            hostAddress : System.Address;  -- vulkan_core.h:11768
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, VkDeviceOrHostAddressKHR);
   pragma Unchecked_Union (VkDeviceOrHostAddressKHR);  -- vulkan_core.h:11766

   type VkDeviceOrHostAddressConstKHR (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            deviceAddress : aliased VkDeviceAddress;  -- vulkan_core.h:11772
         when others =>
            hostAddress : System.Address;  -- vulkan_core.h:11773
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, VkDeviceOrHostAddressConstKHR);
   pragma Unchecked_Union (VkDeviceOrHostAddressConstKHR);  -- vulkan_core.h:11771

   type VkAccelerationStructureBuildRangeInfoKHR is record
      primitiveCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:11777
      primitiveOffset : aliased stdint_h.uint32_t;  -- vulkan_core.h:11778
      firstVertex : aliased stdint_h.uint32_t;  -- vulkan_core.h:11779
      transformOffset : aliased stdint_h.uint32_t;  -- vulkan_core.h:11780
   end record;
   pragma Convention (C_Pass_By_Copy, VkAccelerationStructureBuildRangeInfoKHR);  -- vulkan_core.h:11776

   type VkAccelerationStructureGeometryTrianglesDataKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:11784
      pNext : System.Address;  -- vulkan_core.h:11785
      vertexFormat : aliased VkFormat;  -- vulkan_core.h:11786
      vertexData : VkDeviceOrHostAddressConstKHR;  -- vulkan_core.h:11787
      vertexStride : aliased VkDeviceSize;  -- vulkan_core.h:11788
      maxVertex : aliased stdint_h.uint32_t;  -- vulkan_core.h:11789
      indexType : aliased VkIndexType;  -- vulkan_core.h:11790
      indexData : VkDeviceOrHostAddressConstKHR;  -- vulkan_core.h:11791
      transformData : VkDeviceOrHostAddressConstKHR;  -- vulkan_core.h:11792
   end record;
   pragma Convention (C_Pass_By_Copy, VkAccelerationStructureGeometryTrianglesDataKHR);  -- vulkan_core.h:11783

   type VkAccelerationStructureGeometryAabbsDataKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:11796
      pNext : System.Address;  -- vulkan_core.h:11797
      data : VkDeviceOrHostAddressConstKHR;  -- vulkan_core.h:11798
      stride : aliased VkDeviceSize;  -- vulkan_core.h:11799
   end record;
   pragma Convention (C_Pass_By_Copy, VkAccelerationStructureGeometryAabbsDataKHR);  -- vulkan_core.h:11795

   type VkAccelerationStructureGeometryInstancesDataKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:11803
      pNext : System.Address;  -- vulkan_core.h:11804
      arrayOfPointers : aliased VkBool32;  -- vulkan_core.h:11805
      data : VkDeviceOrHostAddressConstKHR;  -- vulkan_core.h:11806
   end record;
   pragma Convention (C_Pass_By_Copy, VkAccelerationStructureGeometryInstancesDataKHR);  -- vulkan_core.h:11802

   type VkAccelerationStructureGeometryDataKHR (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            triangles : aliased VkAccelerationStructureGeometryTrianglesDataKHR;  -- vulkan_core.h:11810
         when 1 =>
            aabbs : aliased VkAccelerationStructureGeometryAabbsDataKHR;  -- vulkan_core.h:11811
         when others =>
            instances : aliased VkAccelerationStructureGeometryInstancesDataKHR;  -- vulkan_core.h:11812
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, VkAccelerationStructureGeometryDataKHR);
   pragma Unchecked_Union (VkAccelerationStructureGeometryDataKHR);  -- vulkan_core.h:11809

   type VkAccelerationStructureGeometryKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:11816
      pNext : System.Address;  -- vulkan_core.h:11817
      geometryType : aliased VkGeometryTypeKHR;  -- vulkan_core.h:11818
      geometry : VkAccelerationStructureGeometryDataKHR;  -- vulkan_core.h:11819
      flags : aliased VkGeometryFlagsKHR;  -- vulkan_core.h:11820
   end record;
   pragma Convention (C_Pass_By_Copy, VkAccelerationStructureGeometryKHR);  -- vulkan_core.h:11815

   type VkAccelerationStructureBuildGeometryInfoKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:11824
      pNext : System.Address;  -- vulkan_core.h:11825
      c_type : aliased VkAccelerationStructureTypeKHR;  -- vulkan_core.h:11826
      flags : aliased VkBuildAccelerationStructureFlagsKHR;  -- vulkan_core.h:11827
      mode : aliased VkBuildAccelerationStructureModeKHR;  -- vulkan_core.h:11828
      srcAccelerationStructure : VkAccelerationStructureKHR;  -- vulkan_core.h:11829
      dstAccelerationStructure : VkAccelerationStructureKHR;  -- vulkan_core.h:11830
      geometryCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:11831
      pGeometries : System.Address;  -- vulkan_core.h:11832
      ppGeometries : System.Address;  -- vulkan_core.h:11833
      scratchData : VkDeviceOrHostAddressKHR;  -- vulkan_core.h:11834
   end record;
   pragma Convention (C_Pass_By_Copy, VkAccelerationStructureBuildGeometryInfoKHR);  -- vulkan_core.h:11823

   type VkAccelerationStructureCreateInfoKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:11838
      pNext : System.Address;  -- vulkan_core.h:11839
      createFlags : aliased VkAccelerationStructureCreateFlagsKHR;  -- vulkan_core.h:11840
      buffer : VkBuffer;  -- vulkan_core.h:11841
      offset : aliased VkDeviceSize;  -- vulkan_core.h:11842
      size : aliased VkDeviceSize;  -- vulkan_core.h:11843
      c_type : aliased VkAccelerationStructureTypeKHR;  -- vulkan_core.h:11844
      deviceAddress : aliased VkDeviceAddress;  -- vulkan_core.h:11845
   end record;
   pragma Convention (C_Pass_By_Copy, VkAccelerationStructureCreateInfoKHR);  -- vulkan_core.h:11837

   type VkWriteDescriptorSetAccelerationStructureKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:11849
      pNext : System.Address;  -- vulkan_core.h:11850
      accelerationStructureCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:11851
      pAccelerationStructures : System.Address;  -- vulkan_core.h:11852
   end record;
   pragma Convention (C_Pass_By_Copy, VkWriteDescriptorSetAccelerationStructureKHR);  -- vulkan_core.h:11848

   type VkPhysicalDeviceAccelerationStructureFeaturesKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:11856
      pNext : System.Address;  -- vulkan_core.h:11857
      accelerationStructure : aliased VkBool32;  -- vulkan_core.h:11858
      accelerationStructureCaptureReplay : aliased VkBool32;  -- vulkan_core.h:11859
      accelerationStructureIndirectBuild : aliased VkBool32;  -- vulkan_core.h:11860
      accelerationStructureHostCommands : aliased VkBool32;  -- vulkan_core.h:11861
      descriptorBindingAccelerationStructureUpdateAfterBind : aliased VkBool32;  -- vulkan_core.h:11862
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceAccelerationStructureFeaturesKHR);  -- vulkan_core.h:11855

   type VkPhysicalDeviceAccelerationStructurePropertiesKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:11866
      pNext : System.Address;  -- vulkan_core.h:11867
      maxGeometryCount : aliased stdint_h.uint64_t;  -- vulkan_core.h:11868
      maxInstanceCount : aliased stdint_h.uint64_t;  -- vulkan_core.h:11869
      maxPrimitiveCount : aliased stdint_h.uint64_t;  -- vulkan_core.h:11870
      maxPerStageDescriptorAccelerationStructures : aliased stdint_h.uint32_t;  -- vulkan_core.h:11871
      maxPerStageDescriptorUpdateAfterBindAccelerationStructures : aliased stdint_h.uint32_t;  -- vulkan_core.h:11872
      maxDescriptorSetAccelerationStructures : aliased stdint_h.uint32_t;  -- vulkan_core.h:11873
      maxDescriptorSetUpdateAfterBindAccelerationStructures : aliased stdint_h.uint32_t;  -- vulkan_core.h:11874
      minAccelerationStructureScratchOffsetAlignment : aliased stdint_h.uint32_t;  -- vulkan_core.h:11875
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceAccelerationStructurePropertiesKHR);  -- vulkan_core.h:11865

   type VkAccelerationStructureDeviceAddressInfoKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:11879
      pNext : System.Address;  -- vulkan_core.h:11880
      accelerationStructure : VkAccelerationStructureKHR;  -- vulkan_core.h:11881
   end record;
   pragma Convention (C_Pass_By_Copy, VkAccelerationStructureDeviceAddressInfoKHR);  -- vulkan_core.h:11878

   type VkAccelerationStructureVersionInfoKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:11885
      pNext : System.Address;  -- vulkan_core.h:11886
      pVersionData : access stdint_h.uint8_t;  -- vulkan_core.h:11887
   end record;
   pragma Convention (C_Pass_By_Copy, VkAccelerationStructureVersionInfoKHR);  -- vulkan_core.h:11884

   type VkCopyAccelerationStructureToMemoryInfoKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:11891
      pNext : System.Address;  -- vulkan_core.h:11892
      src : VkAccelerationStructureKHR;  -- vulkan_core.h:11893
      dst : VkDeviceOrHostAddressKHR;  -- vulkan_core.h:11894
      mode : aliased VkCopyAccelerationStructureModeKHR;  -- vulkan_core.h:11895
   end record;
   pragma Convention (C_Pass_By_Copy, VkCopyAccelerationStructureToMemoryInfoKHR);  -- vulkan_core.h:11890

   type VkCopyMemoryToAccelerationStructureInfoKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:11899
      pNext : System.Address;  -- vulkan_core.h:11900
      src : VkDeviceOrHostAddressConstKHR;  -- vulkan_core.h:11901
      dst : VkAccelerationStructureKHR;  -- vulkan_core.h:11902
      mode : aliased VkCopyAccelerationStructureModeKHR;  -- vulkan_core.h:11903
   end record;
   pragma Convention (C_Pass_By_Copy, VkCopyMemoryToAccelerationStructureInfoKHR);  -- vulkan_core.h:11898

   type VkCopyAccelerationStructureInfoKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:11907
      pNext : System.Address;  -- vulkan_core.h:11908
      src : VkAccelerationStructureKHR;  -- vulkan_core.h:11909
      dst : VkAccelerationStructureKHR;  -- vulkan_core.h:11910
      mode : aliased VkCopyAccelerationStructureModeKHR;  -- vulkan_core.h:11911
   end record;
   pragma Convention (C_Pass_By_Copy, VkCopyAccelerationStructureInfoKHR);  -- vulkan_core.h:11906

   type VkAccelerationStructureBuildSizesInfoKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:11915
      pNext : System.Address;  -- vulkan_core.h:11916
      accelerationStructureSize : aliased VkDeviceSize;  -- vulkan_core.h:11917
      updateScratchSize : aliased VkDeviceSize;  -- vulkan_core.h:11918
      buildScratchSize : aliased VkDeviceSize;  -- vulkan_core.h:11919
   end record;
   pragma Convention (C_Pass_By_Copy, VkAccelerationStructureBuildSizesInfoKHR);  -- vulkan_core.h:11914

   type PFN_vkCreateAccelerationStructureKHR is access function
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : System.Address;
         arg4 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateAccelerationStructureKHR);  -- vulkan_core.h:11922

   type PFN_vkDestroyAccelerationStructureKHR is access procedure
        (arg1 : VkDevice;
         arg2 : VkAccelerationStructureKHR;
         arg3 : System.Address);
   pragma Convention (C, PFN_vkDestroyAccelerationStructureKHR);  -- vulkan_core.h:11923

   type PFN_vkCmdBuildAccelerationStructuresKHR is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : stdint_h.uint32_t;
         arg3 : System.Address;
         arg4 : System.Address);
   pragma Convention (C, PFN_vkCmdBuildAccelerationStructuresKHR);  -- vulkan_core.h:11924

   type PFN_vkCmdBuildAccelerationStructuresIndirectKHR is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : stdint_h.uint32_t;
         arg3 : System.Address;
         arg4 : access VkDeviceAddress;
         arg5 : access stdint_h.uint32_t;
         arg6 : System.Address);
   pragma Convention (C, PFN_vkCmdBuildAccelerationStructuresIndirectKHR);  -- vulkan_core.h:11925

   type PFN_vkBuildAccelerationStructuresKHR is access function
        (arg1 : VkDevice;
         arg2 : VkDeferredOperationKHR;
         arg3 : stdint_h.uint32_t;
         arg4 : System.Address;
         arg5 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkBuildAccelerationStructuresKHR);  -- vulkan_core.h:11926

   type PFN_vkCopyAccelerationStructureKHR is access function
        (arg1 : VkDevice;
         arg2 : VkDeferredOperationKHR;
         arg3 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCopyAccelerationStructureKHR);  -- vulkan_core.h:11927

   type PFN_vkCopyAccelerationStructureToMemoryKHR is access function
        (arg1 : VkDevice;
         arg2 : VkDeferredOperationKHR;
         arg3 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCopyAccelerationStructureToMemoryKHR);  -- vulkan_core.h:11928

   type PFN_vkCopyMemoryToAccelerationStructureKHR is access function
        (arg1 : VkDevice;
         arg2 : VkDeferredOperationKHR;
         arg3 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCopyMemoryToAccelerationStructureKHR);  -- vulkan_core.h:11929

   type PFN_vkWriteAccelerationStructuresPropertiesKHR is access function
        (arg1 : VkDevice;
         arg2 : stdint_h.uint32_t;
         arg3 : System.Address;
         arg4 : VkQueryType;
         arg5 : crtdefs_h.size_t;
         arg6 : System.Address;
         arg7 : crtdefs_h.size_t) return VkResult;
   pragma Convention (C, PFN_vkWriteAccelerationStructuresPropertiesKHR);  -- vulkan_core.h:11930

   type PFN_vkCmdCopyAccelerationStructureKHR is access procedure (arg1 : VkCommandBuffer; arg2 : System.Address);
   pragma Convention (C, PFN_vkCmdCopyAccelerationStructureKHR);  -- vulkan_core.h:11931

   type PFN_vkCmdCopyAccelerationStructureToMemoryKHR is access procedure (arg1 : VkCommandBuffer; arg2 : System.Address);
   pragma Convention (C, PFN_vkCmdCopyAccelerationStructureToMemoryKHR);  -- vulkan_core.h:11932

   type PFN_vkCmdCopyMemoryToAccelerationStructureKHR is access procedure (arg1 : VkCommandBuffer; arg2 : System.Address);
   pragma Convention (C, PFN_vkCmdCopyMemoryToAccelerationStructureKHR);  -- vulkan_core.h:11933

   type PFN_vkGetAccelerationStructureDeviceAddressKHR is access function (arg1 : VkDevice; arg2 : System.Address) return VkDeviceAddress;
   pragma Convention (C, PFN_vkGetAccelerationStructureDeviceAddressKHR);  -- vulkan_core.h:11934

   type PFN_vkCmdWriteAccelerationStructuresPropertiesKHR is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : stdint_h.uint32_t;
         arg3 : System.Address;
         arg4 : VkQueryType;
         arg5 : VkQueryPool;
         arg6 : stdint_h.uint32_t);
   pragma Convention (C, PFN_vkCmdWriteAccelerationStructuresPropertiesKHR);  -- vulkan_core.h:11935

   type PFN_vkGetDeviceAccelerationStructureCompatibilityKHR is access procedure
        (arg1 : VkDevice;
         arg2 : System.Address;
         arg3 : access VkAccelerationStructureCompatibilityKHR);
   pragma Convention (C, PFN_vkGetDeviceAccelerationStructureCompatibilityKHR);  -- vulkan_core.h:11936

   type PFN_vkGetAccelerationStructureBuildSizesKHR is access procedure
        (arg1 : VkDevice;
         arg2 : VkAccelerationStructureBuildTypeKHR;
         arg3 : System.Address;
         arg4 : access stdint_h.uint32_t;
         arg5 : access VkAccelerationStructureBuildSizesInfoKHR);
   pragma Convention (C, PFN_vkGetAccelerationStructureBuildSizesKHR);  -- vulkan_core.h:11937

   function vkCreateAccelerationStructureKHR
     (device : VkDevice;
      pCreateInfo : System.Address;
      pAllocator : System.Address;
      pAccelerationStructure : System.Address) return VkResult;  -- vulkan_core.h:11940
   pragma Import (C, vkCreateAccelerationStructureKHR, "vkCreateAccelerationStructureKHR");

   procedure vkDestroyAccelerationStructureKHR
     (device : VkDevice;
      accelerationStructure : VkAccelerationStructureKHR;
      pAllocator : System.Address);  -- vulkan_core.h:11946
   pragma Import (C, vkDestroyAccelerationStructureKHR, "vkDestroyAccelerationStructureKHR");

   procedure vkCmdBuildAccelerationStructuresKHR
     (commandBuffer : VkCommandBuffer;
      infoCount : stdint_h.uint32_t;
      pInfos : System.Address;
      ppBuildRangeInfos : System.Address);  -- vulkan_core.h:11951
   pragma Import (C, vkCmdBuildAccelerationStructuresKHR, "vkCmdBuildAccelerationStructuresKHR");

   procedure vkCmdBuildAccelerationStructuresIndirectKHR
     (commandBuffer : VkCommandBuffer;
      infoCount : stdint_h.uint32_t;
      pInfos : System.Address;
      pIndirectDeviceAddresses : access VkDeviceAddress;
      pIndirectStrides : access stdint_h.uint32_t;
      ppMaxPrimitiveCounts : System.Address);  -- vulkan_core.h:11957
   pragma Import (C, vkCmdBuildAccelerationStructuresIndirectKHR, "vkCmdBuildAccelerationStructuresIndirectKHR");

   function vkBuildAccelerationStructuresKHR
     (device : VkDevice;
      deferredOperation : VkDeferredOperationKHR;
      infoCount : stdint_h.uint32_t;
      pInfos : System.Address;
      ppBuildRangeInfos : System.Address) return VkResult;  -- vulkan_core.h:11965
   pragma Import (C, vkBuildAccelerationStructuresKHR, "vkBuildAccelerationStructuresKHR");

   function vkCopyAccelerationStructureKHR
     (device : VkDevice;
      deferredOperation : VkDeferredOperationKHR;
      pInfo : System.Address) return VkResult;  -- vulkan_core.h:11972
   pragma Import (C, vkCopyAccelerationStructureKHR, "vkCopyAccelerationStructureKHR");

   function vkCopyAccelerationStructureToMemoryKHR
     (device : VkDevice;
      deferredOperation : VkDeferredOperationKHR;
      pInfo : System.Address) return VkResult;  -- vulkan_core.h:11977
   pragma Import (C, vkCopyAccelerationStructureToMemoryKHR, "vkCopyAccelerationStructureToMemoryKHR");

   function vkCopyMemoryToAccelerationStructureKHR
     (device : VkDevice;
      deferredOperation : VkDeferredOperationKHR;
      pInfo : System.Address) return VkResult;  -- vulkan_core.h:11982
   pragma Import (C, vkCopyMemoryToAccelerationStructureKHR, "vkCopyMemoryToAccelerationStructureKHR");

   function vkWriteAccelerationStructuresPropertiesKHR
     (device : VkDevice;
      accelerationStructureCount : stdint_h.uint32_t;
      pAccelerationStructures : System.Address;
      queryType : VkQueryType;
      dataSize : crtdefs_h.size_t;
      pData : System.Address;
      stride : crtdefs_h.size_t) return VkResult;  -- vulkan_core.h:11987
   pragma Import (C, vkWriteAccelerationStructuresPropertiesKHR, "vkWriteAccelerationStructuresPropertiesKHR");

   procedure vkCmdCopyAccelerationStructureKHR (commandBuffer : VkCommandBuffer; pInfo : System.Address);  -- vulkan_core.h:11996
   pragma Import (C, vkCmdCopyAccelerationStructureKHR, "vkCmdCopyAccelerationStructureKHR");

   procedure vkCmdCopyAccelerationStructureToMemoryKHR (commandBuffer : VkCommandBuffer; pInfo : System.Address);  -- vulkan_core.h:12000
   pragma Import (C, vkCmdCopyAccelerationStructureToMemoryKHR, "vkCmdCopyAccelerationStructureToMemoryKHR");

   procedure vkCmdCopyMemoryToAccelerationStructureKHR (commandBuffer : VkCommandBuffer; pInfo : System.Address);  -- vulkan_core.h:12004
   pragma Import (C, vkCmdCopyMemoryToAccelerationStructureKHR, "vkCmdCopyMemoryToAccelerationStructureKHR");

   function vkGetAccelerationStructureDeviceAddressKHR (device : VkDevice; pInfo : System.Address) return VkDeviceAddress;  -- vulkan_core.h:12008
   pragma Import (C, vkGetAccelerationStructureDeviceAddressKHR, "vkGetAccelerationStructureDeviceAddressKHR");

   procedure vkCmdWriteAccelerationStructuresPropertiesKHR
     (commandBuffer : VkCommandBuffer;
      accelerationStructureCount : stdint_h.uint32_t;
      pAccelerationStructures : System.Address;
      queryType : VkQueryType;
      queryPool : VkQueryPool;
      firstQuery : stdint_h.uint32_t);  -- vulkan_core.h:12012
   pragma Import (C, vkCmdWriteAccelerationStructuresPropertiesKHR, "vkCmdWriteAccelerationStructuresPropertiesKHR");

   procedure vkGetDeviceAccelerationStructureCompatibilityKHR
     (device : VkDevice;
      pVersionInfo : System.Address;
      pCompatibility : access VkAccelerationStructureCompatibilityKHR);  -- vulkan_core.h:12020
   pragma Import (C, vkGetDeviceAccelerationStructureCompatibilityKHR, "vkGetDeviceAccelerationStructureCompatibilityKHR");

   procedure vkGetAccelerationStructureBuildSizesKHR
     (device : VkDevice;
      buildType : VkAccelerationStructureBuildTypeKHR;
      pBuildInfo : System.Address;
      pMaxPrimitiveCounts : access stdint_h.uint32_t;
      pSizeInfo : access VkAccelerationStructureBuildSizesInfoKHR);  -- vulkan_core.h:12025
   pragma Import (C, vkGetAccelerationStructureBuildSizesKHR, "vkGetAccelerationStructureBuildSizesKHR");

   subtype VkShaderGroupShaderKHR is unsigned;
   VK_SHADER_GROUP_SHADER_GENERAL_KHR : constant VkShaderGroupShaderKHR := 0;
   VK_SHADER_GROUP_SHADER_CLOSEST_HIT_KHR : constant VkShaderGroupShaderKHR := 1;
   VK_SHADER_GROUP_SHADER_ANY_HIT_KHR : constant VkShaderGroupShaderKHR := 2;
   VK_SHADER_GROUP_SHADER_INTERSECTION_KHR : constant VkShaderGroupShaderKHR := 3;
   VK_SHADER_GROUP_SHADER_MAX_ENUM_KHR : constant VkShaderGroupShaderKHR := 2147483647;  -- vulkan_core.h:12038

   type VkRayTracingShaderGroupCreateInfoKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:12046
      pNext : System.Address;  -- vulkan_core.h:12047
      c_type : aliased VkRayTracingShaderGroupTypeKHR;  -- vulkan_core.h:12048
      generalShader : aliased stdint_h.uint32_t;  -- vulkan_core.h:12049
      closestHitShader : aliased stdint_h.uint32_t;  -- vulkan_core.h:12050
      anyHitShader : aliased stdint_h.uint32_t;  -- vulkan_core.h:12051
      intersectionShader : aliased stdint_h.uint32_t;  -- vulkan_core.h:12052
      pShaderGroupCaptureReplayHandle : System.Address;  -- vulkan_core.h:12053
   end record;
   pragma Convention (C_Pass_By_Copy, VkRayTracingShaderGroupCreateInfoKHR);  -- vulkan_core.h:12045

   type VkRayTracingPipelineInterfaceCreateInfoKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:12057
      pNext : System.Address;  -- vulkan_core.h:12058
      maxPipelineRayPayloadSize : aliased stdint_h.uint32_t;  -- vulkan_core.h:12059
      maxPipelineRayHitAttributeSize : aliased stdint_h.uint32_t;  -- vulkan_core.h:12060
   end record;
   pragma Convention (C_Pass_By_Copy, VkRayTracingPipelineInterfaceCreateInfoKHR);  -- vulkan_core.h:12056

   type VkRayTracingPipelineCreateInfoKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:12064
      pNext : System.Address;  -- vulkan_core.h:12065
      flags : aliased VkPipelineCreateFlags;  -- vulkan_core.h:12066
      stageCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:12067
      pStages : System.Address;  -- vulkan_core.h:12068
      groupCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:12069
      pGroups : System.Address;  -- vulkan_core.h:12070
      maxPipelineRayRecursionDepth : aliased stdint_h.uint32_t;  -- vulkan_core.h:12071
      pLibraryInfo : System.Address;  -- vulkan_core.h:12072
      pLibraryInterface : System.Address;  -- vulkan_core.h:12073
      pDynamicState : System.Address;  -- vulkan_core.h:12074
      layout : VkPipelineLayout;  -- vulkan_core.h:12075
      basePipelineHandle : VkPipeline;  -- vulkan_core.h:12076
      basePipelineIndex : aliased stdint_h.int32_t;  -- vulkan_core.h:12077
   end record;
   pragma Convention (C_Pass_By_Copy, VkRayTracingPipelineCreateInfoKHR);  -- vulkan_core.h:12063

   type VkPhysicalDeviceRayTracingPipelineFeaturesKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:12081
      pNext : System.Address;  -- vulkan_core.h:12082
      rayTracingPipeline : aliased VkBool32;  -- vulkan_core.h:12083
      rayTracingPipelineShaderGroupHandleCaptureReplay : aliased VkBool32;  -- vulkan_core.h:12084
      rayTracingPipelineShaderGroupHandleCaptureReplayMixed : aliased VkBool32;  -- vulkan_core.h:12085
      rayTracingPipelineTraceRaysIndirect : aliased VkBool32;  -- vulkan_core.h:12086
      rayTraversalPrimitiveCulling : aliased VkBool32;  -- vulkan_core.h:12087
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceRayTracingPipelineFeaturesKHR);  -- vulkan_core.h:12080

   type VkPhysicalDeviceRayTracingPipelinePropertiesKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:12091
      pNext : System.Address;  -- vulkan_core.h:12092
      shaderGroupHandleSize : aliased stdint_h.uint32_t;  -- vulkan_core.h:12093
      maxRayRecursionDepth : aliased stdint_h.uint32_t;  -- vulkan_core.h:12094
      maxShaderGroupStride : aliased stdint_h.uint32_t;  -- vulkan_core.h:12095
      shaderGroupBaseAlignment : aliased stdint_h.uint32_t;  -- vulkan_core.h:12096
      shaderGroupHandleCaptureReplaySize : aliased stdint_h.uint32_t;  -- vulkan_core.h:12097
      maxRayDispatchInvocationCount : aliased stdint_h.uint32_t;  -- vulkan_core.h:12098
      shaderGroupHandleAlignment : aliased stdint_h.uint32_t;  -- vulkan_core.h:12099
      maxRayHitAttributeSize : aliased stdint_h.uint32_t;  -- vulkan_core.h:12100
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceRayTracingPipelinePropertiesKHR);  -- vulkan_core.h:12090

   type VkStridedDeviceAddressRegionKHR is record
      deviceAddress : aliased VkDeviceAddress;  -- vulkan_core.h:12104
      stride : aliased VkDeviceSize;  -- vulkan_core.h:12105
      size : aliased VkDeviceSize;  -- vulkan_core.h:12106
   end record;
   pragma Convention (C_Pass_By_Copy, VkStridedDeviceAddressRegionKHR);  -- vulkan_core.h:12103

   type VkTraceRaysIndirectCommandKHR is record
      width : aliased stdint_h.uint32_t;  -- vulkan_core.h:12110
      height : aliased stdint_h.uint32_t;  -- vulkan_core.h:12111
      depth : aliased stdint_h.uint32_t;  -- vulkan_core.h:12112
   end record;
   pragma Convention (C_Pass_By_Copy, VkTraceRaysIndirectCommandKHR);  -- vulkan_core.h:12109

   type PFN_vkCmdTraceRaysKHR is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : System.Address;
         arg3 : System.Address;
         arg4 : System.Address;
         arg5 : System.Address;
         arg6 : stdint_h.uint32_t;
         arg7 : stdint_h.uint32_t;
         arg8 : stdint_h.uint32_t);
   pragma Convention (C, PFN_vkCmdTraceRaysKHR);  -- vulkan_core.h:12115

   type PFN_vkCreateRayTracingPipelinesKHR is access function
        (arg1 : VkDevice;
         arg2 : VkDeferredOperationKHR;
         arg3 : VkPipelineCache;
         arg4 : stdint_h.uint32_t;
         arg5 : System.Address;
         arg6 : System.Address;
         arg7 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkCreateRayTracingPipelinesKHR);  -- vulkan_core.h:12116

   type PFN_vkGetRayTracingCaptureReplayShaderGroupHandlesKHR is access function
        (arg1 : VkDevice;
         arg2 : VkPipeline;
         arg3 : stdint_h.uint32_t;
         arg4 : stdint_h.uint32_t;
         arg5 : crtdefs_h.size_t;
         arg6 : System.Address) return VkResult;
   pragma Convention (C, PFN_vkGetRayTracingCaptureReplayShaderGroupHandlesKHR);  -- vulkan_core.h:12117

   type PFN_vkCmdTraceRaysIndirectKHR is access procedure
        (arg1 : VkCommandBuffer;
         arg2 : System.Address;
         arg3 : System.Address;
         arg4 : System.Address;
         arg5 : System.Address;
         arg6 : VkDeviceAddress);
   pragma Convention (C, PFN_vkCmdTraceRaysIndirectKHR);  -- vulkan_core.h:12118

   type PFN_vkGetRayTracingShaderGroupStackSizeKHR is access function
        (arg1 : VkDevice;
         arg2 : VkPipeline;
         arg3 : stdint_h.uint32_t;
         arg4 : VkShaderGroupShaderKHR) return VkDeviceSize;
   pragma Convention (C, PFN_vkGetRayTracingShaderGroupStackSizeKHR);  -- vulkan_core.h:12119

   type PFN_vkCmdSetRayTracingPipelineStackSizeKHR is access procedure (arg1 : VkCommandBuffer; arg2 : stdint_h.uint32_t);
   pragma Convention (C, PFN_vkCmdSetRayTracingPipelineStackSizeKHR);  -- vulkan_core.h:12120

   procedure vkCmdTraceRaysKHR
     (commandBuffer : VkCommandBuffer;
      pRaygenShaderBindingTable : System.Address;
      pMissShaderBindingTable : System.Address;
      pHitShaderBindingTable : System.Address;
      pCallableShaderBindingTable : System.Address;
      width : stdint_h.uint32_t;
      height : stdint_h.uint32_t;
      depth : stdint_h.uint32_t);  -- vulkan_core.h:12123
   pragma Import (C, vkCmdTraceRaysKHR, "vkCmdTraceRaysKHR");

   function vkCreateRayTracingPipelinesKHR
     (device : VkDevice;
      deferredOperation : VkDeferredOperationKHR;
      pipelineCache : VkPipelineCache;
      createInfoCount : stdint_h.uint32_t;
      pCreateInfos : System.Address;
      pAllocator : System.Address;
      pPipelines : System.Address) return VkResult;  -- vulkan_core.h:12133
   pragma Import (C, vkCreateRayTracingPipelinesKHR, "vkCreateRayTracingPipelinesKHR");

   function vkGetRayTracingCaptureReplayShaderGroupHandlesKHR
     (device : VkDevice;
      pipeline : VkPipeline;
      firstGroup : stdint_h.uint32_t;
      groupCount : stdint_h.uint32_t;
      dataSize : crtdefs_h.size_t;
      pData : System.Address) return VkResult;  -- vulkan_core.h:12142
   pragma Import (C, vkGetRayTracingCaptureReplayShaderGroupHandlesKHR, "vkGetRayTracingCaptureReplayShaderGroupHandlesKHR");

   procedure vkCmdTraceRaysIndirectKHR
     (commandBuffer : VkCommandBuffer;
      pRaygenShaderBindingTable : System.Address;
      pMissShaderBindingTable : System.Address;
      pHitShaderBindingTable : System.Address;
      pCallableShaderBindingTable : System.Address;
      indirectDeviceAddress : VkDeviceAddress);  -- vulkan_core.h:12150
   pragma Import (C, vkCmdTraceRaysIndirectKHR, "vkCmdTraceRaysIndirectKHR");

   function vkGetRayTracingShaderGroupStackSizeKHR
     (device : VkDevice;
      pipeline : VkPipeline;
      group : stdint_h.uint32_t;
      groupShader : VkShaderGroupShaderKHR) return VkDeviceSize;  -- vulkan_core.h:12158
   pragma Import (C, vkGetRayTracingShaderGroupStackSizeKHR, "vkGetRayTracingShaderGroupStackSizeKHR");

   procedure vkCmdSetRayTracingPipelineStackSizeKHR (commandBuffer : VkCommandBuffer; pipelineStackSize : stdint_h.uint32_t);  -- vulkan_core.h:12164
   pragma Import (C, vkCmdSetRayTracingPipelineStackSizeKHR, "vkCmdSetRayTracingPipelineStackSizeKHR");

   type VkPhysicalDeviceRayQueryFeaturesKHR is record
      sType : aliased VkStructureType;  -- vulkan_core.h:12174
      pNext : System.Address;  -- vulkan_core.h:12175
      rayQuery : aliased VkBool32;  -- vulkan_core.h:12176
   end record;
   pragma Convention (C_Pass_By_Copy, VkPhysicalDeviceRayQueryFeaturesKHR);  -- vulkan_core.h:12173

end Vulkan.Core.Binding;
