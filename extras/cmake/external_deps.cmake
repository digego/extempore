# External shared library dependencies for Extempore
# Requires: EXTERNAL_SHLIBS_AUDIO, EXTERNAL_SHLIBS_GRAPHICS options
# Sets up: the library targets (ExternalProjects, or a prebuilt download for
# wgpu-native) and the platform-shlibs copy targets.

if(NOT (EXTERNAL_SHLIBS_AUDIO OR EXTERNAL_SHLIBS_GRAPHICS))
    return()
endif()

include(ExternalProject)

set(EXT_DEPS_INSTALL_DIR ${CMAKE_BINARY_DIR}/deps-install)
set(EXT_DEPS_C_FLAGS "")
set(EXT_DEPS_CXX_FLAGS "")

if(WIN32)
    string(APPEND EXT_DEPS_C_FLAGS " /DWIN32")
endif()

function(extempore_add_external name)
    cmake_parse_arguments(ARG "" "URL;URL_SHA256;FOLDER" "CMAKE_ARGS" ${ARGN})
    if(NOT ARG_URL_SHA256)
        message(FATAL_ERROR "extempore_add_external(${name}): URL_SHA256 is required")
    endif()
    ExternalProject_Add(${name}
        PREFIX ${name}
        URL ${ARG_URL}
        URL_HASH SHA256=${ARG_URL_SHA256}
        LIST_SEPARATOR |
        CMAKE_ARGS
            -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
            -DCMAKE_C_FLAGS=${EXT_DEPS_C_FLAGS}
            -DCMAKE_CXX_FLAGS=${EXT_DEPS_CXX_FLAGS}
            -DCMAKE_INSTALL_PREFIX=${EXT_DEPS_INSTALL_DIR}
            ${EXT_DEPS_TOOLCHAIN_ARGS}
            ${ARG_CMAKE_ARGS})
    set_target_properties(${name} PROPERTIES FOLDER ${ARG_FOLDER})
endfunction()

if(EXTERNAL_SHLIBS_AUDIO)
    extempore_add_external(portmidi
        URL https://github.com/PortMidi/portmidi/archive/${DEP_PORTMIDI_VERSION}.zip
        URL_SHA256 ${DEP_PORTMIDI_SHA256}
        FOLDER EXTERNAL_SHLIBS)

    if(UNIX)
        add_custom_target(external_shlibs_audio
            COMMENT "Copying audio shared libs to ${EXT_PLATFORM_SHLIBS_DIR}"
            DEPENDS portmidi
            COMMAND ${CMAKE_COMMAND} -E make_directory ${EXT_PLATFORM_SHLIBS_DIR}
            COMMAND ${CMAKE_COMMAND} -E copy libportmidi${CMAKE_SHARED_LIBRARY_SUFFIX} ${EXT_PLATFORM_SHLIBS_DIR}
            WORKING_DIRECTORY ${EXT_DEPS_INSTALL_DIR}/lib)
        set_target_properties(external_shlibs_audio PROPERTIES FOLDER EXTERNAL_SHLIBS)
    elseif(WIN32)
        add_custom_target(external_shlibs_audio
            COMMENT "Copying audio .dll and .lib files to ${EXT_PLATFORM_SHLIBS_DIR}"
            DEPENDS portmidi
            COMMAND ${CMAKE_COMMAND} -E make_directory ${EXT_PLATFORM_SHLIBS_DIR}
            COMMAND ${CMAKE_COMMAND} -E copy bin/portmidi.dll ${EXT_PLATFORM_SHLIBS_DIR}
            COMMAND ${CMAKE_COMMAND} -E copy lib/portmidi.lib ${EXT_PLATFORM_SHLIBS_DIR}
            WORKING_DIRECTORY ${EXT_DEPS_INSTALL_DIR})
        set_target_properties(external_shlibs_audio PROPERTIES FOLDER EXTERNAL_SHLIBS)
    endif()
endif()

##########################
# WebGPU graphics stack  #
##########################

if(EXTERNAL_SHLIBS_GRAPHICS)
    extempore_add_external(glfw3
        URL https://github.com/glfw/glfw/releases/download/${DEP_GLFW_VERSION}/glfw-${DEP_GLFW_VERSION}.zip
        URL_SHA256 ${DEP_GLFW_SHA256}
        FOLDER EXTERNAL_SHLIBS
        CMAKE_ARGS
            -DBUILD_SHARED_LIBS=ON
            -DGLFW_BUILD_EXAMPLES=OFF
            -DGLFW_BUILD_TESTS=OFF
            -DGLFW_BUILD_WAYLAND=OFF)

    # wgpu-native: prebuilt shared library (no build step)
    if(APPLE)
        if(CMAKE_SYSTEM_PROCESSOR MATCHES "arm64|aarch64")
            set(_wgpu_platform "macos-aarch64")
            set(_wgpu_sha256 ${DEP_WGPU_SHA256_MACOS_AARCH64})
        else()
            set(_wgpu_platform "macos-x86_64")
            set(_wgpu_sha256 ${DEP_WGPU_SHA256_MACOS_X86_64})
        endif()
        set(_wgpu_lib_name "libwgpu_native.dylib")
    elseif(WIN32)
        set(_wgpu_platform "windows-x86_64-msvc")
        set(_wgpu_sha256 ${DEP_WGPU_SHA256_WINDOWS_X86_64})
        set(_wgpu_lib_name "wgpu_native.dll")
    else()
        if(CMAKE_SYSTEM_PROCESSOR MATCHES "aarch64|arm64")
            set(_wgpu_platform "linux-aarch64")
            set(_wgpu_sha256 ${DEP_WGPU_SHA256_LINUX_AARCH64})
        else()
            set(_wgpu_platform "linux-x86_64")
            set(_wgpu_sha256 ${DEP_WGPU_SHA256_LINUX_X86_64})
        endif()
        set(_wgpu_lib_name "libwgpu_native.so")
    endif()

    set(_wgpu_url "https://github.com/gfx-rs/wgpu-native/releases/download/v${DEP_WGPU_VERSION}/wgpu-${_wgpu_platform}-release.zip")
    set(_wgpu_dir ${CMAKE_BINARY_DIR}/wgpu-native)

    ExternalProject_Add(wgpu_native_download
        PREFIX wgpu-native
        URL ${_wgpu_url}
        URL_HASH SHA256=${_wgpu_sha256}
        CONFIGURE_COMMAND ""
        BUILD_COMMAND ""
        INSTALL_COMMAND ${CMAKE_COMMAND} -E copy_directory <SOURCE_DIR>/include ${_wgpu_dir}/include
            COMMAND ${CMAKE_COMMAND} -E copy_directory <SOURCE_DIR>/lib ${_wgpu_dir}/lib)
    set_target_properties(wgpu_native_download PROPERTIES FOLDER EXTERNAL_SHLIBS)

    # xtmwebgpu helper library
    add_library(xtmwebgpu SHARED ${CMAKE_CURRENT_SOURCE_DIR}/extras/webgpu/xtmwebgpu.c)
    add_dependencies(xtmwebgpu wgpu_native_download glfw3)
    target_include_directories(xtmwebgpu PRIVATE
        ${CMAKE_CURRENT_SOURCE_DIR}/extras/webgpu
        ${_wgpu_dir}/include
        ${EXT_DEPS_INSTALL_DIR}/include)
    target_link_directories(xtmwebgpu PRIVATE
        ${_wgpu_dir}/lib
        ${EXT_DEPS_INSTALL_DIR}/lib)
    target_link_libraries(xtmwebgpu PRIVATE wgpu_native glfw)

    if(APPLE)
        set_source_files_properties(${CMAKE_CURRENT_SOURCE_DIR}/extras/webgpu/xtmwebgpu.c
            PROPERTIES LANGUAGE OBJC)
        target_link_libraries(xtmwebgpu PRIVATE
            "-framework QuartzCore"
            "-framework Metal"
            "-framework Foundation")
    elseif(UNIX)
        set_target_properties(xtmwebgpu PROPERTIES
            POSITION_INDEPENDENT_CODE ON
            BUILD_RPATH ${_wgpu_dir}/lib
            INSTALL_RPATH "\$ORIGIN")
    endif()

    set_target_properties(xtmwebgpu PROPERTIES
        LIBRARY_OUTPUT_DIRECTORY $<1:${EXT_PLATFORM_SHLIBS_DIR}>
        RUNTIME_OUTPUT_DIRECTORY $<1:${EXT_PLATFORM_SHLIBS_DIR}>
        FOLDER EXTERNAL_SHLIBS)

    if(UNIX)
        add_custom_target(external_shlibs_graphics
            COMMENT "Copying WebGPU graphics shared libs to ${EXT_PLATFORM_SHLIBS_DIR}"
            DEPENDS glfw3 wgpu_native_download xtmwebgpu
            COMMAND ${CMAKE_COMMAND} -E make_directory ${EXT_PLATFORM_SHLIBS_DIR}
            COMMAND ${CMAKE_COMMAND} -E copy libglfw${CMAKE_SHARED_LIBRARY_SUFFIX} ${EXT_PLATFORM_SHLIBS_DIR}
            COMMAND ${CMAKE_COMMAND} -E copy ${_wgpu_dir}/lib/${_wgpu_lib_name} ${EXT_PLATFORM_SHLIBS_DIR}
            WORKING_DIRECTORY ${EXT_DEPS_INSTALL_DIR}/lib)
        set_target_properties(external_shlibs_graphics PROPERTIES FOLDER EXTERNAL_SHLIBS)
    elseif(WIN32)
        add_custom_target(external_shlibs_graphics
            COMMENT "Copying WebGPU graphics shared libs to ${EXT_PLATFORM_SHLIBS_DIR}"
            DEPENDS glfw3 wgpu_native_download xtmwebgpu
            COMMAND ${CMAKE_COMMAND} -E make_directory ${EXT_PLATFORM_SHLIBS_DIR}
            COMMAND ${CMAKE_COMMAND} -E copy lib/glfw3.dll ${EXT_PLATFORM_SHLIBS_DIR}
            COMMAND ${CMAKE_COMMAND} -E copy lib/glfw3dll.lib ${EXT_PLATFORM_SHLIBS_DIR}/glfw3.lib
            COMMAND ${CMAKE_COMMAND} -E copy ${_wgpu_dir}/lib/${_wgpu_lib_name} ${EXT_PLATFORM_SHLIBS_DIR}
            COMMAND ${CMAKE_COMMAND} -E copy ${_wgpu_dir}/lib/wgpu_native.lib ${EXT_PLATFORM_SHLIBS_DIR}
            WORKING_DIRECTORY ${EXT_DEPS_INSTALL_DIR})
        set_target_properties(external_shlibs_graphics PROPERTIES FOLDER EXTERNAL_SHLIBS)
    endif()
endif()
