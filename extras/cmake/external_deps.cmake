# External shared library dependencies for Extempore
# Requires: EXTERNAL_SHLIBS_AUDIO, EXTERNAL_SHLIBS_GRAPHICS, EXTERNAL_SHLIBS_GRAPHICS_OPENGL options
# Sets up: ExternalProject targets and platform-shlibs copy targets

if(NOT (EXTERNAL_SHLIBS_AUDIO OR EXTERNAL_SHLIBS_GRAPHICS OR EXTERNAL_SHLIBS_GRAPHICS_OPENGL))
    return()
endif()

include(ExternalProject)

set(EXT_DEPS_INSTALL_DIR ${CMAKE_BINARY_DIR}/deps-install)
set(EXT_PLATFORM_SHLIBS_DIR ${CMAKE_CURRENT_SOURCE_DIR}/libs/platform-shlibs)
set(EXT_DEPS_C_FLAGS "")
set(EXT_DEPS_CXX_FLAGS "")

if(EXT_DYLIB)
    string(APPEND EXT_DEPS_C_FLAGS " -fPIC")
    string(APPEND EXT_DEPS_CXX_FLAGS " -fPIC")
endif()

if(WIN32)
    string(APPEND EXT_DEPS_C_FLAGS " /DWIN32")
endif()

function(extempore_add_external name)
    cmake_parse_arguments(ARG "" "URL;URL_MD5;FOLDER" "CMAKE_ARGS" ${ARGN})
    ExternalProject_Add(${name}
        PREFIX ${name}
        URL ${ARG_URL}
        URL_MD5 ${ARG_URL_MD5}
        CMAKE_ARGS
            -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
            -DCMAKE_C_FLAGS=${EXT_DEPS_C_FLAGS}
            -DCMAKE_CXX_FLAGS=${EXT_DEPS_CXX_FLAGS}
            -DCMAKE_INSTALL_PREFIX=${EXT_DEPS_INSTALL_DIR}
            -DCMAKE_POLICY_VERSION_MINIMUM=3.5
            ${ARG_CMAKE_ARGS})
    set_target_properties(${name} PROPERTIES FOLDER ${ARG_FOLDER})
endfunction()

if(EXTERNAL_SHLIBS_AUDIO)
    extempore_add_external(portmidi
        URL https://github.com/PortMidi/portmidi/archive/${DEP_PORTMIDI_VERSION}.zip
        URL_MD5 ${DEP_PORTMIDI_MD5}
        FOLDER EXTERNAL_SHLIBS)

    extempore_add_external(rtmidi
        URL https://github.com/thestk/rtmidi/archive/${DEP_RTMIDI_VERSION}.zip
        URL_MD5 ${DEP_RTMIDI_MD5}
        FOLDER EXTERNAL_SHLIBS
        CMAKE_ARGS -DRTMIDI_BUILD_TESTING=OFF
            $<$<BOOL:${WIN32}>:-DCMAKE_INSTALL_LIBDIR=${EXT_DEPS_INSTALL_DIR}>
            $<$<BOOL:${WIN32}>:-DCMAKE_INSTALL_BINDIR=${EXT_DEPS_INSTALL_DIR}>)

    extempore_add_external(kiss_fft
        URL https://github.com/extemporelang/kiss_fft/archive/${DEP_KISS_FFT_VERSION}.zip
        FOLDER EXTERNAL_SHLIBS)

    extempore_add_external(sndfile
        URL https://github.com/erikd/libsndfile/archive/${DEP_SNDFILE_COMMIT}.zip
        FOLDER EXTERNAL_SHLIBS
        CMAKE_ARGS
            -DBUILD_SHARED_LIBS=ON
            -DBUILD_PROGRAMS=OFF
            -DBUILD_EXAMPLES=OFF
            -DENABLE_EXTERNAL_LIBS=OFF
            -DBUILD_TESTING=OFF
            -DENABLE_CPACK=OFF
            -DENABLE_PACKAGE_CONFIG=OFF
            $<$<BOOL:${WIN32}>:-DENABLE_STATIC_RUNTIME=OFF>)

    if(UNIX)
        add_custom_target(external_shlibs_audio
            COMMENT "Copying audio shared libs to ${EXT_PLATFORM_SHLIBS_DIR}"
            DEPENDS sndfile kiss_fft portmidi rtmidi
            COMMAND ${CMAKE_COMMAND} -E make_directory ${EXT_PLATFORM_SHLIBS_DIR}
            COMMAND ${CMAKE_COMMAND} -E copy libkiss_fft${CMAKE_SHARED_LIBRARY_SUFFIX} ${EXT_PLATFORM_SHLIBS_DIR}
            COMMAND ${CMAKE_COMMAND} -E copy libportmidi${CMAKE_SHARED_LIBRARY_SUFFIX} ${EXT_PLATFORM_SHLIBS_DIR}
            COMMAND ${CMAKE_COMMAND} -E copy librtmidi${CMAKE_SHARED_LIBRARY_SUFFIX} ${EXT_PLATFORM_SHLIBS_DIR}
            COMMAND ${CMAKE_COMMAND} -E copy libsndfile${CMAKE_SHARED_LIBRARY_SUFFIX} ${EXT_PLATFORM_SHLIBS_DIR}
            WORKING_DIRECTORY ${EXT_DEPS_INSTALL_DIR}/lib)
        set_target_properties(external_shlibs_audio PROPERTIES FOLDER EXTERNAL_SHLIBS)
    elseif(WIN32)
        add_custom_target(external_shlibs_audio
            COMMENT "Copying audio .dll and .lib files to ${EXT_PLATFORM_SHLIBS_DIR}"
            DEPENDS sndfile kiss_fft portmidi rtmidi
            COMMAND ${CMAKE_COMMAND} -E make_directory ${EXT_PLATFORM_SHLIBS_DIR}
            COMMAND ${CMAKE_COMMAND} -E copy lib/kiss_fft.dll ${EXT_PLATFORM_SHLIBS_DIR}
            COMMAND ${CMAKE_COMMAND} -E copy lib/kiss_fft.lib ${EXT_PLATFORM_SHLIBS_DIR}
            COMMAND ${CMAKE_COMMAND} -E copy bin/portmidi.dll ${EXT_PLATFORM_SHLIBS_DIR}
            COMMAND ${CMAKE_COMMAND} -E copy lib/portmidi.lib ${EXT_PLATFORM_SHLIBS_DIR}
            COMMAND ${CMAKE_COMMAND} -E copy rtmidi.dll ${EXT_PLATFORM_SHLIBS_DIR}
            COMMAND ${CMAKE_COMMAND} -E copy rtmidi.lib ${EXT_PLATFORM_SHLIBS_DIR}
            COMMAND ${CMAKE_COMMAND} -E copy bin/sndfile.dll ${EXT_PLATFORM_SHLIBS_DIR}
            COMMAND ${CMAKE_COMMAND} -E copy lib/sndfile.lib ${EXT_PLATFORM_SHLIBS_DIR}
            WORKING_DIRECTORY ${EXT_DEPS_INSTALL_DIR})
        set_target_properties(external_shlibs_audio PROPERTIES FOLDER EXTERNAL_SHLIBS)
    endif()
endif()

##########################
# WebGPU graphics stack  #
##########################

if(EXTERNAL_SHLIBS_GRAPHICS)
    extempore_add_external(stb_image
        URL https://github.com/extemporelang/stb/archive/${DEP_STB_COMMIT}.zip
        FOLDER EXTERNAL_SHLIBS)

    extempore_add_external(glfw3
        URL https://github.com/glfw/glfw/releases/download/${DEP_GLFW_VERSION}/glfw-${DEP_GLFW_VERSION}.zip
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
        else()
            set(_wgpu_platform "macos-x86_64")
        endif()
        set(_wgpu_lib_name "libwgpu_native.dylib")
    elseif(WIN32)
        set(_wgpu_platform "windows-x86_64-msvc")
        set(_wgpu_lib_name "wgpu_native.dll")
    else()
        if(CMAKE_SYSTEM_PROCESSOR MATCHES "aarch64|arm64")
            set(_wgpu_platform "linux-aarch64")
        else()
            set(_wgpu_platform "linux-x86_64")
        endif()
        set(_wgpu_lib_name "libwgpu_native.so")
    endif()

    set(_wgpu_url "https://github.com/gfx-rs/wgpu-native/releases/download/v${DEP_WGPU_VERSION}/wgpu-${_wgpu_platform}-release.zip")
    set(_wgpu_dir ${CMAKE_BINARY_DIR}/wgpu-native)

    ExternalProject_Add(wgpu_native_download
        PREFIX wgpu-native
        URL ${_wgpu_url}
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
        target_compile_options(xtmwebgpu PRIVATE -x objective-c)
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
        LIBRARY_OUTPUT_DIRECTORY ${EXT_PLATFORM_SHLIBS_DIR}
        RUNTIME_OUTPUT_DIRECTORY ${EXT_PLATFORM_SHLIBS_DIR}
        FOLDER EXTERNAL_SHLIBS)

    if(UNIX)
        add_custom_target(external_shlibs_graphics
            COMMENT "Copying WebGPU graphics shared libs to ${EXT_PLATFORM_SHLIBS_DIR}"
            DEPENDS glfw3 stb_image wgpu_native_download xtmwebgpu
            COMMAND ${CMAKE_COMMAND} -E make_directory ${EXT_PLATFORM_SHLIBS_DIR}
            COMMAND ${CMAKE_COMMAND} -E copy libglfw${CMAKE_SHARED_LIBRARY_SUFFIX} ${EXT_PLATFORM_SHLIBS_DIR}
            COMMAND ${CMAKE_COMMAND} -E copy libstb_image${CMAKE_SHARED_LIBRARY_SUFFIX} ${EXT_PLATFORM_SHLIBS_DIR}
            COMMAND ${CMAKE_COMMAND} -E copy ${_wgpu_dir}/lib/${_wgpu_lib_name} ${EXT_PLATFORM_SHLIBS_DIR}
            WORKING_DIRECTORY ${EXT_DEPS_INSTALL_DIR}/lib)
        set_target_properties(external_shlibs_graphics PROPERTIES FOLDER EXTERNAL_SHLIBS)
    elseif(WIN32)
        add_custom_target(external_shlibs_graphics
            COMMENT "Copying WebGPU graphics shared libs to ${EXT_PLATFORM_SHLIBS_DIR}"
            DEPENDS glfw3 stb_image wgpu_native_download xtmwebgpu
            COMMAND ${CMAKE_COMMAND} -E make_directory ${EXT_PLATFORM_SHLIBS_DIR}
            COMMAND ${CMAKE_COMMAND} -E copy lib/glfw3.dll ${EXT_PLATFORM_SHLIBS_DIR}
            COMMAND ${CMAKE_COMMAND} -E copy lib/glfw3dll.lib ${EXT_PLATFORM_SHLIBS_DIR}/glfw3.lib
            COMMAND ${CMAKE_COMMAND} -E copy lib/stb_image.dll ${EXT_PLATFORM_SHLIBS_DIR}
            COMMAND ${CMAKE_COMMAND} -E copy lib/stb_image.lib ${EXT_PLATFORM_SHLIBS_DIR}
            COMMAND ${CMAKE_COMMAND} -E copy ${_wgpu_dir}/lib/${_wgpu_lib_name} ${EXT_PLATFORM_SHLIBS_DIR}
            COMMAND ${CMAKE_COMMAND} -E copy ${_wgpu_dir}/lib/wgpu_native.lib ${EXT_PLATFORM_SHLIBS_DIR}
            WORKING_DIRECTORY ${EXT_DEPS_INSTALL_DIR})
        set_target_properties(external_shlibs_graphics PROPERTIES FOLDER EXTERNAL_SHLIBS)
    endif()
endif()

################################
# Legacy OpenGL graphics stack #
################################

if(EXTERNAL_SHLIBS_GRAPHICS_OPENGL)
    if(NOT TARGET stb_image)
        extempore_add_external(stb_image
            URL https://github.com/extemporelang/stb/archive/${DEP_STB_COMMIT}.zip
            FOLDER EXTERNAL_SHLIBS)
    endif()

    if(NOT TARGET glfw3)
        extempore_add_external(glfw3
            URL https://github.com/glfw/glfw/releases/download/${DEP_GLFW_VERSION}/glfw-${DEP_GLFW_VERSION}.zip
            FOLDER EXTERNAL_SHLIBS
            CMAKE_ARGS
                -DBUILD_SHARED_LIBS=ON
                -DGLFW_BUILD_EXAMPLES=OFF
                -DGLFW_BUILD_TESTS=OFF)
    endif()

    extempore_add_external(nanovg
        URL https://github.com/extemporelang/nanovg/archive/${DEP_NANOVG_COMMIT}.tar.gz
        FOLDER EXTERNAL_SHLIBS
        CMAKE_ARGS -DEXTEMPORE_LIB_PATH=${CMAKE_CURRENT_SOURCE_DIR}/libs/platform-shlibs/extempore.lib)
    add_dependencies(nanovg extempore)

    extempore_add_external(assimp
        URL https://github.com/assimp/assimp/archive/v${DEP_ASSIMP_VERSION}.zip
        FOLDER EXTERNAL_SHLIBS
        CMAKE_ARGS
            -DCMAKE_DEBUG_POSTFIX=
            -DASSIMP_BUILD_ASSIMP_TOOLS=OFF
            -DASSIMP_BUILD_SAMPLES=OFF
            -DASSIMP_BUILD_TESTS=OFF)

    if(UNIX)
        set(_opengl_deps assimp nanovg)
        set(_opengl_copy_commands
            COMMAND ${CMAKE_COMMAND} -E copy libassimp${CMAKE_SHARED_LIBRARY_SUFFIX} ${EXT_PLATFORM_SHLIBS_DIR}
            COMMAND ${CMAKE_COMMAND} -E copy libnanovg${CMAKE_SHARED_LIBRARY_SUFFIX} ${EXT_PLATFORM_SHLIBS_DIR})

        if(NOT EXTERNAL_SHLIBS_GRAPHICS)
            list(APPEND _opengl_deps glfw3 stb_image)
            list(APPEND _opengl_copy_commands
                COMMAND ${CMAKE_COMMAND} -E copy libglfw${CMAKE_SHARED_LIBRARY_SUFFIX} ${EXT_PLATFORM_SHLIBS_DIR}
                COMMAND ${CMAKE_COMMAND} -E copy libstb_image${CMAKE_SHARED_LIBRARY_SUFFIX} ${EXT_PLATFORM_SHLIBS_DIR})
        endif()

        add_custom_target(external_shlibs_graphics_opengl
            COMMENT "Copying OpenGL graphics shared libs to ${EXT_PLATFORM_SHLIBS_DIR}"
            DEPENDS ${_opengl_deps}
            COMMAND ${CMAKE_COMMAND} -E make_directory ${EXT_PLATFORM_SHLIBS_DIR}
            ${_opengl_copy_commands}
            WORKING_DIRECTORY ${EXT_DEPS_INSTALL_DIR}/lib)
        set_target_properties(external_shlibs_graphics_opengl PROPERTIES FOLDER EXTERNAL_SHLIBS)
    elseif(WIN32)
        set(_opengl_deps assimp nanovg)
        set(_opengl_copy_commands
            COMMAND ${CMAKE_COMMAND} -E copy bin/assimp-vc130-mt.dll ${EXT_PLATFORM_SHLIBS_DIR}
            COMMAND ${CMAKE_COMMAND} -E copy lib/assimp-vc130-mt.lib ${EXT_PLATFORM_SHLIBS_DIR}
            COMMAND ${CMAKE_COMMAND} -E copy lib/nanovg.dll ${EXT_PLATFORM_SHLIBS_DIR}
            COMMAND ${CMAKE_COMMAND} -E copy lib/nanovg.lib ${EXT_PLATFORM_SHLIBS_DIR})

        if(NOT EXTERNAL_SHLIBS_GRAPHICS)
            list(APPEND _opengl_deps glfw3 stb_image)
            list(APPEND _opengl_copy_commands
                COMMAND ${CMAKE_COMMAND} -E copy lib/glfw3.dll ${EXT_PLATFORM_SHLIBS_DIR}
                COMMAND ${CMAKE_COMMAND} -E copy lib/glfw3dll.lib ${EXT_PLATFORM_SHLIBS_DIR}/glfw3.lib
                COMMAND ${CMAKE_COMMAND} -E copy lib/stb_image.dll ${EXT_PLATFORM_SHLIBS_DIR}
                COMMAND ${CMAKE_COMMAND} -E copy lib/stb_image.lib ${EXT_PLATFORM_SHLIBS_DIR})
        endif()

        add_custom_target(external_shlibs_graphics_opengl
            COMMENT "Copying OpenGL graphics .dll and .lib files to ${EXT_PLATFORM_SHLIBS_DIR}"
            DEPENDS ${_opengl_deps}
            COMMAND ${CMAKE_COMMAND} -E make_directory ${EXT_PLATFORM_SHLIBS_DIR}
            ${_opengl_copy_commands}
            WORKING_DIRECTORY ${EXT_DEPS_INSTALL_DIR})
        set_target_properties(external_shlibs_graphics_opengl PROPERTIES FOLDER EXTERNAL_SHLIBS)
    endif()
endif()
