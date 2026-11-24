# External shared library dependencies for Extempore
# Requires: EXTERNAL_SHLIBS_AUDIO, EXTERNAL_SHLIBS_GRAPHICS options
# Sets up: the library targets and the platform-shlibs copy targets. Libraries
# with a CMake build of their own are ExternalProjects; the few-file ones
# (kiss_fft, stb_image) are compiled here straight into platform-shlibs.

if(NOT (EXTERNAL_SHLIBS_AUDIO OR EXTERNAL_SHLIBS_GRAPHICS))
    return()
endif()

include(ExternalProject)
include(FetchContent)

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

# extempore_add_shlib(name sources...) compiles a shared library into
# platform-shlibs, exporting every symbol on Windows as the other platforms do.
# (The $<1:...> keeps multi-config generators from adding a config subdirectory.)
function(extempore_add_shlib name)
    add_library(${name} SHARED ${ARGN})
    set_target_properties(${name} PROPERTIES
        LIBRARY_OUTPUT_DIRECTORY $<1:${EXT_PLATFORM_SHLIBS_DIR}>
        RUNTIME_OUTPUT_DIRECTORY $<1:${EXT_PLATFORM_SHLIBS_DIR}>
        ARCHIVE_OUTPUT_DIRECTORY $<1:${EXT_PLATFORM_SHLIBS_DIR}>
        WINDOWS_EXPORT_ALL_SYMBOLS ON
        FOLDER EXTERNAL_SHLIBS)
endfunction()

if(EXTERNAL_SHLIBS_AUDIO)
    extempore_add_external(portmidi
        URL https://github.com/PortMidi/portmidi/archive/${DEP_PORTMIDI_VERSION}.zip
        URL_SHA256 ${DEP_PORTMIDI_SHA256}
        FOLDER EXTERNAL_SHLIBS)

    # SOURCE_SUBDIR names a directory without a CMakeLists.txt, so the
    # sources are fetched but kissfft's own build isn't added.
    FetchContent_Declare(kissfft
        URL https://github.com/mborgerding/kissfft/archive/refs/tags/${DEP_KISSFFT_VERSION}.tar.gz
        URL_HASH SHA256=${DEP_KISSFFT_SHA256}
        SOURCE_SUBDIR none)
    FetchContent_MakeAvailable(kissfft)
    extempore_add_shlib(kiss_fft
        ${kissfft_SOURCE_DIR}/kiss_fft.c
        ${kissfft_SOURCE_DIR}/kfc.c
        ${kissfft_SOURCE_DIR}/kiss_fftnd.c
        ${kissfft_SOURCE_DIR}/kiss_fftndr.c
        ${kissfft_SOURCE_DIR}/kiss_fftr.c)
    target_include_directories(kiss_fft PRIVATE ${kissfft_SOURCE_DIR})
    if(UNIX)
        target_link_libraries(kiss_fft PRIVATE m)
    endif()

    # No external codecs (FLAC, Vorbis, Opus, MPEG): each would be another
    # shared library to ship. libsndfile's cmake_minimum_required(3.1..3.18)
    # is below the 3.5 that CMake 4 still accepts, hence the policy floor, and
    # its CMake raises deprecation warnings that are upstream's to fix.
    set(_sndfile_args
        -DBUILD_SHARED_LIBS=ON
        -DBUILD_PROGRAMS=OFF
        -DBUILD_EXAMPLES=OFF
        -DENABLE_EXTERNAL_LIBS=OFF
        -DENABLE_MPEG=OFF
        -DBUILD_TESTING=OFF
        -DENABLE_CPACK=OFF
        -DENABLE_PACKAGE_CONFIG=OFF
        -DINSTALL_PKGCONFIG_MODULE=OFF
        -DCMAKE_POLICY_VERSION_MINIMUM=3.5
        -Wno-dev
        -Wno-deprecated)
    if(WIN32)
        list(APPEND _sndfile_args -DENABLE_STATIC_RUNTIME=OFF)
    endif()
    extempore_add_external(sndfile
        URL https://github.com/libsndfile/libsndfile/releases/download/${DEP_SNDFILE_VERSION}/libsndfile-${DEP_SNDFILE_VERSION}.tar.xz
        URL_SHA256 ${DEP_SNDFILE_SHA256}
        FOLDER EXTERNAL_SHLIBS
        CMAKE_ARGS ${_sndfile_args})

    if(UNIX)
        add_custom_target(external_shlibs_audio
            COMMENT "Copying audio shared libs to ${EXT_PLATFORM_SHLIBS_DIR}"
            DEPENDS sndfile kiss_fft portmidi
            COMMAND ${CMAKE_COMMAND} -E make_directory ${EXT_PLATFORM_SHLIBS_DIR}
            COMMAND ${CMAKE_COMMAND} -E copy libportmidi${CMAKE_SHARED_LIBRARY_SUFFIX} ${EXT_PLATFORM_SHLIBS_DIR}
            COMMAND ${CMAKE_COMMAND} -E copy libsndfile${CMAKE_SHARED_LIBRARY_SUFFIX} ${EXT_PLATFORM_SHLIBS_DIR}
            WORKING_DIRECTORY ${EXT_DEPS_INSTALL_DIR}/lib)
        set_target_properties(external_shlibs_audio PROPERTIES FOLDER EXTERNAL_SHLIBS)
    elseif(WIN32)
        add_custom_target(external_shlibs_audio
            COMMENT "Copying audio .dll and .lib files to ${EXT_PLATFORM_SHLIBS_DIR}"
            DEPENDS sndfile kiss_fft portmidi
            COMMAND ${CMAKE_COMMAND} -E make_directory ${EXT_PLATFORM_SHLIBS_DIR}
            COMMAND ${CMAKE_COMMAND} -E copy bin/portmidi.dll ${EXT_PLATFORM_SHLIBS_DIR}
            COMMAND ${CMAKE_COMMAND} -E copy lib/portmidi.lib ${EXT_PLATFORM_SHLIBS_DIR}
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
    FetchContent_Declare(stb
        URL https://github.com/nothings/stb/archive/${DEP_STB_COMMIT}.tar.gz
        URL_HASH SHA256=${DEP_STB_SHA256}
        SOURCE_SUBDIR none)
    FetchContent_MakeAvailable(stb)
    extempore_add_shlib(stb_image ${CMAKE_CURRENT_SOURCE_DIR}/extras/stb/stb_image.c)
    target_include_directories(stb_image PRIVATE ${stb_SOURCE_DIR})
    if(UNIX)
        target_link_libraries(stb_image PRIVATE m)
    endif()

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
            DEPENDS glfw3 stb_image wgpu_native_download xtmwebgpu
            COMMAND ${CMAKE_COMMAND} -E make_directory ${EXT_PLATFORM_SHLIBS_DIR}
            COMMAND ${CMAKE_COMMAND} -E copy libglfw${CMAKE_SHARED_LIBRARY_SUFFIX} ${EXT_PLATFORM_SHLIBS_DIR}
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
            COMMAND ${CMAKE_COMMAND} -E copy ${_wgpu_dir}/lib/${_wgpu_lib_name} ${EXT_PLATFORM_SHLIBS_DIR}
            COMMAND ${CMAKE_COMMAND} -E copy ${_wgpu_dir}/lib/wgpu_native.lib ${EXT_PLATFORM_SHLIBS_DIR}
            WORKING_DIRECTORY ${EXT_DEPS_INSTALL_DIR})
        set_target_properties(external_shlibs_graphics PROPERTIES FOLDER EXTERNAL_SHLIBS)
    endif()
endif()
