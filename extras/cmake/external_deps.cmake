# External shared library dependencies for Extempore
# Requires: EXTERNAL_SHLIBS_AUDIO, EXTERNAL_SHLIBS_GRAPHICS options
# Sets up: ExternalProject targets and platform-shlibs copy targets

if(NOT (EXTERNAL_SHLIBS_AUDIO OR EXTERNAL_SHLIBS_GRAPHICS))
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

if(EXTERNAL_SHLIBS_GRAPHICS)
    extempore_add_external(nanovg
        URL https://github.com/extemporelang/nanovg/archive/${DEP_NANOVG_COMMIT}.tar.gz
        FOLDER EXTERNAL_SHLIBS
        CMAKE_ARGS -DEXTEMPORE_LIB_PATH=${CMAKE_CURRENT_SOURCE_DIR}/libs/platform-shlibs/extempore.lib)
    add_dependencies(nanovg extempore)

    extempore_add_external(stb_image
        URL https://github.com/extemporelang/stb/archive/${DEP_STB_COMMIT}.zip
        FOLDER EXTERNAL_SHLIBS)

    extempore_add_external(glfw3
        URL https://github.com/glfw/glfw/releases/download/${DEP_GLFW_VERSION}/glfw-${DEP_GLFW_VERSION}.zip
        FOLDER EXTERNAL_SHLIBS
        CMAKE_ARGS
            -DBUILD_SHARED_LIBS=ON
            -DGLFW_BUILD_EXAMPLES=OFF
            -DGLFW_BUILD_TESTS=OFF)

    extempore_add_external(assimp
        URL https://github.com/assimp/assimp/archive/v${DEP_ASSIMP_VERSION}.zip
        FOLDER EXTERNAL_SHLIBS
        CMAKE_ARGS
            -DCMAKE_DEBUG_POSTFIX=
            -DASSIMP_BUILD_ASSIMP_TOOLS=OFF
            -DASSIMP_BUILD_SAMPLES=OFF
            -DASSIMP_BUILD_TESTS=OFF)

    if(UNIX)
        add_custom_target(external_shlibs_graphics
            COMMENT "Copying graphics shared libs to ${EXT_PLATFORM_SHLIBS_DIR}"
            DEPENDS assimp glfw3 stb_image nanovg
            COMMAND ${CMAKE_COMMAND} -E make_directory ${EXT_PLATFORM_SHLIBS_DIR}
            COMMAND ${CMAKE_COMMAND} -E copy libassimp${CMAKE_SHARED_LIBRARY_SUFFIX} ${EXT_PLATFORM_SHLIBS_DIR}
            COMMAND ${CMAKE_COMMAND} -E copy libglfw${CMAKE_SHARED_LIBRARY_SUFFIX} ${EXT_PLATFORM_SHLIBS_DIR}
            COMMAND ${CMAKE_COMMAND} -E copy libnanovg${CMAKE_SHARED_LIBRARY_SUFFIX} ${EXT_PLATFORM_SHLIBS_DIR}
            COMMAND ${CMAKE_COMMAND} -E copy libstb_image${CMAKE_SHARED_LIBRARY_SUFFIX} ${EXT_PLATFORM_SHLIBS_DIR}
            WORKING_DIRECTORY ${EXT_DEPS_INSTALL_DIR}/lib)
        set_target_properties(external_shlibs_graphics PROPERTIES FOLDER EXTERNAL_SHLIBS)
    elseif(WIN32)
        add_custom_target(external_shlibs_graphics
            COMMENT "Copying graphics .dll and .lib files to ${EXT_PLATFORM_SHLIBS_DIR}"
            DEPENDS assimp glfw3 stb_image nanovg
            COMMAND ${CMAKE_COMMAND} -E make_directory ${EXT_PLATFORM_SHLIBS_DIR}
            COMMAND ${CMAKE_COMMAND} -E copy bin/assimp-vc130-mt.dll ${EXT_PLATFORM_SHLIBS_DIR}
            COMMAND ${CMAKE_COMMAND} -E copy lib/assimp-vc130-mt.lib ${EXT_PLATFORM_SHLIBS_DIR}
            COMMAND ${CMAKE_COMMAND} -E copy lib/glfw3.dll ${EXT_PLATFORM_SHLIBS_DIR}
            COMMAND ${CMAKE_COMMAND} -E copy lib/glfw3dll.lib ${EXT_PLATFORM_SHLIBS_DIR}/glfw3.lib
            COMMAND ${CMAKE_COMMAND} -E copy lib/nanovg.dll ${EXT_PLATFORM_SHLIBS_DIR}
            COMMAND ${CMAKE_COMMAND} -E copy lib/nanovg.lib ${EXT_PLATFORM_SHLIBS_DIR}
            COMMAND ${CMAKE_COMMAND} -E copy lib/stb_image.dll ${EXT_PLATFORM_SHLIBS_DIR}
            COMMAND ${CMAKE_COMMAND} -E copy lib/stb_image.lib ${EXT_PLATFORM_SHLIBS_DIR}
            WORKING_DIRECTORY ${EXT_DEPS_INSTALL_DIR})
        set_target_properties(external_shlibs_graphics PROPERTIES FOLDER EXTERNAL_SHLIBS)
    endif()
endif()
