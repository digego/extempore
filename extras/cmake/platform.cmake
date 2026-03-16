# Platform detection for Extempore
# Sets: EXTEMPORE_SYSTEM_NAME, EXTEMPORE_SYSTEM_VERSION, EXTEMPORE_SYSTEM_ARCHITECTURE

function(extempore_detect_platform)
    if(UNIX)
        find_program(UNAME_PROGRAM uname)
        execute_process(COMMAND ${UNAME_PROGRAM} -m
            OUTPUT_VARIABLE UNAME_MACHINE_NAME
            OUTPUT_STRIP_TRAILING_WHITESPACE)
        execute_process(COMMAND ${UNAME_PROGRAM} -r
            OUTPUT_VARIABLE UNAME_OS_RELEASE
            OUTPUT_STRIP_TRAILING_WHITESPACE)
        execute_process(COMMAND ${UNAME_PROGRAM} -s
            OUTPUT_VARIABLE UNAME_OS_NAME
            OUTPUT_STRIP_TRAILING_WHITESPACE)
        set(UNAME_MACHINE_NAME ${UNAME_MACHINE_NAME} PARENT_SCOPE)
    endif()

    if(APPLE)
        set(EXTEMPORE_SYSTEM_NAME "osx" PARENT_SCOPE)
        execute_process(COMMAND sw_vers -productVersion
            OUTPUT_VARIABLE _version
            OUTPUT_STRIP_TRAILING_WHITESPACE)
        string(REGEX MATCH "^[0-9]+\\.?[0-9]*" _version ${_version})
        set(EXTEMPORE_SYSTEM_VERSION ${_version} PARENT_SCOPE)
        set(EXTEMPORE_SYSTEM_ARCHITECTURE ${UNAME_MACHINE_NAME} PARENT_SCOPE)
    elseif(UNIX)
        execute_process(COMMAND lsb_release -is
            OUTPUT_VARIABLE _name
            OUTPUT_STRIP_TRAILING_WHITESPACE
            ERROR_QUIET)
        if(NOT _name)
            set(_name ${UNAME_OS_NAME})
        endif()
        set(EXTEMPORE_SYSTEM_NAME ${_name} PARENT_SCOPE)
        set(EXTEMPORE_SYSTEM_VERSION ${UNAME_OS_RELEASE} PARENT_SCOPE)
        set(EXTEMPORE_SYSTEM_ARCHITECTURE ${UNAME_MACHINE_NAME} PARENT_SCOPE)
    elseif(WIN32)
        set(EXTEMPORE_SYSTEM_NAME "Windows" PARENT_SCOPE)
        string(REGEX MATCH "^[0-9]+" _version ${CMAKE_SYSTEM_VERSION})
        if(_version LESS 10)
            math(EXPR _version "${_version} + 1")
        endif()
        set(EXTEMPORE_SYSTEM_VERSION ${_version} PARENT_SCOPE)
        set(EXTEMPORE_SYSTEM_ARCHITECTURE ${CMAKE_SYSTEM_PROCESSOR} PARENT_SCOPE)
    else()
        message(FATAL_ERROR "Sorry, Extempore isn't supported on this platform - macOS, Linux & Windows only.")
    endif()
endfunction()
