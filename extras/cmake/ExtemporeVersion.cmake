# Writes OUT (ExtemporeVersion.h) from the template IN. The version is VERSION
# when given (CI passes the tag, since its checkout is shallow), else
# `git describe` of SOURCE_DIR, else "unknown". The build runs this every time,
# so the binary names the commit it was built from; configure_file leaves OUT
# alone when the version hasn't changed.
if(NOT VERSION)
    find_package(Git QUIET)
    if(GIT_FOUND AND EXISTS "${SOURCE_DIR}/.git")
        execute_process(
            COMMAND ${GIT_EXECUTABLE} describe --tags --always --dirty
            WORKING_DIRECTORY ${SOURCE_DIR}
            OUTPUT_VARIABLE VERSION
            OUTPUT_STRIP_TRAILING_WHITESPACE
            ERROR_QUIET)
    endif()
endif()
if(NOT VERSION)
    set(VERSION "unknown")
endif()
set(EXTEMPORE_VERSION "${VERSION}")
configure_file(${IN} ${OUT} @ONLY)
