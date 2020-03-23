set(ASSETS_DOWNLOAD_PATH ${CMAKE_BINARY_DIR}/assets.tar.gz)
set(ASSETS_GIT_REF 0c9f32c)

message(STATUS "https://github.com/extemporelang/extempore-assets" )
file(DOWNLOAD
  https://api.github.com/repos/extemporelang/extempore-assets/tarball/${ASSETS_GIT_REF}
  ${ASSETS_DOWNLOAD_PATH}
  # options
  INACTIVITY_TIMEOUT 60
  SHOW_PROGRESS)

# untar it with CMake's built-in untar command
execute_process(COMMAND ${CMAKE_COMMAND} -E tar xz ${ASSETS_DOWNLOAD_PATH})

# if there's a previously downloaded assets file in in your build directory (or
# wherever you're running cmake) then remove it
# NOTE: this is bad for in-tree builds. probably should add a warning or something.
if(EXISTS ${CMAKE_BINARY_DIR}/assets)
  file(REMOVE_RECURSE ${CMAKE_BINARY_DIR}/assets)
endif()

# rename the folder to just "assets"
file(RENAME
  ${CMAKE_BINARY_DIR}/extemporelang-extempore-assets-${ASSETS_GIT_REF}
  ${CMAKE_BINARY_DIR}/assets)
