set(ASSETS_DOWNLOAD_PATH ${CMAKE_BINARY_DIR}/assets.tar.gz)
set(ASSETS_GIT_REF 0c9f32c)

if(EXISTS ${CMAKE_SOURCE_DIR}/assets)
  message(FATAL_ERROR "Directory ${CMAKE_SOURCE_DIR}/assets already exists. You must delete or move it so that the build process can download the latest assets folder from GitHub.")
endif()

# deleting stuff in the build directory is fair game from here (since we're already protected against the problem of in-tree builds with the previous fatal error)
if(EXISTS ${CMAKE_BINARY_DIR}/assets)
  file(REMOVE_RECURSE ${CMAKE_BINARY_DIR}/assets)
endif()

message(STATUS "https://github.com/extemporelang/extempore-assets" )
file(DOWNLOAD
  https://api.github.com/repos/extemporelang/extempore-assets/tarball/${ASSETS_GIT_REF}
  ${ASSETS_DOWNLOAD_PATH}
  # options
  INACTIVITY_TIMEOUT 60
  SHOW_PROGRESS)

# untar it with CMake's built-in untar command
execute_process(COMMAND ${CMAKE_COMMAND} -E tar xz ${ASSETS_DOWNLOAD_PATH})

# rename folder to just "assets", move into source directory
file(RENAME
  ${CMAKE_BINARY_DIR}/extemporelang-extempore-assets-${ASSETS_GIT_REF}
  ${CMAKE_SOURCE_DIR}/assets)

message(STATUS "moving assets to ${CMAKE_SOURCE_DIR}/assets")
