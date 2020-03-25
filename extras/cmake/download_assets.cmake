# set(ASSETS_PATH ${CMAKE_BINARY_DIR}/assets.tar.gz)
# set(ASSETS_GIT_REF 0c9f32c)

message(STATUS "Downloading assets from https://github.com/extemporelang/extempore-assets")

if(EXISTS ${ASSETS_PATH})
  message(FATAL_ERROR "Directory ${ASSETS_PATH} already exists - you must delete or move it.")
endif()

file(DOWNLOAD
  https://api.github.com/repos/extemporelang/extempore-assets/tarball/${ASSETS_GIT_REF}
  assets.tar.gz
  # options
  INACTIVITY_TIMEOUT 60
  SHOW_PROGRESS)

# untar it with CMake's built-in untar command
execute_process(COMMAND ${CMAKE_COMMAND} -E tar xz assets.tar.gz)

# rename folder to just "assets", move into source directory
file(RENAME
  extemporelang-extempore-assets-${ASSETS_GIT_REF}
  ${ASSETS_PATH})

message(STATUS "moved assets to ${ASSETS_PATH}")
