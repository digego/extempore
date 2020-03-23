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

# rename the folder to just "assets"
file(RENAME "extemporelang-extempore-assets-${ASSETS_GIT_REF}" assets)

# remove the compressed file at the end
file(REMOVE ${ASSETS_DOWNLOAD_PATH})
