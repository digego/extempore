# assumes that these variables have been set by the main CMakeLists.txt, e.g.

# don't set this in this file - CMAKE_SOURCE_DIR isn't what you think it is when
# running a command with -P

# set(ASSETS_PATH ${CMAKE_SOURCE_DIR}/assets)

# also set a (short) git ref for the asset tarball

# set(ASSETS_GIT_REF 0c9f32c)

if(EXISTS ${ASSETS_PATH})
  message(WARNING "Directory ${ASSETS_PATH} already exists - skipping asset dowload step")
else()
  message(STATUS "Downloading assets from https://github.com/extemporelang/extempore-assets")

  file(DOWNLOAD
	https://api.github.com/repos/extemporelang/extempore-assets/tarball/${ASSETS_GIT_REF}
	${CMAKE_BINARY_DIR}/assets.tar.gz
	# options
	INACTIVITY_TIMEOUT 60
	SHOW_PROGRESS)

  # untar it with CMake's built-in untar command
  execute_process(COMMAND ${CMAKE_COMMAND} -E tar xz ${CMAKE_BINARY_DIR}/assets.tar.gz)

  # rename folder to just "assets", move into source directory
  file(RENAME
	extemporelang-extempore-assets-${ASSETS_GIT_REF}
	${ASSETS_PATH})

  message(STATUS "moved assets to ${ASSETS_PATH}")

endif()
