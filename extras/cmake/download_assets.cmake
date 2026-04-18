# Download the extempore-assets tarball from a GitHub release.
#
# Expected variables (set by the caller, e.g. via -D):
#   ASSETS_PATH     - absolute path where the assets/ directory should end up
#   ASSETS_VERSION  - release tag, e.g. "v0.9.0"
#
# The release asset must be named "extempore-assets.tar.xz" and, when
# extracted, must produce a top-level directory called "extempore-assets".

if(EXISTS ${ASSETS_PATH})
  message(WARNING "Directory ${ASSETS_PATH} already exists - skipping asset download step")
  return()
endif()

set(_url "https://github.com/extemporelang/extempore-assets/releases/download/${ASSETS_VERSION}/extempore-assets.tar.xz")
set(_archive "${CMAKE_BINARY_DIR}/extempore-assets.tar.xz")

message(STATUS "Downloading extempore-assets ${ASSETS_VERSION} from ${_url}")

file(DOWNLOAD
  ${_url}
  ${_archive}
  INACTIVITY_TIMEOUT 60
  SHOW_PROGRESS
  STATUS _status)

list(GET _status 0 _status_code)
if(NOT _status_code EQUAL 0)
  list(GET _status 1 _status_msg)
  message(FATAL_ERROR "Failed to download assets (${_status_code}): ${_status_msg}")
endif()

file(ARCHIVE_EXTRACT
  INPUT ${_archive}
  DESTINATION ${CMAKE_BINARY_DIR})

file(RENAME ${CMAKE_BINARY_DIR}/extempore-assets ${ASSETS_PATH})

message(STATUS "Assets extracted to ${ASSETS_PATH}")
