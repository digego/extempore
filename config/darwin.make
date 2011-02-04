OSX_FRAMEWORKS := \
	-framework cocoa \
	-framework coreaudio \
	-framework glut \
	-framework opengl \

PLATFORM_LIBS := $(OSX_FRAMEWORKS)

PLATFORM_CXXFLAGS :=
PLATFORM_LDFLAGS :=

PLATFORM_DEFINES := -DTARGET_OS_MAC
PLATFORM_CXX := g++
PLATFORM_LD := g++
