OSX_FRAMEWORKS := \
	-framework cocoa \
	-framework coreaudio \
	-framework glut \
	-framework opengl \

PLATFORM_LIBS := $(OSX_FRAMEWORKS)

PLATFORM_CXXFLAGS := -g -O0
PLATFORM_LDFLAGS :=

PLATFORM_DEFINES := -DTARGET_OS_MAC -DUSE_GLUT
PLATFORM_CXX := g++
PLATFORM_LD := g++
