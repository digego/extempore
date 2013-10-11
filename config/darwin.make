OSX_FRAMEWORKS := \
	-framework Cocoa \
	-framework CoreAudio \
        -framework AudioToolbox \
        -framework AudioUnit \
	-framework GLUT \
	-framework OpenGL \

PLATFORM_LIBS := -lpcre

ifdef EXT_BOOST
PLATFORM_LIBS += -lboost_thread -lboost_system -lboost_filesystem -lpthread
else
PLATFORM_LIBS += -lpthread
endif

ifndef COREAUDIO
PLATFORM_LIBS += -lportaudio
endif

PLATFORM_LIBS += $(OSX_FRAMEWORKS)

PLATFORM_CXXFLAGS := -g -O3
PLATFORM_LDFLAGS :=

ifdef EXT_BUILD_GENERIC
PLATFORM_CXXFLAGS += -mtune=generic -mmacosx-version-min=10.6
endif

PLATFORM_DEFINES := -DTARGET_OS_MAC -DUSE_GLUT
PLATFORM_CXX := g++
PLATFORM_LD := g++
