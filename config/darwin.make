OSX_FRAMEWORKS := \
	-framework Cocoa \
	-framework CoreAudio \
  -framework AudioToolbox \
	-framework AudioUnit

PLATFORM_LIBS := -lpcre

ifdef EXT_BOOST
PLATFORM_LIBS += -lboost_thread -lboost_system -lboost_filesystem -lpthread
else
PLATFORM_LIBS += -lpthread
endif

PLATFORM_LIBS += -lportaudio

PLATFORM_LIBS += $(OSX_FRAMEWORKS)

PLATFORM_CXXFLAGS := -g -O3
PLATFORM_LDFLAGS :=

PLATFORM_DEFINES := -DTARGET_OS_MAC
PLATFORM_CXX := g++
PLATFORM_LD := g++
