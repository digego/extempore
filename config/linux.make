PLATFORM_LIBS = -ldl -lm

ifdef EXT_BOOST
PLATFORM_LIBS += -lboost_thread -lboost_system -lboost_filesystem -pthread
else
PLATFORM_LIBS += -pthread
endif

ifdef JACK_AUDIO
PLATFORM_LIBS += -ljack
else
PLATFORM_LIBS += -lportaudio
endif

ifdef EXT_BUILD_GENERIC
PLATFORM_LIBS += -Wl,-Bstatic -lpcre -Wl,-Bdynamic -lGL -lX11
else
PLATFORM_LIBS += -lpcre -lGL -lX11
endif

PLATFORM_CXXFLAGS := -g -fPIC -O3
PLATFORM_LDFLAGS := -Wl,--export-dynamic

PLATFORM_DEFINES := -DTARGET_OS_LINUX
PLATFORM_CXX := g++
PLATFORM_LD := g++

ifdef EXT_BUILD_GENERIC
PLATFORM_CXXFLAGS += -mtune=generic
endif
