PLATFORM_LIBS = -ldl -lm

ifdef EXT_BOOST
PLATFORM_LIBS += -lboost_thread -lboost_system -lboost_filesystem -pthread
else
PLATFORM_LIBS += -pthread
endif

PLATFORM_LIBS += -lportaudio -lpcre

PLATFORM_CXXFLAGS := -std=c++11 -g -fPIC -O3
PLATFORM_LDFLAGS := -Wl,--export-dynamic

PLATFORM_DEFINES := -DTARGET_OS_LINUX
PLATFORM_CXX := g++
PLATFORM_LD := g++
