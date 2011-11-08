PLATFORM_LIBS := -pthread -lboost_thread -lboost_system -lboost_filesystem -lm -lpcre -lportaudio /usr/lib/libGL.so

PLATFORM_CXXFLAGS :=
PLATFORM_LDFLAGS :=

PLATFORM_DEFINES := -DTARGET_OS_LINUX
PLATFORM_CXX := g++
PLATFORM_LD := g++
