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

#PLATFORM_LIBS := -ldl -lm -ljack -lboost_thread -lboost_system -lboost_filesystem  -lm -lpcre /usr/lib/nvidia-current/libGL.so
PLATFORM_LIBS += -lpcre -lGL #/usr/lib/nvidia-current/libGL.so

PLATFORM_CXXFLAGS :=
PLATFORM_LDFLAGS :=

PLATFORM_DEFINES := -DTARGET_OS_LINUX
PLATFORM_CXX := g++
PLATFORM_LD := g++
