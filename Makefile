TARGET = extempore
PREFIX = /usr/local
UNAME = $(shell uname -a)

LLVM_CFLAGS = $(shell llvm-config --cxxflags)
LLVM_LIBS = $(shell llvm-config --libs)
LLVM_LDFLAGS = $(shell llvm-config --ldflags)
PCRE_CFLAGS = $(shell pkg-config --cflags libpcre)
PCRE_LIBS = $(shell pkg-config --libs libpcre)
PORTAUDIO_CFLAGS = $(shell pkg-config --cflags portaudio-2.0)
PORTAUDIO_LIBS = $(shell pkg-config --libs portaudio-2.0)

INCLUDES = -I./include $(LLVM_CFLAGS) $(PCRE_CFLAGS) $(PORTAUDIO_CFLAGS)
CXXFLAGS = $(INCLUDES) -fexceptions
LDFLAGS = $(LLVM_LIBS) $(LLVM_LDFLAGS) $(PCRE_LIBS) $(PORTAUDIO_LIBS)

SRC = AudioDevice.cpp \
	EXTCondition.cpp \
	EXTLLVM.cpp \
	EXTMonitor.cpp \
	EXTMutex.cpp \
	EXTThread.cpp \
	Extempore.cpp \
	OSC.cpp \
	Scheme.cpp \
	SchemeFFI.cpp \
	SchemeProcess.cpp \
	SchemeREPL.cpp \
	TaskScheduler.cpp \
	UNIV.cpp

VPATH = src
OBJ = $(patsubst %.cpp, %.o, $(filter %.cpp, $(SRC)))

.PHONY: all clean

all: $(TARGET)

$(TARGET): $(OBJ)
	$(CXX) $(LDFLAGS) $(LDLIBS) -o $@ $^

clean:
	@rm -f $(OBJ)
	@rm -f $(TARGET)
