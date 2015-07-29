PLATFORM_LIBS := -lm -lpcre -lglfw -lportaudio

PLATFORM_CXXFLAGS :=
PLATFORM_LDFLAGS := --verbose -L/usr/local/lib

PLATFORM_DEFINES := -DEXT_MCJIT -DEXT_BOOST
PLATFORM_CXX := g++
PLATFORM_LD := g++
