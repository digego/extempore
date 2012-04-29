SRCFILES := \
	src/AudioDevice.cpp \
	src/EXTCondition.cpp \
	src/Extempore.cpp \
	src/EXTLLVM.cpp \
	src/EXTMonitor.cpp \
	src/EXTMutex.cpp \
	src/EXTThread.cpp \
	src/OSC.cpp \
	src/Scheme.cpp \
	src/SchemeFFI.cpp \
	src/SchemeProcess.cpp \
	src/SchemeREPL.cpp \
	src/TaskScheduler.cpp \
	src/UNIV.cpp \

OBJFILES := $(patsubst src/%.cpp, $(OBJDIR)/%.o, $(SRCFILES))

ifeq ($(EXTEMPORE_OS),darwin)
$(OBJDIR)/%.o: src/%.cpp
#	@echo + cxx $(CXXFLAGS) -c -x objective-c++ -o $@ $<
	@echo + $@ $<
	@mkdir -p $(@D)
	@$(CXX) $(CXXFLAGS) -c -x objective-c++ -o $@ $<
else
$(OBJDIR)/%.o: src/%.cpp
#	@echo + cxx $(CXXFLAGS) -c -x c++ -o $@ $<
	@echo + $@ $< 
	@mkdir -p $(@D)
	@$(CXX) $(CXXFLAGS) -c -x c++ -o $@ $<
endif