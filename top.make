OBJDIR := build/obj

PERL := perl

include config/$(EXTEMPORE_OS).make
include src/component.make

DEFINES := $(PLATFORM_DEFINES) \
	-D_GNU_SOURCE \
	-D__STDC_CONSTANT_MACROS \
	-D__STDC_LIMIT_MACROS \

LLVM_CXXFLAGS := $(EXT_LLVM_CXXFLAGS)
LLVM_LDFLAGS := $(EXT_LLVM_LDFLAGS)
LLVM_LIBS := $(EXT_LLVM_LIBS)

LIBS := \
	$(PLATFORM_LIBS) \
	$(LLVM_LIBS) \

CXX := $(PLATFORM_CXX)
LD := $(PLATFORM_LD)

CXXFLAGS := \
	-w -O3 -MMD \
	$(LLVM_CXXFLAGS) \
	$(PLATFORM_CXXFLAGS) \
	$(DEFINES) \
	$(EXT_USER_ARGS) \
	-Iinclude \
        -fexceptions \
        -frtti \

LDFLAGS := \
	$(EXT_LLVM_LDFLAGS) \
	$(PLATFORM_LDFLAGS) \

extempore: $(OBJFILES)
	@echo + ld $(LDFLAGS) -o $@ $(OBJFILES) $(LIBS)
	@$(LD) $(LDFLAGS) -o $@ $(OBJFILES) $(LIBS)

# C++ include-dependencies are tracked for us by the compiler.  In the
# following, we gather all the dependency information into one file and
# include it.

$(OBJDIR)/.deps: $(wildcard $(OBJDIR)/*.d)
	@mkdir -p $(@D)
	@$(PERL) build/mergedep.pl $@ $^


-include $(OBJDIR)/.deps
