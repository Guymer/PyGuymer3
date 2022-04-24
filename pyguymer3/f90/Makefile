# Find executables ...
CUT  := $(shell which cut            2> /dev/null || echo "ERROR")
FC1  := $(shell which f2py-3.10      2> /dev/null || echo "ERROR")
FC2  := $(shell which gfortran-mp-11 2> /dev/null || echo "ERROR")
GREP := $(shell which grep           2> /dev/null || echo "ERROR")
LN   := $(shell which ln             2> /dev/null || echo "ERROR")
RM   := $(shell which rm             2> /dev/null || echo "ERROR")

# Set defaults ...
DEBUG  ?= false
FTNLIB ?= fortranlib
LIBDIR ?= /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib

# ******************************************************************************

# Set compiler flags ...
DEBG_OPTS := -g -fcheck=all
LANG_OPTS := -ffree-form -ffree-line-length-none -frecursive -fno-unsafe-math-optimizations -frounding-math -fsignaling-nans -fPIC
WARN_OPTS := -Wall -Wextra -Waliasing -Wcharacter-truncation -Wconversion-extra -Wimplicit-interface -Wimplicit-procedure -Wunderflow -Wtabs
OPTM_OPTS := -O2
MACH_OPTS := -march=native -m64

# If the user wants debugging then append the debugging flags to the language
# flags ...
ifeq ($(DEBUG), true)
	LANG_OPTS += $(DEBG_OPTS)
endif

# Check binaries ...
ifeq ($(CUT),ERROR)
    $(error The binary "cut" is not installed)
endif
ifeq ($(FC1),ERROR)
    $(error The binary "fc1" is not installed)
endif
ifeq ($(FC2),ERROR)
    $(error The binary "fc2" is not installed)
endif
ifeq ($(GREP),ERROR)
    $(error The binary "grep" is not installed)
endif
ifeq ($(LN),ERROR)
    $(error The binary "ln" is not installed)
endif
ifeq ($(RM),ERROR)
    $(error The binary "rm" is not installed)
endif

# Check Python modules ...
# ifneq ($(shell $(PYTHON3) -c "import numpy; print(0)" 2> /dev/null),0)
    # $(error The Python module "numpy" is not installed)
# endif

# Define source files ...
MOD_F90_SRC := $(sort f90.F90 $(wildcard src/*.f90))

# ******************************************************************************

# "gmake -r [all]"   = "make compile" (default)
all:		compile

# "gmake -r clean"   = removes the compiled code
clean:		$(RM)																\
			$(FC2)
	$(RM) -f *.mod *.o *.so
	$(MAKE) -r -C $(FTNLIB) FC=$(FC2) clean

# "gmake -r compile" = compiles the code
compile:	f90.so

# "gmake -r help"    = print this help
help:		$(GREP)																\
			$(CUT)
	echo "These are the available options:"
	$(GREP) -E "^# \"gmake -r " Makefile | $(CUT) -c 2-

# ******************************************************************************

.SILENT: help

# ******************************************************************************

# NOTE: As of 01/Nov/2019 there is still a bug in "gcc9" from MacPorts which
#       results in it being unable to find some system libraries. Below are
#       links to the MacPorts ticket and the GCC ticket as well as the reference
#       for my chosen (hopefully temporary) workaround.
#         * https://trac.macports.org/ticket/59113
#         * https://gcc.gnu.org/bugzilla/show_bug.cgi?id=90835
#         * https://stackoverflow.com/a/58081934

$(FTNLIB)/%.mod																	\
$(FTNLIB)/%.o:				$(FC2)												\
							Makefile											\
							$(FTNLIB)/Makefile									\
							$(FTNLIB)/%.F90
	$(MAKE) -r -C $(FTNLIB) DEBUG=$(DEBUG) FC=$(FC2) $*.o

f90.cpython-39m-darwin.so:	$(FC1)												\
							$(FC2)												\
							Makefile											\
							$(FTNLIB)/mod_safe.o								\
							$(MOD_F90_SRC)
	$(FC1) -c --f77exec=$(FC2) --f90exec=$(FC2) --opt="-fopenmp $(LANG_OPTS) $(WARN_OPTS) $(OPTM_OPTS)" --arch="$(MACH_OPTS)" -lgomp -m f90 -I$(FTNLIB) f90.F90 $(FTNLIB)/mod_safe.o -L$(LIBDIR)

f90.so:						$(LN)												\
							Makefile											\
							f90.cpython-39m-darwin.so
	$(LN) -sf f90.cpython-39m-darwin.so f90.so
