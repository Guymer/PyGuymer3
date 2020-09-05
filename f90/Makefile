# Find executables ...
CUT   := $(shell which cut      2> /dev/null || echo "ERROR")
EGREP := $(shell which egrep    2> /dev/null || echo "ERROR")
FC1   := $(shell which f2py-3.7 2> /dev/null || echo "ERROR")
FC2   := $(shell which mpif90   2> /dev/null || echo "ERROR")
LN    := $(shell which ln       2> /dev/null || echo "ERROR")
RM    := $(shell which rm       2> /dev/null || echo "ERROR")

# Set defaults ...
DEBUG  ?= false
FTNLIB ?= fortranlib

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

# ******************************************************************************

# "gmake -r [all]"   = "make compile" (default)
all:		compile

# "gmake -r clean"   = removes the compiled code
clean:		$(RM)
	$(RM) -f *.mod *.o *.so
	$(MAKE) -r -C $(FTNLIB) clean

# "gmake -r compile" = compiles the code
compile:	f90.so

# "gmake -r help"    = print this help
help:		$(EGREP)															\
			$(CUT)
	echo "These are the available options:"
	$(EGREP) "^# \"gmake -r " Makefile | $(CUT) -c 2-

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

f90.cpython-37m-darwin.so:	$(FC1)												\
							$(FC2)												\
							$(FTNLIB)/mod_safe.o								\
							f90.F90												\
							src/*.f90
	$(FC1) -c --f77exec=$(FC2) --f90exec=$(FC2) --opt="$(LANG_OPTS) $(WARN_OPTS) $(OPTM_OPTS)" --arch="$(MACH_OPTS)" -m f90 -I$(FTNLIB) f90.F90 $(FTNLIB)/mod_safe.o -L/usr/lib

f90.so:						$(LN)												\
							f90.cpython-37m-darwin.so
	$(LN) -sf f90.cpython-37m-darwin.so f90.so

$(FTNLIB)/%.mod																	\
$(FTNLIB)/%.o:				$(FTNLIB)/Makefile									\
							$(FTNLIB)/%.F90
	$(MAKE) -r -C $(FTNLIB) DEBUG=$(DEBUG) $*.o
