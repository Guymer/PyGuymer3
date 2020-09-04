# Find executables ...
CUT   := $(shell which cut      2> /dev/null || echo "ERROR")
EGREP := $(shell which egrep    2> /dev/null || echo "ERROR")
FC1   := $(shell which f2py-3.7 2> /dev/null || echo "ERROR")
FC2   := $(shell which mpif90   2> /dev/null || echo "ERROR")
RM    := $(shell which rm       2> /dev/null || echo "ERROR")

# Set defaults ...
DEBUG  ?= true
FTNLIB ?= fortranlib

# ******************************************************************************

# Set compiler flags ...
DEBG_OPTS := -g -fcheck=all
LANG_OPTS := -ffree-form -ffree-line-length-none -frecursive -fno-unsafe-math-optimizations -frounding-math -fsignaling-nans
WARN_OPTS := -Wall -Wextra -Waliasing -Wcharacter-truncation -Wconversion-extra -Wimplicit-interface -Wimplicit-procedure -Wunderflow -Wtabs
OPTM_OPTS := -O2
MACH_OPTS := -march=native -m64

# If the user wants debugging then append the debugging flags to the language
# flags ...
ifeq ($(DEBUG), true)
	LANG_OPTS += $(DEBG_OPTS)
endif

# ******************************************************************************

# "gmake [all]"   = "make compile" (default)
all:		compile

# "gmake clean"   = removes the compiled code
clean:		$(RM)
	$(RM) -f *.mod *.o *.so
	$(MAKE) -r -C $(FTNLIB) clean

# "gmake compile" = compiles the code
compile:	f90.so

# "gmake help"    = print this help
help:		$(EGREP)															\
			$(CUT)
	echo "These are the available options:"
	$(EGREP) "^# \"gmake " Makefile | $(CUT) -c 2-

# ******************************************************************************

.SILENT: help

# ******************************************************************************

f90.cpython-37m-darwin.so:	$(FC1)												\
							$(FC2)												\
							$(FTNLIB)/mod_safe.o								\
							f90.F90												\
							src/*.f90
	$(FC1) -c --f77exec=$(FC2) --f90exec=$(FC2) --opt="$(LANG_OPTS) $(WARN_OPTS) $(OPTM_OPTS)" --arch="$(MACH_OPTS)" -m f90 -I$(FTNLIB) f90.F90

f90.so:						f90.cpython-37m-darwin.so
	ln -s f90.cpython-37m-darwin.so f90.so

$(FTNLIB)/mod_safe.o:
	$(MAKE) -r -C $(FTNLIB) mod_safe.o
