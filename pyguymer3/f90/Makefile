# ******************************************************************************
# *                                 VARIABLES                                  *
# ******************************************************************************

# NOTE: There was a bug in NumPy (using "meson" to build) where "f2py" would
#       copy the file to a build folder and, therefore, the relative paths to
#       external libraries would break. To work around this I prepend the
#       current directory to the library path to make it an absolute path. See:
#         * https://github.com/numpy/numpy/issues/25344

DEBUG     ?= false
FTNLIB    ?= $(PWD)/fortranlib
LIBDIR    ?= /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib
DEBG_OPTS := -g -fcheck=all
LANG_OPTS := -ffree-form -ffree-line-length-none -frecursive -fno-unsafe-math-optimizations -frounding-math -fsignaling-nans -fPIC
WARN_OPTS := -Wall -Wextra -Waliasing -Wcharacter-truncation -Wconversion-extra -Wimplicit-interface -Wimplicit-procedure -Wunderflow -Wtabs
OPTM_OPTS := -O2
MACH_OPTS := -march=native -m64

# ******************************************************************************
# *                                  BINARIES                                  *
# ******************************************************************************

CUT     := $(shell which cut            2> /dev/null || echo "ERROR")
FC1     := $(shell which f2py-3.11      2> /dev/null || echo "ERROR")
FC2     := $(shell which gfortran-mp-13 2> /dev/null || echo "ERROR")
GREP    := $(shell which grep           2> /dev/null || echo "ERROR")
LN      := $(shell which ln             2> /dev/null || echo "ERROR")
PYTHON3 := $(shell which python3.11     2> /dev/null || echo "ERROR")
RM      := $(shell which rm             2> /dev/null || echo "ERROR")

# ******************************************************************************
# *                             DYNAMIC VARIABLES                              *
# ******************************************************************************

ifeq ($(DEBUG), true)
	LANG_OPTS += $(DEBG_OPTS)
endif

# ******************************************************************************
# *                               CHECK BINARIES                               *
# ******************************************************************************

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
ifeq ($(PYTHON3),ERROR)
    $(error The binary "python3" is not installed)
endif
ifeq ($(RM),ERROR)
    $(error The binary "rm" is not installed)
endif

# ******************************************************************************
# *                            CHECK PYTHON MODULES                            *
# ******************************************************************************

# ifneq ($(shell $(PYTHON3) -c "import numpy; print(0)" 2> /dev/null),0)
#     $(error The Python module "numpy" is not installed)
# endif

# ******************************************************************************
# *                             DERIVED VARIABLES                              *
# ******************************************************************************

SUFFIX ?= $(shell $(PYTHON3) -c "import sysconfig; print(sysconfig.get_config_var(\"EXT_SUFFIX\"))")

# ******************************************************************************
# *                           USER-SPECIFIED TARGETS                           *
# ******************************************************************************

# "gmake -r [all]"   = "gmake -r compile" (default)
all:		compile

# "gmake -r clean"   = removes the compiled code
clean:
	$(RM) -f *.mod *.o *.so
	$(MAKE) -r -C $(FTNLIB) FC=$(FC2) PYTHON3=$(PYTHON3) clean

# "gmake -r compile" = compiles the code
compile:	funcs.so

# "gmake -r help"    = print this help
help:
	echo "The suffix is \"${SUFFIX}\"."
	echo "These are the available options:"
	$(GREP) -E "^# \"gmake -r " Makefile | $(CUT) -c 2-

# ******************************************************************************
# *                            ENVIRONMENT SETTINGS                            *
# ******************************************************************************

.SILENT: help

# ******************************************************************************
# *                        INTERNALLY-SPECIFIED TARGETS                        *
# ******************************************************************************

# NOTE: As of 01/Nov/2019 there is still a bug in "gcc9" from MacPorts which
#       results in it being unable to find some system libraries. Below are
#       links to the MacPorts ticket and the GCC ticket as well as the reference
#       for my chosen (hopefully temporary) workaround.
#         * https://trac.macports.org/ticket/59113
#         * https://gcc.gnu.org/bugzilla/show_bug.cgi?id=90835
#         * https://stackoverflow.com/a/58081934

# NOTE: There was a bug in NumPy (using "meson" to build) where "f2py" would
#       copy the file to a build folder but not copy the "INCLUDE" files and,
#       therefore, the compilation would fail because it would not find the
#       included file. To work around this I use the "--include-paths" argument
#       as demonstrated in the test code added as part of the Pull Request which
#       closed the following issue:
#         * https://github.com/numpy/numpy/issues/25344

$(FTNLIB)/%.mod																	\
$(FTNLIB)/%.o &:	$(FTNLIB)/%.F90
	$(MAKE) -r -C $(FTNLIB) DEBUG=$(DEBUG) FC=$(FC2) PYTHON3=$(PYTHON3) $*.o

funcs.so:			$(FTNLIB)/mod_safe.o										\
					funcs.F90													\
					src/*.f90
	$(RM) -f funcs.*.so funcs.so
	$(FC1) -c --include-paths $(PWD) --f77exec=$(FC2) --f90exec=$(FC2) --opt="-fopenmp $(LANG_OPTS) $(WARN_OPTS) $(OPTM_OPTS)" --arch="$(MACH_OPTS)" -lgomp -m funcs -I$(FTNLIB) funcs.F90 $(FTNLIB)/mod_safe.o -L$(LIBDIR)
	$(LN) -s funcs$(SUFFIX) funcs.so
