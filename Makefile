# ******************************************************************************
# *                                  BINARIES                                  *
# ******************************************************************************

CUT     := $(shell which cut            2> /dev/null || echo "ERROR")
FC      := $(shell which gfortran-mp-14 2> /dev/null || echo "ERROR")
GREP    := $(shell which grep           2> /dev/null || echo "ERROR")
LS      := $(shell which ls             2> /dev/null || echo "ERROR")
PYTHON3 := $(shell which python3.13     2> /dev/null || echo "ERROR")
RM      := $(shell which rm             2> /dev/null || echo "ERROR")

# ******************************************************************************
# *                               CHECK BINARIES                               *
# ******************************************************************************

ifeq ($(CUT),ERROR)
    $(error The binary "cut" is not installed)
endif
ifeq ($(FC),ERROR)
    $(error The binary "fc" is not installed)
endif
ifeq ($(GREP),ERROR)
    $(error The binary "grep" is not installed)
endif
ifeq ($(LS),ERROR)
    $(error The binary "ls" is not installed)
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

ifneq ($(shell $(PYTHON3) -c "import sphinx; print(0)" 2> /dev/null),0)
    $(error The Python module "sphinx" is not installed)
endif

# ******************************************************************************
# *                           USER-SPECIFIED TARGETS                           *
# ******************************************************************************

# "gmake -r [all]"       = "gmake -r build doc" (default)
all:			build															\
				compile															\
				doc

# "gmake -r build"       = build a Python wheel
build:
	$(PYTHON3) -m build
	$(LS) -hl dist

# "gmake -r clean"       = removes linting logs, build output, compiled Sphinx documentation and compiled FORTRAN
clean:
	$(RM) -rf dist PyGuymer3.egg-info
	$(RM) -f mypy.log pylint.log shellcheck.log
	$(RM) -f pyguymer3/mypy.log pyguymer3/pylint.log pyguymer3/shellcheck.log
	$(RM) -f pyguymer3/f90/fortranlib/mypy.log pyguymer3/f90/fortranlib/pylint.log pyguymer3/f90/fortranlib/shellcheck.log
	$(MAKE) -r -C docs clean
	$(MAKE) -r -C pyguymer3/f90 FC=$(FC) PYTHON3=$(PYTHON3) clean

compile:
	$(MAKE) -r -C pyguymer3/f90 FC=$(FC) PYTHON3=$(PYTHON3)

# "gmake -r doc"         = compiles the Sphinx documentation
docs/_build/html/objects.inv													\
doc &:
	$(MAKE) -r -C docs html
	$(MAKE) -r -C pyguymer3/f90 FC=$(FC) PYTHON3=$(PYTHON3) doc

# "gmake -r doc-targets" = lists all the available Sphinx documentation targets
doc-targets:	docs/_build/html/objects.inv
	$(PYTHON3) -m sphinx.ext.intersphinx $<

# "gmake -r help"        = print this help
help:
	echo "These are the available options:"
	$(GREP) -E "^# \"gmake -r " Makefile | $(CUT) -c 2-

# ******************************************************************************
# *                            ENVIRONMENT SETTINGS                            *
# ******************************************************************************

.SILENT: help
