# ******************************************************************************
# *                                  BINARIES                                  *
# ******************************************************************************

CUT     := $(shell which cut        2> /dev/null || echo "ERROR")
GREP    := $(shell which grep       2> /dev/null || echo "ERROR")
PYTHON3 := $(shell which python3.12 2> /dev/null || echo "ERROR")

# ******************************************************************************
# *                               CHECK BINARIES                               *
# ******************************************************************************

ifeq ($(CUT),ERROR)
    $(error The binary "cut" is not installed)
endif
ifeq ($(GREP),ERROR)
    $(error The binary "grep" is not installed)
endif
ifeq ($(PYTHON3),ERROR)
    $(error The binary "python3" is not installed)
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

# "gmake -r [all]"       = "gmake -r doc" (default)
all:			doc

# "gmake -r clean"       = removes the compiled Sphinx documentation
clean:
	$(MAKE) -C docs clean

# "gmake -r doc"         = compiles the Sphinx documentation
docs/_build/html/objects.inv													\
doc &:
	$(MAKE) -C docs html

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
