#!/usr/bin/env bash

# Clean up ...
rm -rf f90.so f90.cpython-37m-darwin.so f90.cpython-37m-darwin.so.dSYM

# Define options ...
LANG_OPTS="-ffree-form -ffree-line-length-none -frecursive"
WARN_OPTS="-Wall -Wextra -Waliasing -Warray-temporaries -Wconversion-extra -Wimplicit-interface -Wimplicit-procedure"
MACH_OPTS="-m64"

# Compile ...
f2py-3.7 -c --f77exec="gfortran-mp-8" --f90exec="gfortran-mp-8" --opt="${LANG_OPTS} ${WARN_OPTS}" --arch="${MACH_OPTS}" -m f90 f90.F90

# Create link so that it can be imported easily ...
ln -s *.so f90.so
