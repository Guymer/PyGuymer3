#!/usr/bin/env python3

"""
A Python sub-module containing a bunch of random functions that I have written
over the years in FORTRAN to be called from Python using
`f2py <https://numpy.org/doc/stable/f2py/>`_.

Notes
-----
* To compile the FORTRAN source code and create the sub-module just run the
  Makefile.
* `f2py <https://numpy.org/doc/stable/f2py/>`_ isn't good at creating interfaces
  to FORTRAN functions, so all the FORTRAN here will be subroutines (see these
  Stack Overflow questions [1]_ and [2]_).
* `f2py <https://numpy.org/doc/stable/f2py/>`_ doesn't release the GIL, so I
  have to do that with a macro (see this StackOverflow question [3]_).

Documentation of the FORTRAN subroutine can be obtained by running:

>>> import pyguymer3
>>> import pyguymer3.f90
>>> print(pyguymer3.f90.funcs.${FUNC}.__doc__)

According to the definition of ``c2py_map.keys()`` in the
`source of "capi_maps.py" <https://github.com/numpy/numpy/blob/master/numpy/f2py/capi_maps.py>`_
(in the `source of "f2py" <https://github.com/numpy/numpy/tree/master/numpy/f2py>`_)
only the following short-hands for C data types are recognised by
`f2py <https://numpy.org/doc/stable/f2py/>`_:

* ``double``
* ``float``
* ``long_double``
* ``char``
* ``signed_char``
* ``unsigned_char``
* ``short``
* ``unsigned_short``
* ``int``
* ``long``
* ``long_long``
* ``unsigned``
* ``complex_float``
* ``complex_double``
* ``complex_long_double``
* ``string``

According to the
`gfortran "ISO_C_BINDING" documentation <https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fC_005fBINDING.html>`_
only the following FORTRAN-C mappings are possible (where mappings have been
removed if the corresponding C data type does not have a corresponding
short-hand in `f2py <https://numpy.org/doc/stable/f2py/>`_ above):

* integer

    * FORTRAN ``C_INT`` goes to C ``int`` (which is ``int`` in
      `f2py <https://numpy.org/doc/stable/f2py/>`_)
    * FORTRAN ``C_SHORT`` goes to C ``short int`` (which is ``short`` in
      `f2py <https://numpy.org/doc/stable/f2py/>`_)
    * FORTRAN ``C_LONG`` goes to C ``long int`` (which is ``long`` in
      `f2py <https://numpy.org/doc/stable/f2py/>`_)
    * FORTRAN ``C_LONG_LONG`` goes to C ``long long int`` (which is
      ``long_long`` in `f2py <https://numpy.org/doc/stable/f2py/>`_)
    * FORTRAN ``C_SIGNED_CHAR`` goes to C ``signed char`` (which is
      ``signed_char`` in `f2py <https://numpy.org/doc/stable/f2py/>`_)

* real

    * FORTRAN ``C_FLOAT`` goes to C ``float`` (which is ``float`` in
      `f2py <https://numpy.org/doc/stable/f2py/>`_)
    * FORTRAN ``C_DOUBLE`` goes to C ``double`` (which is ``double`` in
      `f2py <https://numpy.org/doc/stable/f2py/>`_)
    * FORTRAN ``C_LONG_DOUBLE`` goes to C ``long double`` (which is
      ``long_double`` in `f2py <https://numpy.org/doc/stable/f2py/>`_)

* complex

    * FORTRAN ``C_FLOAT_COMPLEX`` goes to C ``float _Complex`` (which is
      ``complex_float`` in `f2py <https://numpy.org/doc/stable/f2py/>`_)
    * FORTRAN ``C_DOUBLE_COMPLEX`` goes to C ``double _Complex`` (which is
      ``complex_double`` in `f2py <https://numpy.org/doc/stable/f2py/>`_)
    * FORTRAN ``C_LONG_DOUBLE_COMPLEX`` goes to C ``long double _Complex`` (
      which is ``complex_long_double`` in
      `f2py <https://numpy.org/doc/stable/f2py/>`_)

* character

    * FORTRAN ``C_CHAR`` goes to C ``char`` (which is ``char`` in
      `f2py <https://numpy.org/doc/stable/f2py/>`_)

The above study has led me to create
:download:`.f2py_f2cmap <../../pyguymer3/f90/.f2py_f2cmap>` to allow me to use
FORTRAN data types from "ISO_C_BINDING" and have them magically mapped over to
the *correct* corresponding C data type in
`f2py <https://numpy.org/doc/stable/f2py/>`_.

Copyright 2017 Thomas Guymer [4]_

References
----------
.. [1] "f2py array valued functions" on StackOverflow, https://stackoverflow.com/q/10913003
.. [2] "when using f2py, function scope within FORTRAN module different than when compiled for FORTRAN program" on StackOverflow, https://stackoverflow.com/q/18669814
.. [3] "f2py function release GIL" on StackOverflow, https://stackoverflow.com/q/15976369
.. [4] PyGuymer3, https://github.com/Guymer/PyGuymer3
"""

# Import sub-functions ...
from .funcs import *
