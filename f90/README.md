# PyGuymer3.F90

This is a Python 3.X sub-module containing a bunch of random functions that I have written over the years in FORTRAN to be called from Python 3.X using f2py.

## Notes

* To compile the FORTRAN source code and create the sub-module just run [Makefile](Makefile).
* f2py isn't good at creating interfaces to FORTRAN functions, see [this Stack Overflow question](https://stackoverflow.com/questions/10913003/f2py-array-valued-functions) and [this Stack Overflow question](https://stackoverflow.com/questions/18669814/when-using-f2py-function-scope-within-fortran-module-different-than-when-compil), so all the FORTRAN here will be subroutines.

## Data Types

According to the definition of `c2py_map.keys()` in the [source of capi_maps.py](https://github.com/numpy/numpy/blob/master/numpy/f2py/capi_maps.py) in the [source of f2py](https://github.com/numpy/numpy/tree/master/numpy/f2py) only the following short-hands for C data types are recognised by f2py:

* `double`
* `float`
* `long_double`
* `char`
* `signed_char`
* `unsigned_char`
* `short`
* `unsigned_short`
* `int`
* `long`
* `long_long`
* `unsigned`
* `complex_float`
* `complex_double`
* `complex_long_double`
* `string`

According to the [gfortran `ISO_C_BINDING` documentation](https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fC_005fBINDING.html) only the following FORTRAN-C mappings are possible (where mappings have been removed if the corresponding C data type does not have a corresponding short-hand in f2py above):

* integer
    * FORTRAN `C_INT` goes to C `int` (which is `int` in f2py)
    * FORTRAN `C_SHORT` goes to C `short int` (which is `short` in f2py)
    * FORTRAN `C_LONG` goes to C `long int` (which is `long` in f2py)
    * FORTRAN `C_LONG_LONG` goes to C `long long int` (which is `long_long` in f2py)
    * FORTRAN `C_SIGNED_CHAR` goes to C `signed char` (which is `signed_char` in f2py)
* real
    * FORTRAN `C_FLOAT` goes to C `float` (which is `float` in f2py)
    * FORTRAN `C_DOUBLE` goes to C `double` (which is `double` in f2py)
    * FORTRAN `C_LONG_DOUBLE` goes to C `long double` (which is `long_double` in f2py)
* complex
    * FORTRAN `C_FLOAT_COMPLEX` goes to C `float _Complex` (which is `complex_float` in f2py)
    * FORTRAN `C_DOUBLE_COMPLEX` goes to C `double _Complex` (which is `complex_double` in f2py)
    * FORTRAN `C_LONG_DOUBLE_COMPLEX` goes to C `long double _Complex` (which is `complex_long_double` in f2py)
* character
    * FORTRAN `C_CHAR` goes to C `char` (which is `char` in f2py)

The above study has led me to create [.f2py_f2cmap](.f2py_f2cmap) to allow me to use FORTRAN data types from `ISO_C_BINDING` and have them magically mapped over to the *correct* corresponding C data type in f2py.

## Documentation

```
c = add(a,b)

Wrapper for ``add``.

Parameters
----------
a : input float
b : input float

Returns
-------
c : float
```
