#!/usr/bin/env python3

# Define function ...
def paethFilter(
    a,
    b,
    c,
    /,
):
    """The Path filter (as defined in the PNG specification [2]_).

    Parameters
    ----------
    a : numpy.int16
        See the PNG specification [2]_.
    b : numpy.int16
        See the PNG specification [2]_.
    c : numpy.int16
        See the PNG specification [2]_.

    Returns
    -------
    pr : numpy.int16
        See the PNG specification [2]_.

    Notes
    -----

    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    .. [2] PNG Specification (Third Edition), https://www.w3.org/TR/png-3/
    """

    # **************************************************************************

    # Find differences ...
    pi = a + b - c
    pa = abs(pi - a)
    pb = abs(pi - b)
    pc = abs(pi - c)

    # Return best point ...
    if pa <= pb and pa <= pc:
        return a
    if pb <= pc:
        return b
    return c
