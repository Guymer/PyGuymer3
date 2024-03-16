#!/usr/bin/env python3

# Define function ...
def convert_bytes_to_pretty_bytes(size, /):
    """Convert a value of bytes to a pretty value of bytes with some units

    This function reads in a value of bytes and returns the same value expressed
    prettily for a human to read with some units.

    Parameters
    ----------
    size : int
        the value of bytes

    Returns
    -------
    size : float
        the value of bytes in *unit*
    unit : str
        the units of *size*

    Notes
    -----
    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3

    Examples
    --------
    >>> pyguymer3.convert_bytes_to_pretty_bytes(1536)
    (1.5, 'KiB')
    """

    # Convert input to float and set default units ...
    size = float(size)
    unit = "B"

    # Convert to useful units ...
    if size >= 1024.0:
        size /= 1024.0
        unit = "KiB"
    if size >= 1024.0:
        size /= 1024.0
        unit = "MiB"
    if size >= 1024.0:
        size /= 1024.0
        unit = "GiB"
    if size >= 1024.0:
        size /= 1024.0
        unit = "TiB"
    if size >= 1024.0:
        size /= 1024.0
        unit = "PiB"

    # Return answer ...
    return size, unit
