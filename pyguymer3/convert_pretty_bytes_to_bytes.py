#!/usr/bin/env python3

# Define function ...
def convert_pretty_bytes_to_bytes(string, /):
    """Convert a pretty string of a bytes with some units to a value of bytes

    This function reads in a string of a human-readable value of bytes with some
    units and returns the same value as a float.

    Parameters
    ----------
    string : str
        the human-readable value of bytes with some units

    Returns
    -------
    size : float
        the value of bytes

    Notes
    -----
    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3

    Examples
    --------
    >>> pyguymer3.convert_pretty_bytes_to_bytes("1.5 KiB")
    1536.0

    """

    # Import standard modules ...
    import re

    # Extract digits (with decimal point) and letters separately ...
    size = float(re.sub(r"[a-zA-Z]", "", string).strip())                       # [?]
    units = re.sub(r"[0-9\.]", "", string).strip()

    # Scale value ...
    if units in ["KB", "KiB"]:
        size *= 1024.0                                                          # [B]
    elif units in ["MB", "MiB"]:
        size *= 1024.0 * 1024.0                                                 # [B]
    elif units in ["GB", "GiB"]:
        size *= 1024.0 * 1024.0 * 1024.0                                        # [B]
    elif units in ["TB", "TiB"]:
        size *= 1024.0 * 1024.0 * 1024.0 * 1024.0                               # [B]
    elif units in ["PB", "PiB"]:
        size *= 1024.0 * 1024.0 * 1024.0 * 1024.0 * 1024.0                      # [B]

    # Return answer ...
    return size
