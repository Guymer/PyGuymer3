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

    # Check what the units are and return it scaled ...
    match units:
        case "B":
            return size
        case "KB" | "KiB":
            return 1024.0 * size
        case "MB" | "MiB":
            return 1024.0 * 1024.0 * size
        case "GB" | "GiB":
            return 1024.0 * 1024.0 * 1024.0 * size
        case "TB" | "TiB":
            return 1024.0 * 1024.0 * 1024.0 * 1024.0 * size
        case "PB" | "PiB":
            return 1024.0 * 1024.0 * 1024.0 * 1024.0 * 1024.0 * size
        case _:
            # Crash ...
            raise ValueError(f"\"units\" is an unexpected value ({repr(units)})") from None
