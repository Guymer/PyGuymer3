#!/usr/bin/env python3

# Define function ...
def convert_bytes_to_pretty_bytes(
    uglySize : int,
    /,
) -> tuple[float, str]:
    """Convert a value of bytes to a pretty value of bytes with some units

    This function reads in a value of bytes and returns the same value expressed
    prettily for a human to read with some units.

    Parameters
    ----------
    uglySize : int
        the value of bytes

    Returns
    -------
    prettySize : float
        the value of bytes in *unit*
    unit : str
        the units of *prettySize*

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
    prettySize : float = float(uglySize)                                        # [B]
    unit : str = "B"

    # Convert to useful units ...
    if prettySize >= 1024.0:
        prettySize /= 1024.0                                                    # [KiB]
        unit = "KiB"
    if prettySize >= 1024.0:
        prettySize /= 1024.0                                                    # [MiB]
        unit = "MiB"
    if prettySize >= 1024.0:
        prettySize /= 1024.0                                                    # [GiB]
        unit = "GiB"
    if prettySize >= 1024.0:
        prettySize /= 1024.0                                                    # [TiB]
        unit = "TiB"
    if prettySize >= 1024.0:
        prettySize /= 1024.0                                                    # [PiB]
        unit = "PiB"

    # Return answer ...
    return prettySize, unit
