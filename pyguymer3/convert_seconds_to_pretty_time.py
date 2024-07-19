#!/usr/bin/env python3

# Define function ...
def convert_seconds_to_pretty_time(secFlt, /):
    """Convert a value of seconds to a pretty value of time

    This function reads in a value of seconds and returns the same value
    expressed prettily for a human to read as hours, minutes and seconds.

    Parameters
    ----------
    secFlt : float
        the value of seconds

    Returns
    -------
    secStr : float
        the value of seconds as a string

    Notes
    -----
    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3

    Examples
    --------
    >>> pyguymer3.convert_seconds_to_pretty_time(7326.311)
    '2h 2m 6.3s'
    """

    # Calculate how many hours, minutes and seconds there are ...
    h, rem = divmod(secFlt, 3600.0)
    m, s = divmod(rem, 60.0)

    # Check how big the number is ...
    if h > 0.0:
        # Return answer ...
        return f"{h:,.0f}h {m:.0f}m {s:.1f}s"

    # Check how big the number is ...
    if m > 0.0:
        # Return answer ...
        return f"{m:.0f}m {s:.1f}s"

    # Return answer ...
    return f"{s:.1f}s"
