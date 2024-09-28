#!/usr/bin/env python3

# Define function ...
def now():
    """Get the current date and time as a timezone-aware ``datetime`` object.

    This function aims to mimic ``datetime.datetime.now()`` but instead it
    returns a timezone-aware ``datetime`` object.

    Returns
    -------
    ans : datetime.datetime
        the timezone-aware ``datetime`` object

    Notes
    -----
    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import standard modules ...
    import datetime

    # Return answer ...
    return datetime.datetime.now(
        tz = datetime.UTC,
    )
