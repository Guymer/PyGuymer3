#!/usr/bin/env python3

# Define function ...
def getmtime(
    fname,
    /,
):
    """Get the modification time of a file as a timezone-aware datetime object.

    This function aims to mimic ``os.path.getmtime()`` but instead of returning
    a integer or float it returns a timezone-aware ``datetime`` object.

    Parameters
    ----------
    fname : str
        the file name

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
    import os

    # Return answer ...
    return datetime.datetime.fromtimestamp(
        os.path.getmtime(fname),
        tz = datetime.UTC,
    )
