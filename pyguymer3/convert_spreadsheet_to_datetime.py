#!/usr/bin/env python3

# Define function ...
def convert_spreadsheet_to_datetime(val, /):
    """Convert an Excel spreadsheet date to a timezone-aware :class:`.datetime`
    object

    This function converts an Excel spreadsheet date (which is an integer) to a
    timezone-aware :class:`.datetime` object.

    Parameters
    ----------
    val : int
        the Excel spreadsheet date

    Returns
    -------
    ans : datetime.datetime
        the timezone-aware :class:`.datetime` object

    Notes
    -----
    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import standard modules ...
    import datetime

    # Import sub-functions ...
    from .convert_spreadsheet_to_unix import convert_spreadsheet_to_unix

    # Return answer ..
    return datetime.datetime.fromtimestamp(
        convert_spreadsheet_to_unix(val),
        tz = datetime.timezone.utc,
    )
