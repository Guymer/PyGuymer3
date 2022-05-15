def getatime(fname):
    """Get the access time of a file as a timezone-aware ``datetime`` object.

    This function aims to mimic ``os.path.getatime()`` but instead of returning
    a integer or float it returns a timezone-aware ``datetime`` object.

    Parameters
    ----------
    fname : str
            the file name

    Returns
    -------
    ans : datetime.datetime
            the timezone-aware ``datetime`` object
    """

    # Import standard modules ...
    import datetime
    import os

    # Return answer ...
    return datetime.datetime.fromtimestamp(
        os.path.getatime(fname),
        tz = datetime.timezone.utc,
    )
