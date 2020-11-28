def getmtime(fname):
    """
    This function aims to mimic os.path.getmtime() but instead of returning a
    int or float it returns a timezone-aware datetime object.
    """

    # Import standard modules ...
    import datetime
    import os

    # Return answer ...
    return datetime.datetime.fromtimestamp(
        os.path.getmtime(fname),
        tz = datetime.timezone.utc,
    )