def getctime(fname):
    """
    This function aims to mimic os.path.getctime() but instead of returning a
    int or float it returns a timezone-aware datetime object.
    """

    # Import standard modules ...
    import datetime
    import os

    # Return answer ...
    return datetime.datetime.fromtimestamp(
        os.path.getctime(fname),
        tz = datetime.timezone.utc,
    )
