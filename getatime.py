def getatime(fname):
    """
    This function aims to mimic os.path.getatime() but instead of returning a
    int or float it returns a timezone-aware datetime object.
    """

    # Import standard modules ...
    import datetime
    import os

    # Return answer ...
    return datetime.datetime.fromtimestamp(
        os.path.getatime(fname),
        tz = datetime.timezone.utc,
    )
