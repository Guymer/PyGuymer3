def getmtime(fname):
    """
    This function aims to mimic os.path.getmtime() but instead of returning a
    int or float it returns a timezone-aware datetime object.
    """

    # Import standard modules ...
    import datetime
    import os

    # Import special modules ...
    try:
        import pytz
    except:
        raise Exception("\"pytz\" is not installed; run \"pip install --user pytz\"")

    # Return answer ...
    return datetime.datetime.fromtimestamp(
        os.path.getmtime(fname),
        tz = pytz.timezone("UTC"),
    )
