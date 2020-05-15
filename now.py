def now():
    """
    This function aims to mimic datetime.datetime.now() but instead it returns
    a timezone-aware datetime object.
    """

    # Import standard modules ...
    import datetime

    # Import special modules ...
    try:
        import pytz
    except:
        raise Exception("\"pytz\" is not installed; run \"pip install --user pytz\"")

    # Return answer ...
    return datetime.datetime.now(
        tz = pytz.timezone("UTC"),
    )
