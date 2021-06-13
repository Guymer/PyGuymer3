def now():
    """
    This function aims to mimic datetime.datetime.now() but instead it returns
    a timezone-aware datetime object.
    """

    # Import standard modules ...
    import datetime

    # Return answer ...
    return datetime.datetime.now(
        tz = datetime.timezone.utc,
    )
