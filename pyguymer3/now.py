def now():
    """Get the current date and time as a timezone-aware ``datetime`` object.

    This function aims to mimic ``datetime.datetime.now()`` but instead it
    returns a timezone-aware ``datetime`` object.

    Returns
    -------
    ans : datetime.datetime
            the timezone-aware ``datetime`` object
    """

    # Import standard modules ...
    import datetime

    # Return answer ...
    return datetime.datetime.now(
        tz = datetime.timezone.utc,
    )
