def convert_spreadsheet_to_datetime(val):
    """
    This function converts a date from an Excel spreadsheet (which is an
    integer) into a timezone-aware datetime object.
    """

    # Import standard modules ...
    import datetime

    # Import special modules ...
    try:
        import pytz
    except:
        raise Exception("\"pytz\" is not installed; run \"pip install --user pytz\"")

    # Return answer ..
    return datetime.datetime.fromtimestamp(
        86400 * (val - 25569),
        tz = pytz.timezone("UTC"),
    )
