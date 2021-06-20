def convert_spreadsheet_to_datetime(val):
    """
    This function converts a date from an Excel spreadsheet (which is an
    integer) into a timezone-aware datetime object.
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
