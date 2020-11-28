def convert_spreadsheet_to_unix(val):
    """
    This function converts a date from an Excel spreadsheet (which is an
    integer) into the number of seconds since the Unix epoch (which is also an
    integer).
    """

    # Return answer ..
    return 86400 * (val - 25569)