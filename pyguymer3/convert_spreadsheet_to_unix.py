def convert_spreadsheet_to_unix(val):
    """Convert an Excel spreadsheet date to an UNIX timestamp.

    This function converts an Excel spreadsheet date (which is an integer) to an
    UNIX timestamp (which is also an integer).

    Parameters
    ----------
    val : int
            the Excel spreadsheet date

    Returns
    -------
    ans : int
            the UNIX timestamp
    """

    # Return answer ..
    return 86400 * (val - 25569)
