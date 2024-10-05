#!/usr/bin/env python3

# Define function ...
def make_path_safe(
    path,
    /,
    *,
    allowHidden = False,
):
    """Make a path safe for using on a filesystem.

    Parameters
    ----------
    path : str
        the path to make safe
    allowHidden : bool, optional
        allow the path to be hidden if it starts with a period (default False)

    Notes
    -----
    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import standard modules ...
    import os

    # Remove "bad" characters according to me ...
    # NOTE: This just avoids accidents with comments and variables in a shell.
    for badChar in ["#", "$"]:
        path = path.replace(badChar, "")

    # Remove more bad characters according to Wikipedia (not covered by either
    # myself earlier or Microsoft later) ...
    # NOTE: See https://en.wikipedia.org/wiki/Filename#Reserved_characters_and_words
    # NOTE: See https://en.wikipedia.org/wiki/Filename#Comparison_of_filename_limitations
    for badChar in ["%"]:
        path = path.replace(badChar, "")

    # Remove more bad characters according to Microsoft (not covered by either
    # myself earlier or Wikipedia earlier) ...
    # NOTE: See https://learn.microsoft.com/en-gb/windows/win32/fileio/naming-a-file#naming-conventions
    for badChar in ["\\", "/", ":", "*", "?", "\"", "<", ">", "|"]:
        path = path.replace(badChar, "")
    for badChar in [chr(i) for i in range(32)]:
        path = path.replace(badChar, "")
    path = path.rstrip(" .")
    if allowHidden:
        path = path.lstrip(" ")
    else:
        path = path.lstrip(" .")
    root, ext = os.path.splitext(path)
    if root in ["CON", "PRN", "AUX", "NUL", "COM0", "COM1", "COM2", "COM3", "COM4", "COM5", "COM6", "COM7", "COM8", "COM9", "LPT0", "LPT1", "LPT2", "LPT3", "LPT4", "LPT5", "LPT6", "LPT7", "LPT8", "LPT9"]:
        path = f"reservedName{ext}"

    # Check if the path is now empty ...
    if len(path) == 0:
        raise Exception("\"path\" did not contain any good characters") from None

    # Return answer ...
    return path
