#!/usr/bin/env python3

# Define function ...
def typeId2typeName(
    typeId,
    /,
    *,
    debug = __debug__,
):
    """Match type ID number to type name.

    Parameters
    ----------
    typeId : int
        The type ID number.
    debug : bool, optional
        Print debug messages.

    Returns
    -------
    typeName : str
        The type name.

    Notes
    -----
    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Match the type ID with its name ...
    # NOTE: See Section 4.1.2 (pages 8-9) of N4378.pdf
    match typeId:
        case 1:
            return "byte"
        case 2:
            return "ascii"
        case 3:
            return "short"
        case 4:
            return "long"
        case 5:
            return "rational"
        case 6:
            return "sbyte"
        case 7:
            return "undefined"
        case 8:
            return "sshort"
        case 9:
            return "slong"
        case 10:
            return "srational"
        case 11:
            return "float"
        case 12:
            return "double"
        case _:
            if debug:
                print(f"DEBUG: {typeId:d} is not in Section 4.1.2 of \"N4378.pdf\"")
            return None
