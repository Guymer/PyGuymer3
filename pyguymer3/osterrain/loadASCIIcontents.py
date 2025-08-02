#!/usr/bin/env python3

# Define function ...
def loadASCIIcontents(
    fObj,
    n,
    /,
):
    """Load the content from the file

    Parameters
    ----------
    fObj : io.BytesIO
        the file object
    n : int
        the number of lines to skip before reading the content

    Returns
    -------
    contents : numpy.ndarray
        the file content

    Notes
    -----

    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import special modules ...
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None

    # **************************************************************************

    # Read ASCII dataset (and reset pointer) ...
    contents = numpy.loadtxt(
        fObj,
        delimiter = " ",
            dtype = numpy.float32,
         skiprows = n,
    )                                                                           # [m]
    fObj.seek(0)

    # Return contents ...
    return contents
