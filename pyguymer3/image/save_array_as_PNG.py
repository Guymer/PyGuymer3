#!/usr/bin/env python3

# Define function ...
def save_array_as_PNG(
    img,
    fname,
    /,
    *,
    debug = __debug__,
      dpi = None,
):
    """Save an array as a PNG image.

    Parameters
    ----------
    img : numpy.ndarray
        a 3D NumPy array of uint8 type with shape (ny,nx,nc)
    fname : str
        output file name
    debug : bool, optional
        Print debug messages.
    dpi : None or float or int, optional
        If a number is passed then the ancillary "pHYs" chunk will get created
        and the resolution will be specified.

    Notes
    -----
    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import sub-functions ...
    from .makePng import makePng

    # **************************************************************************

    # Check input ...
    assert img.dtype == "uint8", "the NumPy array is not 8-bit"
    assert img.ndim == 3, "the NumPy array does not have a colour dimension"
    assert img.shape[2] == 3, "the NumPy array does not have 3 colour channels"

    # **************************************************************************

    # Make PNG source ...
    src = makePng(
        img,
           choices = "all",
             debug = debug,
               dpi = dpi,
            levels = [9,],
         memLevels = [9,],
        strategies = None,
            wbitss = [15,],
    )

    # Write PNG ...
    with open(fname, "wb") as fObj:
        fObj.write(src)
