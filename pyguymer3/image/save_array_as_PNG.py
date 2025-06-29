#!/usr/bin/env python3

# Define function ...
def save_array_as_PNG(
    img,
    fname,
    /,
    *,
      debug = __debug__,
        dpi = None,
    modTime = None,
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
    modTime : None or datetime.datetime, optional
        The image last-modification time.

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
    assert img.dtype == "uint8", f"the NumPy array is not 8-bit (\"{arrUint8.dtype}\")"
    assert img.ndim == 3, f"the NumPy array is not 3D (\"{arrUint8.ndim:d}\")"
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
           modTime = modTime,
        strategies = None,
            wbitss = [15,],
    )

    # Write PNG ...
    with open(fname, "wb") as fObj:
        fObj.write(src)
