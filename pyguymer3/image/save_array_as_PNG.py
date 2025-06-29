#!/usr/bin/env python3

# Define function ...
def save_array_as_PNG(
    img,
    fname,
    /,
    *,
    calcAdaptive = True,
     calcAverage = True,
        calcNone = True,
       calcPaeth = True,
         calcSub = True,
          calcUp = True,
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
    calcAdaptive : bool, optional
        See :py:func:`pyguymer3.image.makePng` for the documentation.
    calcAverage : bool, optional
        See :py:func:`pyguymer3.image.makePng` for the documentation.
    calcNone : bool, optional
        See :py:func:`pyguymer3.image.makePng` for the documentation.
    calcPaeth : bool, optional
        See :py:func:`pyguymer3.image.makePng` for the documentation.
    calcSub : bool, optional
        See :py:func:`pyguymer3.image.makePng` for the documentation.
    calcUp : bool, optional
        See :py:func:`pyguymer3.image.makePng` for the documentation.
    debug : bool, optional
        Print debug messages.
    dpi : None or float or int, optional
        See :py:func:`pyguymer3.image.makePng` for the documentation.
    modTime : None or datetime.datetime, optional
        See :py:func:`pyguymer3.image.makePng` for the documentation.

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
    assert img.dtype == "uint8", f"the NumPy array is not 8-bit (\"{img.dtype}\")"
    assert img.ndim == 3, f"the NumPy array is not 3D (\"{img.ndim:d}\")"
    assert img.shape[2] == 3, "the NumPy array does not have 3 colour channels"

    # **************************************************************************

    # Make PNG source ...
    src = makePng(
        img,
        calcAdaptive = calcAdaptive,
         calcAverage = calcAverage,
            calcNone = calcNone,
           calcPaeth = calcPaeth,
             calcSub = calcSub,
              calcUp = calcUp,
             choices = "all",
               debug = debug,
                 dpi = dpi,
              levels = [9,],
           memLevels = [9,],
             modTime = modTime,
            palUint8 = None,
          strategies = None,
              wbitss = [15,],
    )

    # Write PNG ...
    with open(fname, "wb") as fObj:
        fObj.write(src)
