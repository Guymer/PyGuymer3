#!/usr/bin/env python3

# Define function ...
def save_array_as_PNG(
    arrUint8,
    fname,
    /,
    *,
    calcAdaptive: bool = True,
     calcAverage: bool = True,
        calcNone: bool = True,
       calcPaeth: bool = True,
         calcSub: bool = True,
          calcUp: bool = True,
           debug: bool = __debug__,
             dpi: None | int = None,
         modTime = None,
        palUint8 = None,
):
    """Save an array as a PNG image.

    Parameters
    ----------
    arrUint8 : numpy.ndarray
        A "height * width * colour" unsigned 8-bit integer NumPy array.
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
    palUint8 : None or numpy.ndarray, optional
        See :py:func:`pyguymer3.image.makePng` for the documentation.

    Notes
    -----
    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import standard modules ...
    import sys

    # Import sub-functions ...
    from .makePng import makePng

    # **************************************************************************

    # Check system ...
    assert sys.byteorder == "little", "the system is not little-endian"

    # Check input ...
    assert arrUint8.dtype == "uint8", f"the NumPy array is not 8-bit (\"{arrUint8.dtype}\")"
    assert arrUint8.ndim == 3, f"the NumPy array is not 3D (\"{arrUint8.ndim:d}\")"
    match arrUint8.shape[2]:
        case 1:
            if palUint8 is None:
                pass
            else:
                assert palUint8.dtype == "uint8", f"the NumPy palette is not 8-bit (\"{palUint8.dtype}\")"
                assert palUint8.ndim == 2, f"the NumPy palette is not 2D (\"{palUint8.ndim:d}\")"
                assert palUint8.shape[0] <= 256, f"the NumPy palette has more than 256 colours (\"{palUint8.shape[0]:,d}\")"
                assert palUint8.shape[1] == 3, "the NumPy palette does not have 3 colour channels"
                assert arrUint8.max() < palUint8.shape[0], f"the NumPy array references more colours than are in the NumPy palette (\"{arrUint8.max():d}\" -vs- \"{palUint8.shape[0]:d}\")"
        case 3:
            pass
        case _:
            raise ValueError(f"the NumPy array does not have either 1 or 3 colour channels (\"{arrUint8.shape[2]:d}\")") from None

    # **************************************************************************

    # Make PNG source ...
    src = makePng(
        arrUint8,
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
            palUint8 = palUint8,
          strategies = None,
              wbitss = [15,],
    )

    # Write PNG ...
    with open(fname, "wb") as fObj:
        fObj.write(src)
