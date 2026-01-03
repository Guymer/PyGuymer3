#!/usr/bin/env python3

# Define function ...
def manuallyOptimisePng(
    pName,
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
    maxImagePixels: int = 1073741824,
           modTime = None,
):
    """Manually optimise a PNG image.

    This function will load a PNG image and recreate it in RAM. If the stored
    source in RAM is smaller than the source on disk then the PNG file will be
    overwritten.

    Parameters
    ----------
    pName : str
        the PNG file name
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
    maxImagePixels : int, optional
        The maximum number of pixels in an image, to prevent decompression bombs.
    modTime : None or datetime.datetime, optional
        See :py:func:`pyguymer3.image.makePng` for the documentation.

    Notes
    -----
    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import standard modules ...
    import os
    import sys

    # Import special modules ...
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None
    try:
        import PIL
        import PIL.Image
        PIL.Image.MAX_IMAGE_PIXELS = maxImagePixels                             # [px]
    except:
        raise Exception("\"PIL\" is not installed; run \"pip install --user Pillow\"") from None

    # Import sub-functions ...
    from .makePng import makePng

    # **************************************************************************

    # Check system ...
    assert sys.byteorder == "little", "the system is not little-endian"

    # **************************************************************************

    # Open image and make arrays ...
    with PIL.Image.open(pName) as iObj:
        match iObj.mode:
            case "L":
                arrUint8 = numpy.array(iObj).reshape((iObj.size[1], iObj.size[0], 1))
                palUint8 = None
            case "P":
                arrUint8 = numpy.array(iObj).reshape((iObj.size[1], iObj.size[0], 1))
                palUint8 = numpy.frombuffer(iObj.palette.tobytes(), dtype = numpy.uint8)
                palUint8 = palUint8.reshape((palUint8.size // 3, 3))
            case "RGB":
                arrUint8 = numpy.array(iObj)
                palUint8 = None
            case _:
                raise ValueError(f"the image has an unsupported mode (\"{iObj.mode}\")") from None

    # Check soon-to-be input ...
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

    # Check if the new source is smaller than the old source ...
    if len(src) >= os.path.getsize(pName):
        return

    if debug:
        print(f"Overwriting \"{pName}\" with optimised version ({len(src):,d} bytes < {os.path.getsize(pName):,d} bytes) ...")

    # Write PNG ...
    with open(pName, "wb") as fObj:
        fObj.write(src)
