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
    import sys

    # Import special modules ...
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None
    try:
        import PIL
        import PIL.Image
        PIL.Image.MAX_IMAGE_PIXELS = 1024 * 1024 * 1024                         # [px]
    except:
        raise Exception("\"PIL\" is not installed; run \"pip install --user Pillow\"") from None

    # Import sub-functions ...
    from .makePng import makePng

    # **************************************************************************

    # Check system ...
    assert sys.byteorder == "little", "the system is not little-endian"

    # **************************************************************************

    # Open image as RGB (even if it is paletted) ...
    with PIL.Image.open(pName) as iObj:
        img = iObj.convert("RGB")

    # Convert image to a NumPy array ...
    arrUint8 = numpy.array(img)
    assert arrUint8.dtype == "uint8", f"the NumPy array is not 8-bit (\"{arrUint8.dtype}\")"
    assert arrUint8.ndim == 3, f"the NumPy array is not 3D (\"{arrUint8.ndim:d}\")"
    assert arrUint8.shape[2] == 3, "the NumPy array does not have 3 colour channels"

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
            palUint8 = None,
          strategies = None,
              wbitss = [15,],
    )

    # Check if the new source is smaller than the old source ...
    if len(src) >= os.path.getsize(pName):
        return

    # Write PNG ...
    with open(pName, "wb") as fObj:
        fObj.write(src)
