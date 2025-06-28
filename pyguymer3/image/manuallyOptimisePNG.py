#!/usr/bin/env python3

# Define function ...
def manuallyOptimisePNG(
    pName,
    /,
    *,
    debug = __debug__,
):
    # NOTE: Endian is not relevant for 8-bit integers.
    # NOTE: Think about using https://github.com/pycompression/python-isal

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

    # Open image as RGB (even if it is paletted) and convert it to a NumPy array ...
    with PIL.Image.open(pName) as iObj:
        inputImg = iObj.convert("RGB")
    inputArrUint8 = numpy.array(inputImg)
    assert inputArrUint8.dtype == "uint8", "the NumPy array is not 8-bit"
    assert inputArrUint8.ndim == 3, "the NumPy array does not have a colour dimension"
    assert inputArrUint8.shape[2] == 3, "the NumPy array does not have 3 colour channels"

    # Make PNG source ...
    src = makePng(
        inputArrUint8,
           choices = "all",
             debug = debug,
            levels = [9,],
         memLevels = [9,],
        strategies = None,
            wbitss = [15,],
    )

    # Write PNG ...
    with open("foobar.png", "wb") as fObj:
        fObj.write(src)
