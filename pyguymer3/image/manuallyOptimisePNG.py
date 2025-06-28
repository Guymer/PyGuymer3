#!/usr/bin/env python3

# Define function ...
def manuallyOptimisePNG(
    img,
    png,
    /,
    *,
           debug = __debug__,
             dpi = None,
    screenHeight = -1,
     screenWidth = -1,
):
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

    # **************************************************************************

    # Find out what the user supplied ...
    match img:
        case str():
            # Open image as RGB (even if it is paletted) ...
            with PIL.Image.open(img) as iObj:
                tmpImg = iObj.convert("RGB")

            # Check if the user wants to scale the image down to fit within a
            # screen size ...
            if screenWidth >= 100 and screenHeight >= 100:
                # Resize image in place ...
                tmpImg.thumbnail(
                    (screenWidth, screenHeight),
                    resample = PIL.Image.Resampling.LANCZOS,
                )
        case PIL.Image.Image():
            # Convert image to RGB ...
            tmpImg = img.convert("RGB")
        case _:
            # Crash ...
            raise TypeError(f"\"img\" is an unexpected type ({repr(type(img))})") from None

    # Convert image to a NumPy array ...
    inputArrUint8 = numpy.array(tmpImg)
    assert inputArrUint8.dtype == "uint8", "the NumPy array is not 8-bit"
    assert inputArrUint8.ndim == 3, "the NumPy array does not have a colour dimension"
    assert inputArrUint8.shape[2] == 3, "the NumPy array does not have 3 colour channels"

    # Make PNG source ...
    src = makePng(
        inputArrUint8,
           choices = "all",
             debug = debug,
               dpi = dpi,
            levels = [9,],
         memLevels = [9,],
        strategies = None,
            wbitss = [15,],
    )

    # Write PNG ...
    with open(png, "wb") as fObj:
        fObj.write(src)
