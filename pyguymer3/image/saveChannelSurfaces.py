#!/usr/bin/env python3

# Define function ...
def saveChannelSurfaces(
    iName,
    oName,
    /,
    *,
             debug = __debug__,
    maxImagePixels = 1073741824,
):
    """Save channel surfaces as PNGs

    This function accepts a file path and surveys the number of red-ish,
    green-ish and blue-ish pixels in the image. It then saves 256 px × 256 px
    images of: all the most common red-ish pixels as a function of green and
    blue; all the most common green-ish pixels as a function of red and blue;
    and all the most common blue-ish pixels as a function of red and green.

    Parameters
    ----------
    iName : str
        the path to the input image
    oName : str
        the prefix to the output images
    debug : bool, optional
        print debug messages
    maxImagePixels : int, optional
        the maximum number of pixels in an image, to prevent decompression bombs

    Notes
    -----
    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import standard modules ...
    import os

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

    # Return if nothing needs doing ...
    if all(
        [
            os.path.exists(f"{oName}_reds.png"),
            os.path.exists(f"{oName}_greens.png"),
            os.path.exists(f"{oName}_blues.png"),
        ]
    ):
        return

    # Check input ...
    assert maxImagePixels < pow(2, 64), f"the maximum number of pixels (\"{maxImagePixels:,d}\") is larger than the range of the histogram (\"{pow(2, 64) - 1:,d}\")"

    # Open image as RGB (even if it is paletted) ...
    with PIL.Image.open(
        iName,
        mode = "r",
    ) as iObj:
        img = iObj.convert("RGB")

    # Convert PIL image to a NumPy array and clean up ...
    arr = numpy.array(img)
    img.close()
    del img

    # Check input ...
    assert arr.dtype == "uint8", f"the NumPy array is not 8-bit (\"{arr.dtype}\")"
    assert arr.ndim == 3, f"the NumPy array is not 3D (\"{arr.ndim:d}\")"

    # Calculate channel histograms and clean up ...
    # NOTE: Taking the red channel as a worked example, the logic here is that
    #       for every possible combination of green and blue (the first two axes
    #       in "rHist") the number of red-ish pixels (defined as being pixels
    #       where red is the dominant channel) is summed for each possible red
    #       value (the third axis).
    rHist = numpy.zeros(
        (256, 256, 256),                # NOTE: Indexed like (g, b, r).
        dtype = numpy.uint64,
    )                                                                           # [#]
    gHist = numpy.zeros(
        (256, 256, 256),                # NOTE: Indexed like (r, b, g).
        dtype = numpy.uint64,
    )                                                                           # [#]
    bHist = numpy.zeros(
        (256, 256, 256),                # NOTE: Indexed like (r, g, b).
        dtype = numpy.uint64,
    )                                                                           # [#]
    for iy in range(arr.shape[0]):
        for ix in range(arr.shape[1]):
            r, g, b = arr[iy, ix, :]
            if r >= g and r >= b:
                rHist[g, b, r] += 1                                             # [#]
            if g >= r and g >= b:
                gHist[r, b, g] += 1                                             # [#]
            if b >= r and b >= g:
                bHist[r, g, b] += 1                                             # [#]
    del arr

    # Calculate channel surfaces and clean up ...
    # NOTE: Taking the red channel as a worked example again, the logic here is
    #       that for every possible combination of green and blue (the height
    #       and width of the surface, respectively) set the pixel colour to be
    #       the most numerous red-ish colour present.
    rSurf = 127 * numpy.ones(
        (256, 256, 3),
        dtype = numpy.uint8,
    )
    gSurf = 127 * numpy.ones(
        (256, 256, 3),
        dtype = numpy.uint8,
    )
    bSurf = 127 * numpy.ones(
        (256, 256, 3),
        dtype = numpy.uint8,
    )
    for g in range(256):
        for b in range(256):
            r = rHist[g, b, :].argmax()
            if rHist[g, b, r] != 0:
                rSurf[g, b, 0] = r
                rSurf[g, b, 1] = g
                rSurf[g, b, 2] = b
    for r in range(256):
        for b in range(256):
            g = gHist[r, b, :].argmax()
            if gHist[r, b, g] != 0:
                gSurf[r, b, 0] = r
                gSurf[r, b, 1] = g
                gSurf[r, b, 2] = b
    for r in range(256):
        for g in range(256):
            b = bHist[r, g, :].argmax()
            if bHist[r, g, b] != 0:
                bSurf[r, g, 0] = r
                bSurf[r, g, 1] = g
                bSurf[r, g, 2] = b
    del rHist, gHist, bHist

    # Save channel surfaces and clean up ...
    with open(
        f"{oName}_reds.png",
        mode = "wb",
    ) as fObj:
        fObj.write(
            makePng(
                rSurf[::-1, :, :],
                calcAdaptive = True,
                 calcAverage = True,
                    calcNone = True,
                   calcPaeth = True,
                     calcSub = True,
                      calcUp = True,
                     choices = "all",
                       debug = debug,
                         dpi = None,
                      levels = [9,],
                   memLevels = [9,],
                     modTime = None,
                    palUint8 = None,
                  strategies = None,
                      wbitss = [15,],
            )
        )
    with open(
        f"{oName}_greens.png",
        mode = "wb",
    ) as fObj:
        fObj.write(
            makePng(
                gSurf[::-1, :, :],
                calcAdaptive = True,
                 calcAverage = True,
                    calcNone = True,
                   calcPaeth = True,
                     calcSub = True,
                      calcUp = True,
                     choices = "all",
                       debug = debug,
                         dpi = None,
                      levels = [9,],
                   memLevels = [9,],
                     modTime = None,
                    palUint8 = None,
                  strategies = None,
                      wbitss = [15,],
            )
        )
    with open(
        f"{oName}_blues.png",
        mode = "wb",
    ) as fObj:
        fObj.write(
            makePng(
                bSurf[::-1, :, :],
                calcAdaptive = True,
                 calcAverage = True,
                    calcNone = True,
                   calcPaeth = True,
                     calcSub = True,
                      calcUp = True,
                     choices = "all",
                       debug = debug,
                         dpi = None,
                      levels = [9,],
                   memLevels = [9,],
                     modTime = None,
                    palUint8 = None,
                  strategies = None,
                      wbitss = [15,],
            )
        )
    del rSurf, gSurf, bSurf
