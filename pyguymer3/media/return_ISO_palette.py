#!/usr/bin/env python3

# Define function ...
def return_ISO_palette(
    fname,
    /,
    *,
     cacheDir = "~/.cache/pyguymer3",
        debug = __debug__,
    ensureNFC = True,
    lsdvdPath = None,
      timeout = 60.0,
        track = -1,
):
    # Import special modules ...
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None

    # Import sub-functions ...
    from .yuv2rgb import yuv2rgb

    # Import global (subclassed) dictionary ...
    from .__lsdvd__ import __lsdvd__

    # **************************************************************************

    # Configure global (subclassed) dictionary ...
    # NOTE: If I blindly set "__lsdvd__.lsdvdPath" to "lsdvdPath" each time then
    #       I would clobber any previous calls to "shutil.which()" performed by
    #       the global (subclassed) dictionary itself.
    __lsdvd__.cacheDir = cacheDir
    __lsdvd__.debug = debug
    __lsdvd__.ensureNFC = ensureNFC
    if lsdvdPath is not None:
        __lsdvd__.lsdvdPath = lsdvdPath
    __lsdvd__.timeout = timeout                                                 # [s]

    # Check input ...
    if track == -1:
        raise Exception("no track was requested") from None

    # **************************************************************************

    # Loop over all tracks ...
    for trackInfo in __lsdvd__[fname]["track"]:
        # Skip if this track is not the chosen one ...
        if int(trackInfo["ix"]) != int(track):
            continue

        # Create empty list ...
        vals = []

        # Loop over all colours in the palette ...
        for color in trackInfo["palette"]["color"]:
            # Convert YUV to RGB ...
            yuv = numpy.zeros((1, 1, 3), dtype = numpy.uint8)
            yuv[0, 0, 0] = int(color[0:2], 16)
            yuv[0, 0, 1] = int(color[2:4], 16)
            yuv[0, 0, 2] = int(color[4:6], 16)
            rgb = yuv2rgb(yuv, version = "SDTV")
            vals.append(format(rgb[0, 0, 0], "x").rjust(2, "0") + format(rgb[0, 0, 1], "x").rjust(2, "0") + format(rgb[0, 0, 2], "x").rjust(2, "0"))

        # Return answer ...
        return ",".join(vals)
