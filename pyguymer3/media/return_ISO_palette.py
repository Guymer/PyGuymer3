#!/usr/bin/env python3

# Define function ...
def return_ISO_palette(
    fname,
    /,
    *,
     cacheDir = "~/.cache/pyguymer3",
          cwd = None,
        debug = __debug__,
    ensureNFC = True,
    lsdvdPath = None,
      timeout = 60.0,
        track = -1,
):
    # Import standard modules ...
    import shutil

    # Import special modules ...
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None

    # Import sub-functions ...
    from .__lsdvd__ import __lsdvd__
    from .lsdvd import lsdvd
    from .yuv2rgb import yuv2rgb

    # **************************************************************************

    # Try to find the paths if the user did not provide them ...
    if lsdvdPath is None:
        lsdvdPath = shutil.which("lsdvd")
    assert lsdvdPath is not None, "\"lsdvd\" is not installed"

    # Check input ...
    if track == -1:
        raise Exception("no track was requested") from None

    # **************************************************************************

    # Make sure that this fname is in the global dictionary ...
    if fname not in __lsdvd__:
        if debug:
            print(f"INFO: Running lsdvd(\"{fname}\") ...")
        __lsdvd__[fname] = lsdvd(
            fname,
             cacheDir = cacheDir,
                  cwd = cwd,
                debug = debug,
            ensureNFC = ensureNFC,
            lsdvdPath = lsdvdPath,
              timeout = timeout,
        )

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
