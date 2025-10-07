#!/usr/bin/env python3

# Define function ...
def return_ISO_video_frame_rate(
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

    # Import sub-functions ...
    from .__lsdvd__ import __lsdvd__
    from .lsdvd import lsdvd

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

        # Return answer ...
        return float(trackInfo["fps"])
