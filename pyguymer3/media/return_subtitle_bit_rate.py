#!/usr/bin/env python3

# Define function ...
def return_subtitle_bit_rate(
    fname,
    /,
    *,
       cacheDir = "~/.cache/pyguymer3",
            cwd = None,
          debug = __debug__,
      ensureNFC = True,
    ffprobePath = None,
       playlist = -1,
        timeout = 60.0,
):
    """
    Return the bit rate of the first subtitle stream in the media file.
    """

    # Import standard modules ...
    import shutil

    # Import sub-functions ...
    from .__ffprobe__ import __ffprobe__
    from .ffprobe import ffprobe

    # **************************************************************************

    # Try to find the paths if the user did not provide them ...
    if ffprobePath is None:
        ffprobePath = shutil.which("ffprobe")
    assert ffprobePath is not None, "\"ffprobe\" is not installed"

    # **************************************************************************

    # Make sure that this fname/playlist combination is in the global dictionary ...
    if fname not in __ffprobe__:
        __ffprobe__[fname] = {}
    if playlist not in __ffprobe__[fname]:
        if debug:
            print(f"INFO: Running ffprobe(\"{fname}\", {playlist:d}) ...")
        __ffprobe__[fname][playlist] = ffprobe(
            fname,
               cacheDir = cacheDir,
                    cwd = cwd,
              ensureNFC = ensureNFC,
            ffprobePath = ffprobePath,
               playlist = playlist,
                timeout = timeout,
        )

    # Loop over streams ...
    for stream in __ffprobe__[fname][playlist]["streams"]:
        # Skip stream if it is not subtitle ...
        if stream["codec_type"].strip().lower() != "subtitle":
            continue

        # Return bit rate ...
        return int(stream["bit_rate"])                                          # [b/s]

    # Return error ...
    return -1
