#!/usr/bin/env python3

# Define function ...
def does_media_have_RTP_hints(
    fname,
    /,
    *,
       cacheDir = "~/.cache/pyguymer3",
          debug = __debug__,
      ensureNFC = True,
    ffprobePath = None,
       playlist = -1,
        timeout = 60.0,
):
    # Import global (subclassed) dictionary ...
    from .__ffprobe__ import __ffprobe__

    # **************************************************************************

    # Configure global (subclassed) dictionary ...
    # NOTE: If I blindly set "__ffprobe__.ffprobePath" to "ffprobePath" each
    #       time then I would clobber any previous calls to "shutil.which()"
    #       performed by the global (subclassed) dictionary itself.
    __ffprobe__.cacheDir = cacheDir
    __ffprobe__.debug = debug
    __ffprobe__.ensureNFC = ensureNFC
    if ffprobePath is not None:
        __ffprobe__.ffprobePath = ffprobePath
    __ffprobe__.timeout = timeout                                               # [s]

    # **************************************************************************

    # Loop over streams ...
    for stream in __ffprobe__[f"{fname}:{playlist:d}"]["streams"]:
        # Skip stream if it is not data ...
        if stream["codec_type"].strip().lower() != "data":
            continue

        # Check if this data stream is RTP ...
        if stream["codec_tag_string"].strip().lower() == "rtp":
            # Return answer ...
            return True

    # Return answer ...
    return False
