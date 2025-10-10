#!/usr/bin/env python3

# Define function ...
def return_video_width(
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
    # Import sub-functions ...
    from .return_video_rotation import return_video_rotation

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
        # Skip stream if it is not video ...
        if stream["codec_type"].strip().lower() != "video":
            continue

        # Check the rotation ...
        if return_video_rotation(
            fname,
               cacheDir = cacheDir,
                  debug = debug,
              ensureNFC = ensureNFC,
            ffprobePath = ffprobePath,
               playlist = playlist,
                timeout = timeout,
        ) in [0, 180]:
            # Return width ...
            return int(stream["width"])                                         # [px]
        if return_video_rotation(
            fname,
               cacheDir = cacheDir,
                  debug = debug,
              ensureNFC = ensureNFC,
            ffprobePath = ffprobePath,
               playlist = playlist,
                timeout = timeout,
        ) in [90, 270]:
            # Return height ...
            return int(stream["height"])                                        # [px]

    # Return error ...
    return -1                                                                   # [px]
