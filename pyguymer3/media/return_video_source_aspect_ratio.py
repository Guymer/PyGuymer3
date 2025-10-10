#!/usr/bin/env python3

# Define function ...
def return_video_source_aspect_ratio(
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
    from ..find_integer_divisors import find_integer_divisors
    from .return_video_height import return_video_height
    from .return_video_width import return_video_width

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

        # Find common dimensions divisors ...
        w = return_video_width(
            fname,
               cacheDir = cacheDir,
                  debug = debug,
              ensureNFC = ensureNFC,
            ffprobePath = ffprobePath,
               playlist = playlist,
                timeout = timeout,
        )                                                                       # [px]
        h = return_video_height(
            fname,
               cacheDir = cacheDir,
                  debug = debug,
              ensureNFC = ensureNFC,
            ffprobePath = ffprobePath,
               playlist = playlist,
                timeout = timeout,
        )                                                                       # [px]
        w_divs = find_integer_divisors(w)
        h_divs = find_integer_divisors(h)
        fact = 1
        for w_div in reversed(w_divs):
            if w_div in h_divs:
                fact = w_div
                break

        # Return scaled dimensions as source aspect ratio ...
        return f"{w // fact:d}:{h // fact:d}"

    # Return error ...
    return "ERROR"
