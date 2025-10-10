#!/usr/bin/env python3

# Define function ...
def return_video_bit_depth(
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
        # Skip stream if it is not video ...
        if stream["codec_type"].strip().lower() != "video":
            continue

        # Return bit depth ...
        if "bits_per_raw_sample" in stream:
            return int(stream["bits_per_raw_sample"])                           # [b]

    # **************************************************************************

    # Loop over streams ...
    for stream in __ffprobe__[f"{fname}:{playlist:d}"]["streams"]:
        # Skip stream if it is not video ...
        if stream["codec_type"].strip().lower() != "video":
            continue

        # Return bit depth ...
        # HACK: As of 29/Nov/2023, "ffprobe" does not populate the standard
        #       "bits_per_raw_sample" field of HEVC "Main 10" videos.
        if stream["codec_name"] == "hevc" and stream["profile"] == "Main 10":
            return 10                                                           # [b]

        # Return bit depth ...
        # HACK: As of 2/Dec/2023, "ffprobe" does not populate the standard
        #       "bits_per_raw_sample" field of VP9 "Profile 0" videos.
        if stream["codec_name"] == "vp9" and stream["profile"] == "Profile 0":
            return 8                                                            # [b]

    # **************************************************************************

    # Return error ...
    return -1                                                                   # [b]
