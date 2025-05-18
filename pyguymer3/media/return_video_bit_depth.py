#!/usr/bin/env python3

# Define function ...
def return_video_bit_depth(
    fname,
    /,
    *,
            cwd = None,
          debug = __debug__,
      ensureNFC = True,
    ffprobePath = None,
       playlist = -1,
        timeout = 60.0,
):
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
                    cwd = cwd,
              ensureNFC = ensureNFC,
            ffprobePath = ffprobePath,
               playlist = playlist,
                timeout = timeout,
        )

    # Loop over streams ...
    for stream in __ffprobe__[fname][playlist]["streams"]:
        # Skip stream if it is not video ...
        if stream["codec_type"].strip().lower() != "video":
            continue

        # Return bit depth ...
        if "bits_per_raw_sample" in stream:
            return int(stream["bits_per_raw_sample"])                           # [b]

    # **************************************************************************

    # Loop over streams ...
    for stream in __ffprobe__[fname][playlist]["streams"]:
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
