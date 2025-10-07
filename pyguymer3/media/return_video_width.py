#!/usr/bin/env python3

# Define function ...
def return_video_width(
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
    # Import standard modules ...
    import shutil

    # Import sub-functions ...
    from .__ffprobe__ import __ffprobe__
    from .ffprobe import ffprobe
    from .return_video_rotation import return_video_rotation

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
        # Skip stream if it is not video ...
        if stream["codec_type"].strip().lower() != "video":
            continue

        # Check the rotation ...
        if return_video_rotation(
            fname,
               cacheDir = cacheDir,
                    cwd = cwd,
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
                    cwd = cwd,
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
