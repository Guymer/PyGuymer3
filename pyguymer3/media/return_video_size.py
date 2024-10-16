#!/usr/bin/env python3

# Define function ...
def return_video_size(
    fname,
    /,
    *,
            cwd = None,
          debug = __debug__,
    ffprobePath = None,
       playlist = -1,
        timeout = 60.0,
):
    # Import sub-functions ...
    from .__ffprobe__ import __ffprobe__
    from .ffprobe import ffprobe
    from .return_video_rotation import return_video_rotation

    # Make sure that this fname/playlist combination is in the global dictionary ...
    if fname not in __ffprobe__:
        __ffprobe__[fname] = {}
    if playlist not in __ffprobe__[fname]:
        if debug:
            print(f"INFO: Running ffprobe(\"{fname}\", {playlist:d}) ...")
        __ffprobe__[fname][playlist] = ffprobe(
            fname,
                    cwd = cwd,
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
                    cwd = cwd,
                  debug = debug,
            ffprobePath = ffprobePath,
               playlist = playlist,
                timeout = timeout,
        ) in [0, 180]:
            # Return width and height ...
            return int(stream["width"]), int(stream["height"])                  # [px], [px]
        if return_video_rotation(
            fname,
                    cwd = cwd,
                  debug = debug,
            ffprobePath = ffprobePath,
               playlist = playlist,
                timeout = timeout,
        ) in [90, 270]:
            # Return height and width ...
            return int(stream["height"]), int(stream["width"])                  # [px], [px]

    # Return error ...
    return -1, -1                                                               # [px], [px]
