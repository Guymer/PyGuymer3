#!/usr/bin/env python3

# Define function ...
def return_video_height(fname, /, *, debug = False, playlist = -1):
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
        __ffprobe__[fname][playlist] = ffprobe(fname, playlist = playlist)

    # Loop over streams ...
    for stream in __ffprobe__[fname][playlist]["streams"]:
        # Skip stream if it is not video ...
        if stream["codec_type"].strip().lower() != "video":
            continue

        # Check the rotation ...
        if return_video_rotation(fname, debug = debug, playlist = playlist) in [0, 180]:
            # Return height ...
            return int(stream["height"])                                        # [px]
        if return_video_rotation(fname, debug = debug, playlist = playlist) in [90, 270]:
            # Return width ...
            return int(stream["width"])                                         # [px]

    # Return error ...
    return -1                                                                   # [px]
