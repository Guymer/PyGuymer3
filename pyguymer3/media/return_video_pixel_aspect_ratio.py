#!/usr/bin/env python3

# Define function ...
def return_video_pixel_aspect_ratio(fname, /, *, cwd = None, debug = False, playlist = -1, timeout = 60.0):
    # Import sub-functions ...
    from .__ffprobe__ import __ffprobe__
    from .ffprobe import ffprobe

    # Make sure that this fname/playlist combination is in the global dictionary ...
    if fname not in __ffprobe__:
        __ffprobe__[fname] = {}
    if playlist not in __ffprobe__[fname]:
        if debug:
            print(f"INFO: Running ffprobe(\"{fname}\", {playlist:d}) ...")
        __ffprobe__[fname][playlist] = ffprobe(fname, cwd = cwd, playlist = playlist, timeout = timeout)

    # Loop over streams ...
    for stream in __ffprobe__[fname][playlist]["streams"]:
        # Skip stream if it is not video ...
        if stream["codec_type"].strip().lower() != "video":
            continue

        # Return pixel aspect ratio ...
        # NOTE: "ffprobe" incorrectly calls PAR "sample aspect ratio".
        if "sample_aspect_ratio" in stream:
            return stream["sample_aspect_ratio"]

    # Return error ...
    return "ERROR"
