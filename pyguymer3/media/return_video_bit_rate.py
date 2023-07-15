#!/usr/bin/env python3

# Define function ...
def return_video_bit_rate(fname, /, *, debug = False, playlist = -1, timeout = 60.0):
    """
    Return the bit rate of the first video stream in the media file.
    """

    # Import sub-functions ...
    from .__ffprobe__ import __ffprobe__
    from .ffprobe import ffprobe

    # Make sure that this fname/playlist combination is in the global dictionary ...
    if fname not in __ffprobe__:
        __ffprobe__[fname] = {}
    if playlist not in __ffprobe__[fname]:
        if debug:
            print(f"INFO: Running ffprobe(\"{fname}\", {playlist:d}) ...")
        __ffprobe__[fname][playlist] = ffprobe(fname, playlist = playlist, timeout = timeout)

    # Loop over streams ...
    for stream in __ffprobe__[fname][playlist]["streams"]:
        # Skip stream if it is not video ...
        if stream["codec_type"].strip().lower() != "video":
            continue

        # Return bit rate ...
        if "bit_rate" in stream:
            return int(stream["bit_rate"])                                      # [b/s]

    # Return error ...
    return -1                                                                   # [b/s]
