#!/usr/bin/env python3

# Define function ...
def return_video_frame_rate(
    fname,
    /,
    *,
         cwd = None,
       debug = __debug__,
    playlist = -1,
     timeout = 60.0,
):
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

        # Check format ...
        if "/" not in stream["avg_frame_rate"]:
            raise Exception("\"avg_frame_rate\" did not contain a \"/\"") from None

        # Return frame rate ...
        a = stream["avg_frame_rate"].split("/")[0]                              # [#]
        b = stream["avg_frame_rate"].split("/")[1]                              # [s]
        fps = -1.0                                                              # [Hz]
        if int(b) != 0:
            fps = float(a) / float(b)                                           # [Hz]
        return fps                                                              # [Hz]

    # Return error ...
    return -1.0                                                                 # [Hz]
