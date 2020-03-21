def return_video_frame_rate(fname, playlist = -1):
    # Load sub-functions ...
    from . import __ffprobe
    from .ffprobe import ffprobe

    # Make sure that this fname/playlist combination is in the global dictionary ...
    if fname not in __ffprobe:
        __ffprobe[fname] = {}
    if playlist not in __ffprobe[fname]:
        __ffprobe[fname][playlist] = ffprobe(fname, playlist)

    # Loop over streams ...
    for stream in __ffprobe[fname][playlist]["streams"]:
        # Skip stream if it is not video ...
        if stream["codec_type"].strip().lower() != "video":
            continue

        # Check format ...
        if "/" not in stream["avg_frame_rate"]:
            raise Exception("\"avg_frame_rate\" did not contain a \"/\"")

        # Return frame rate ...
        a = stream["avg_frame_rate"].split("/")[0]                              # [#]
        b = stream["avg_frame_rate"].split("/")[1]                              # [s]
        fps = -1.0                                                              # [Hz]
        if int(b) != 0:
            fps = float(a) / float(b)                                           # [Hz]
        return fps

    # Return error ...
    return -1.0
