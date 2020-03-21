def return_video_width(fname, playlist = -1):
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

        # Return width ...
        return int(stream["width"])                                             # [px]

    # Return error ...
    return -1
