def return_video_display_aspect_ratio(fname, playlist = -1):
    # Load sub-functions ...
    from . import __ffprobe__
    from .ffprobe import ffprobe

    # Make sure that this fname/playlist combination is in the global dictionary ...
    if fname not in __ffprobe__:
        __ffprobe__[fname] = {}
    if playlist not in __ffprobe__[fname]:
        __ffprobe__[fname][playlist] = ffprobe(fname, playlist)

    # Loop over streams ...
    for stream in __ffprobe__[fname][playlist]["streams"]:
        # Skip stream if it is not video ...
        if stream["codec_type"].strip().lower() != "video":
            continue

        # Return display aspect ratio ...
        return stream["display_aspect_ratio"]

    # Return error ...
    return "ERROR"
