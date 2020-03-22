def return_dict_of_media_video_streams(fname, playlist = -1):
    # Load sub-functions ...
    from . import __ffprobe__
    from .ffprobe import ffprobe

    # Make sure that this fname/playlist combination is in the global dictionary ...
    if fname not in __ffprobe__:
        __ffprobe__[fname] = {}
    if playlist not in __ffprobe__[fname]:
        __ffprobe__[fname][playlist] = ffprobe(fname, playlist)

    # Initialize dictionary ...
    ans = {}

    # Loop over streams ...
    for stream in __ffprobe__[fname][playlist]["streams"]:
        # Skip stream if it is incomplete ...
        if "codec_type" not in stream:
            continue

        # Skip stream if it is not video ...
        if stream["codec_type"].strip().lower() != "video":
            continue

        # Append information ...
        ans[str(stream["index"])] = stream

    # Return dictionary ...
    return ans
