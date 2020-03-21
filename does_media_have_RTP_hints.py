def does_media_have_RTP_hints(fname, playlist = -1):
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
        # Skip stream if it is not data ...
        if stream["codec_type"].strip().lower() != "data":
            continue

        # Check if this data stream is RTP ...
        if stream["codec_tag_string"].strip().lower() == "rtp":
            # Return answer ...
            return True

    # Return answer ...
    return False
