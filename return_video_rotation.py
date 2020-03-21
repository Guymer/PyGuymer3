def return_video_rotation(fname, playlist = -1):
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

        # Return rotation ...
        if "tags" in stream:
            if "rotate" in stream["tags"]:
                return int(stream["tags"]["rotate"])                            # [deg]

    # Return default ...
    return 0                                                                    # [deg]
