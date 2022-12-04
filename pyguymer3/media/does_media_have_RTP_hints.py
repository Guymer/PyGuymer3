def does_media_have_RTP_hints(fname, kwArgCheck = None, debug = False, playlist = -1):
    # Import sub-functions ...
    from .__ffprobe__ import __ffprobe__
    from .ffprobe import ffprobe

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Make sure that this fname/playlist combination is in the global dictionary ...
    if fname not in __ffprobe__:
        __ffprobe__[fname] = {}
    if playlist not in __ffprobe__[fname]:
        if debug:
            print(f"INFO: Running ffprobe(\"{fname}\", {playlist:d}) ...")
        __ffprobe__[fname][playlist] = ffprobe(fname, playlist = playlist)

    # Loop over streams ...
    for stream in __ffprobe__[fname][playlist]["streams"]:
        # Skip stream if it is not data ...
        if stream["codec_type"].strip().lower() != "data":
            continue

        # Check if this data stream is RTP ...
        if stream["codec_tag_string"].strip().lower() == "rtp":
            # Return answer ...
            return True

    # Return answer ...
    return False
