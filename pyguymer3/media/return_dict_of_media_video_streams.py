def return_dict_of_media_video_streams(fname, kwArgCheck = None, playlist = -1, debug = False):
    # Load sub-functions ...
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
            print("INFO: Running ffprobe(\"{:s}\", {:d}) ...".format(fname, playlist))
        __ffprobe__[fname][playlist] = ffprobe(fname, playlist = playlist)

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
