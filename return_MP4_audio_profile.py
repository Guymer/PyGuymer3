def return_MP4_audio_profile(fname, playlist = -1, debug = False):
    # Load sub-functions ...
    from .__ffprobe__ import __ffprobe__
    from .ffprobe import ffprobe

    # Make sure that this fname/playlist combination is in the global dictionary ...
    if fname not in __ffprobe__:
        __ffprobe__[fname] = {}
    if playlist not in __ffprobe__[fname]:
        if debug:
            print("INFO: Running ffprobe(\"{:s}\", {:d}) ...".format(fname, playlist))
        __ffprobe__[fname][playlist] = ffprobe(fname, playlist = playlist)

    # Loop over streams ...
    for stream in __ffprobe__[fname][playlist]["streams"]:
        # Skip stream if it is not audio ...
        if stream["codec_type"].strip().lower() != "audio":
            continue

        # Skip stream if it is not AAC audio ...
        if stream["codec_name"].strip().upper() != "AAC":
            continue

        # Return profile ...
        return stream["profile"]

    # Return error ...
    return "ERROR"
