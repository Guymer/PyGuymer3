def return_video_bit_rate(fname, playlist = -1, debug = False):
    """
    Return the bit rate of the first video stream in the media file.
    """

    # Load sub-functions ...
    from . import __ffprobe__
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
        # Skip stream if it is not video ...
        if stream["codec_type"].strip().lower() != "video":
            continue

        # Return bit rate ...
        if "bit_rate" in stream:
            return int(stream["bit_rate"])                                      # [b/s]

    # Return error ...
    return -1                                                                   # [b/s]
