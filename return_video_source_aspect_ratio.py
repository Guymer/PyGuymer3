def return_video_source_aspect_ratio(fname, playlist = -1, debug = False):
    # Load sub-functions ...
    from . import __ffprobe__
    from .ffprobe import ffprobe
    from .find_integer_divisors import find_integer_divisors

    # Make sure that this fname/playlist combination is in the global dictionary ...
    if fname not in __ffprobe__:
        __ffprobe__[fname] = {}
    if playlist not in __ffprobe__[fname]:
        if debug:
            print("INFO: Running ffprobe(\"{:s}\", {:d}) ...".format(fname, playlist))
        __ffprobe__[fname][playlist] = ffprobe(fname, playlist)

    # Loop over streams ...
    for stream in __ffprobe__[fname][playlist]["streams"]:
        # Skip stream if it is not video ...
        if stream["codec_type"].strip().lower() != "video":
            continue

        # Find common dimensions divisors ...
        w_divs = find_integer_divisors(stream["width"])
        h_divs = find_integer_divisors(stream["height"])
        fact = 1
        for w_div in reversed(w_divs):
            if w_div in h_divs:
                fact = w_div
                break

        # Return scaled dimensions as source aspect ratio ...
        return "{0:d}:{1:d}".format(stream["width"] / fact, stream["height"] / fact)

    # Return error ...
    return "ERROR"
