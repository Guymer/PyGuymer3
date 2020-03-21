def return_video_ratios(fname, playlist = -1):
    # Load sub-functions ...
    from . import __ffprobe
    from .ffprobe import ffprobe
    from .find_integer_divisors import find_integer_divisors

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

        # Find common dimensions divisors ...
        w_divs = find_integer_divisors(stream["width"])
        h_divs = find_integer_divisors(stream["height"])
        fact = 1
        for w_div in reversed(w_divs):
            if w_div in h_divs:
                fact = w_div
                break

        # Create short-hands and then return them ...
        # NOTE: "ffmpeg" incorrectly calls PAR "sample aspect ratio".
        dar = stream["display_aspect_ratio"]
        par = stream["sample_aspect_ratio"]
        sar = "{0:d}:{1:d}".format(stream["width"] / fact, stream["height"] / fact)
        return dar, par, sar

    # Return error ...
    return "ERROR", "ERROR", "ERROR"
