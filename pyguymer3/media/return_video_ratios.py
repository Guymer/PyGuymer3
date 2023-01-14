#!/usr/bin/env python3

# Define function ...
def return_video_ratios(fname, kwArgCheck = None, debug = False, playlist = -1):
    # Import sub-functions ...
    from .__ffprobe__ import __ffprobe__
    from .ffprobe import ffprobe
    from ..find_integer_divisors import find_integer_divisors
    from .return_video_display_aspect_ratio import return_video_display_aspect_ratio
    from .return_video_height import return_video_height
    from .return_video_pixel_aspect_ratio import return_video_pixel_aspect_ratio
    from .return_video_width import return_video_width

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
        # Skip stream if it is not video ...
        if stream["codec_type"].strip().lower() != "video":
            continue

        # Find common dimensions divisors ...
        w = return_video_width(fname, debug = debug, playlist = playlist)       # [px]
        h = return_video_height(fname, debug = debug, playlist = playlist)      # [px]
        w_divs = find_integer_divisors(w)
        h_divs = find_integer_divisors(h)
        fact = 1
        for w_div in reversed(w_divs):
            if w_div in h_divs:
                fact = w_div
                break

        # Create short-hands and then return them ...
        dar = return_video_display_aspect_ratio(fname, debug = debug, playlist = playlist)
        par = return_video_pixel_aspect_ratio(fname, debug = debug, playlist = playlist)
        sar = "{:d}:{:d}".format(w // fact, h // fact)
        return dar, par, sar

    # Return error ...
    return "ERROR", "ERROR", "ERROR"
