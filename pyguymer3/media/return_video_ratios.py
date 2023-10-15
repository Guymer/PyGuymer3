#!/usr/bin/env python3

# Define function ...
def return_video_ratios(fname, /, *, cwd = None, debug = False, playlist = -1, timeout = 60.0):
    # Import sub-functions ...
    from .__ffprobe__ import __ffprobe__
    from .ffprobe import ffprobe
    from ..find_integer_divisors import find_integer_divisors
    from .return_video_display_aspect_ratio import return_video_display_aspect_ratio
    from .return_video_height import return_video_height
    from .return_video_pixel_aspect_ratio import return_video_pixel_aspect_ratio
    from .return_video_width import return_video_width

    # Make sure that this fname/playlist combination is in the global dictionary ...
    if fname not in __ffprobe__:
        __ffprobe__[fname] = {}
    if playlist not in __ffprobe__[fname]:
        if debug:
            print(f"INFO: Running ffprobe(\"{fname}\", {playlist:d}) ...")
        __ffprobe__[fname][playlist] = ffprobe(fname, cwd = cwd, playlist = playlist, timeout = timeout)

    # Loop over streams ...
    for stream in __ffprobe__[fname][playlist]["streams"]:
        # Skip stream if it is not video ...
        if stream["codec_type"].strip().lower() != "video":
            continue

        # Find common dimensions divisors ...
        w = return_video_width(
            fname,
                 cwd = cwd,
               debug = debug,
            playlist = playlist,
             timeout = timeout,
        )                                                                       # [px]
        h = return_video_height(
            fname,
                 cwd = cwd,
               debug = debug,
            playlist = playlist,
             timeout = timeout,
        )                                                                       # [px]
        w_divs = find_integer_divisors(w)
        h_divs = find_integer_divisors(h)
        fact = 1
        for w_div in reversed(w_divs):
            if w_div in h_divs:
                fact = w_div
                break

        # Create short-hands and then return them ...
        dar = return_video_display_aspect_ratio(
            fname,
                 cwd = cwd,
               debug = debug,
            playlist = playlist,
             timeout = timeout,
        )
        par = return_video_pixel_aspect_ratio(
            fname,
                 cwd = cwd,
               debug = debug,
            playlist = playlist,
             timeout = timeout,
        )
        sar = f"{w // fact:d}:{h // fact:d}"
        return dar, par, sar

    # Return error ...
    return "ERROR", "ERROR", "ERROR"
