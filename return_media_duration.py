def return_media_duration(fname, playlist = -1, debug = False):
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

    # Return duration ...
    form = __ffprobe__[fname][playlist]["format"]
    dur = -1.0                                                                  # [s]
    if "duration" in form:
        dur = float(form["duration"])                                           # [s]
    return dur
