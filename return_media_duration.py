def return_media_duration(fname, playlist = -1):
    # Load sub-functions ...
    from . import __ffprobe
    from .ffprobe import ffprobe

    # Make sure that this fname/playlist combination is in the global dictionary ...
    if fname not in __ffprobe:
        __ffprobe[fname] = {}
    if playlist not in __ffprobe[fname]:
        __ffprobe[fname][playlist] = ffprobe(fname, playlist)

    # Return duration ...
    form = __ffprobe[fname][playlist]["format"]
    dur = -1.0                                                                  # [s]
    if "duration" in form:
        dur = float(form["duration"])                                           # [s]
    return dur
