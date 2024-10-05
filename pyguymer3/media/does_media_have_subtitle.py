#!/usr/bin/env python3

# Define function ...
def does_media_have_subtitle(
    fname,
    /,
    *,
         cwd = None,
       debug = __debug__,
    playlist = -1,
     timeout = 60.0,
):
    """
    Return True/False if the media has subtitle.
    """

    # Import sub-functions ...
    from .__ffprobe__ import __ffprobe__
    from .ffprobe import ffprobe

    # Make sure that this fname/playlist combination is in the global dictionary ...
    if fname not in __ffprobe__:
        __ffprobe__[fname] = {}
    if playlist not in __ffprobe__[fname]:
        if debug:
            print(f"INFO: Running ffprobe(\"{fname}\", {playlist:d}) ...")
        __ffprobe__[fname][playlist] = ffprobe(fname, cwd = cwd, playlist = playlist, timeout = timeout)

    # Loop over streams ...
    for stream in __ffprobe__[fname][playlist]["streams"]:
        # Skip stream if it is not subtitle ...
        if stream["codec_type"].strip().lower() != "subtitle":
            continue

        # Return answer ...
        return True

    # Return error ...
    return False
