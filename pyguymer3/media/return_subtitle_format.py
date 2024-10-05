#!/usr/bin/env python3

# Define function ...
def return_subtitle_format(
    fname,
    /,
    *,
         cwd = None,
       debug = __debug__,
    playlist = -1,
     timeout = 60.0,
):
    """Return the format of the first subtitle stream in a media file

    This function will return a pretty string of the format of the first
    subtitle stream in a media file.

    Parameters
    ----------
    fname : str
        the media file
    cwd : str, optional
        the directory to change to before running "ffprobe"
    debug : bool, optional
        print debug messages
    playlist : int, optional
        for media files containing playlists, specify which playlist wants to be
        surveyed
    timeout : float, optional
        the timeout for any requests/subprocess calls

    Returns
    -------
    fmt : str
        the format as a pretty string

    Notes
    -----
    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
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

        # Return format ...
        if "codec_name" in stream:
            match stream["codec_name"]:
                case _:
                    raise ValueError(f'\"codec_name\" is an unexpected value ({repr(stream["codec_name"])})') from None

    # Return error ...
    return "ERROR"
