#!/usr/bin/env python3

# Define function ...
def return_subtitle_format(
    fname,
    /,
    *,
       cacheDir = "~/.cache/pyguymer3",
          debug = __debug__,
      ensureNFC = True,
    ffprobePath = None,
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
    cacheDir : str, optional
        if a string, then it is the path to the local cache of "ffprobe" JSON
        output so as to save time in future calls
    cwd : str, optional
        the directory to change to before running "ffprobe"
    debug : bool, optional
        print debug messages
    ffprobePath : None or str, optional
        the path to the "ffprobe" binary (if not provided then Python will
        attempt to find the binary itself)
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

    # Import global (subclassed) dictionary ...
    from .__ffprobe__ import __ffprobe__

    # **************************************************************************

    # Configure global (subclassed) dictionary ...
    # NOTE: If I blindly set "__ffprobe__.ffprobePath" to "ffprobePath" each
    #       time then I would clobber any previous calls to "shutil.which()"
    #       performed by the global (subclassed) dictionary itself.
    __ffprobe__.cacheDir = cacheDir
    __ffprobe__.debug = debug
    __ffprobe__.ensureNFC = ensureNFC
    if ffprobePath is not None:
        __ffprobe__.ffprobePath = ffprobePath
    __ffprobe__.timeout = timeout                                               # [s]

    # **************************************************************************

    # Loop over streams ...
    for stream in __ffprobe__[f"{fname}:{playlist:d}"]["streams"]:
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
