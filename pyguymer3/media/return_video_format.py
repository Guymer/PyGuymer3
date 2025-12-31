#!/usr/bin/env python3

# Define function ...
def return_video_format(
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
    """Return the format of the first video stream in a media file

    This function will return a pretty string of the format of the first video
    stream in a media file.

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
        # Skip stream if it is not video ...
        if stream["codec_type"].strip().lower() != "video":
            continue

        # Return format ...
        if "codec_name" in stream:
            match stream["codec_name"]:
                case "av1":
                    return "AV1"
                case "flv1":
                    return "Sorenson Spark"
                case "h264":
                    return "H.264"
                case "mjpeg":
                    return "Motion JPEG"
                case "mpeg4":
                    return "Xvid"
                case "png":
                    return "PNG"
                case "svq3":
                    return "Sorenson"
                case "vp3":
                    return "VP3"
                case "vp4":
                    return "VP4"
                case "vp5":
                    return "VP5"
                case "vp6":
                    return "VP6"
                case "vp6a" | "vp6f":
                    return "VP6 Flash"
                case "vp7":
                    return "VP7"
                case "vp8":
                    return "VP8"
                case "vp9":
                    return "VP9"
                case "wmv1":
                    return "WMV 7"
                case "wmv2":
                    return "WMV 8"
                case "wmv3":
                    return "WMV 9"
                case _:
                    raise ValueError(f'\"codec_name\" is an unexpected value ({repr(stream["codec_name"])})') from None

    # Return error ...
    return "ERROR"
