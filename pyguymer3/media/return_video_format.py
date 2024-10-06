#!/usr/bin/env python3

# Define function ...
def return_video_format(
    fname,
    /,
    *,
            cwd = None,
          debug = __debug__,
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
    cwd : str, optional
        the directory to change to before running "ffprobe"
    debug : bool, optional
        print debug messages
    ffprobePath : str, optional
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

    # Import sub-functions ...
    from .__ffprobe__ import __ffprobe__
    from .ffprobe import ffprobe

    # Make sure that this fname/playlist combination is in the global dictionary ...
    if fname not in __ffprobe__:
        __ffprobe__[fname] = {}
    if playlist not in __ffprobe__[fname]:
        if debug:
            print(f"INFO: Running ffprobe(\"{fname}\", {playlist:d}) ...")
        __ffprobe__[fname][playlist] = ffprobe(
            fname,
                    cwd = cwd,
            ffprobePath = ffprobePath,
               playlist = playlist,
                timeout = timeout,
        )

    # Loop over streams ...
    for stream in __ffprobe__[fname][playlist]["streams"]:
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
