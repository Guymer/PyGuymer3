#!/usr/bin/env python3

# Define function ...
def return_audio_format(
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
    """Return the format of the first audio stream in a media file

    This function will return a pretty string of the format of the first audio
    stream in a media file.

    Parameters
    ----------
    fname : str
        the media file
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
        # Skip stream if it is not audio ...
        if stream["codec_type"].strip().lower() != "audio":
            continue

        # Return format ...
        if "codec_name" in stream:
            match stream["codec_name"]:
                case "aac":
                    return "AAC"
                case "adpcm_ima_qt":
                    return "Adaptive Differential PCM"
                case "alac":
                    return "ALAC"
                case "flac":
                    return "FLAC"
                case "mp3":
                    return "MP3"
                case "qdm2":
                    return "QDesign Music"
                case "vorbis":
                    return "Vorbis"
                case "wmapro":
                    return "WMA 9 Pro"
                case "wmav1":
                    return "WMA 1"
                case "wmav2":
                    return "WMA 2"
                case _:
                    raise ValueError(f'\"codec_name\" is an unexpected value ({repr(stream["codec_name"])})') from None

    # Return error ...
    return "ERROR"
