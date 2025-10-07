#!/usr/bin/env python3

# Define function ...
def return_audio_format(
    fname,
    /,
    *,
       cacheDir = "~/.cache/pyguymer3",
            cwd = None,
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

    # Import standard modules ...
    import shutil

    # Import sub-functions ...
    from .__ffprobe__ import __ffprobe__
    from .ffprobe import ffprobe

    # **************************************************************************

    # Try to find the paths if the user did not provide them ...
    if ffprobePath is None:
        ffprobePath = shutil.which("ffprobe")
    assert ffprobePath is not None, "\"ffprobe\" is not installed"

    # **************************************************************************

    # Make sure that this fname/playlist combination is in the global dictionary ...
    if fname not in __ffprobe__:
        __ffprobe__[fname] = {}
    if playlist not in __ffprobe__[fname]:
        if debug:
            print(f"INFO: Running ffprobe(\"{fname}\", {playlist:d}) ...")
        __ffprobe__[fname][playlist] = ffprobe(
            fname,
               cacheDir = cacheDir,
                    cwd = cwd,
              ensureNFC = ensureNFC,
            ffprobePath = ffprobePath,
               playlist = playlist,
                timeout = timeout,
        )

    # Loop over streams ...
    for stream in __ffprobe__[fname][playlist]["streams"]:
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
