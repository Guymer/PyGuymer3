#!/usr/bin/env python3

# Define function ...
def return_media_format(
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
    """Return the format of a media file

    This function will return a pretty string of the format of the container
    used by a media file.

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

    # Determine the format of the file container ...
    # NOTE: Unhelpfully, "ffprobe" returns "mov,mp4,m4a,3gp,3g2,mj2" for both
    #       proprietary Apple QuickTime MOV files and ISO standard MP4 files. To
    #       find out what sort of file it is, the "major_brand" must be studied
    #       too. See these references:
    #         * "ISO Base Media File Format", https://www.loc.gov/preservation/digital/formats/fdd/fdd000079.shtml
    #         * "MPEG-4 File Format, Version 1", https://www.loc.gov/preservation/digital/formats/fdd/fdd000037.shtml
    #         * "MPEG-4 File Format, Version 2", https://www.loc.gov/preservation/digital/formats/fdd/fdd000155.shtml
    match __ffprobe__[f"{fname}:{playlist:d}"]["format"]["format_name"]:
        case "asf":
            return "ASF"
        case "avi":
            return "AVI"
        case "flac":
            return "FLAC"
        case "flv":
            return "FLV"
        case "mov,mp4,m4a,3gp,3g2,mj2":
            match __ffprobe__[f"{fname}:{playlist:d}"]["format"]["tags"].get("major_brand", "qt  "):
                case "3gp4" | "3gp5" | "3gp6":
                    return "3GPP"
                case "3g2a" | "3g2b":
                    return "3GPP2"
                case "isom":
                    return "MP4 (ISO/IEC 14496-12)"
                case "M4A ":
                    # NOTE: By reading "libavformat/movenc.c" in the "ffmpeg"
                    #       Git repository on 19/July/2024, the "ipod" format is
                    #       the same as the "mp4" format but it contains a
                    #       "uuid" atom to enable it to play on certain devices.
                    return "iPod-Compatible M4A (ISO/IEC 14496-12)"
                case "M4V ":
                    # NOTE: By reading "libavformat/movenc.c" in the "ffmpeg"
                    #       Git repository on 19/July/2024, the "ipod" format is
                    #       the same as the "mp4" format but it contains a
                    #       "uuid" atom to enable it to play on certain devices.
                    return "iPod-Compatible M4V (ISO/IEC 14496-12)"
                case "mp41":
                    return "MP4 (ISO/IEC 14496-1:2001)"
                case "mp42":
                    return "MP4 (ISO/IEC 14496-14:2003)"
                case "qt  ":
                    return "MOV"
                case _:
                    raise ValueError(f'\"format::tags::major_brand\" is an unexpected value ({repr(__ffprobe__[f"{fname}:{playlist:d}"]["format"]["tags"]["major_brand"])})') from None
        case "mp3":
            return "MP3"
        case "ogg":
            return "OGG"
        case "swf":
            return "SWF"
        case _:
            raise ValueError(f'\"format::format_name\" is an unexpected value ({repr(__ffprobe__[f"{fname}:{playlist:d}"]["format"]["format_name"])})') from None
