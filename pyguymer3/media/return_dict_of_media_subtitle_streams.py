#!/usr/bin/env python3

# Define function ...
def return_dict_of_media_subtitle_streams(
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
    # Import sub-functions ...
    from .parse_MPLS_file import parse_MPLS_file

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

    # Initialize dictionary ...
    ans = {}

    # Loop over streams ...
    for stream in __ffprobe__[f"{fname}:{playlist:d}"]["streams"]:
        # Skip stream if it is incomplete ...
        if "codec_type" not in stream:
            continue

        # Skip stream if it is not subtitle ...
        if stream["codec_type"].strip().lower() != "subtitle":
            continue

        # Append information ...
        ans[str(stream["index"])] = stream

    # Check if it is a Blu-ray ...
    if fname.startswith("bluray:"):
        # Attempt to load the MPLS file for this playlist ...
        nfo = parse_MPLS_file(fname.removeprefix("bluray:"), playlist)

        # Check key ...
        if "PlayList" in nfo:
            # Check key ...
            if "PlayItems" in nfo["PlayList"]:
                # Loop over PlayItems ...
                for PlayItem in nfo["PlayList"]["PlayItems"]:
                    # Loop over subtitle stream list names ...
                    for name in ["PrimaryPGStreamEntries", "SecondaryPGStreamEntries"]:
                        # Loop over PGStreamEntries ...
                        for PGStreamEntry in PlayItem["STNTable"][name]:
                            # Check keys ...
                            if "StreamEntry" in PGStreamEntry and "StreamAttributes" in PGStreamEntry:
                                # Check keys ...
                                if "RefToStreamPID" in PGStreamEntry["StreamEntry"] and "LanguageCode" in PGStreamEntry["StreamAttributes"]:
                                    # Loop over streams ...
                                    for stream in ans:
                                        # Check if this is the stream ...
                                        if PGStreamEntry["StreamEntry"]["RefToStreamPID"] == ans[stream]["id"]:
                                            # Add language code to the stream
                                            # information ...
                                            ans[stream]["langcode"] = PGStreamEntry["StreamAttributes"]["LanguageCode"]

    # Make sure that each stream has a language code ...
    for stream in ans:
        if "langcode" not in ans[stream]:
            ans[stream]["langcode"] = "?"

    # Return dictionary ...
    return ans
