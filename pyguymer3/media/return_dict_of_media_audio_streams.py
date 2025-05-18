#!/usr/bin/env python3

# Define function ...
def return_dict_of_media_audio_streams(
    fname,
    /,
    *,
            cwd = None,
          debug = __debug__,
      ensureNFC = True,
    ffprobePath = None,
       playlist = -1,
        timeout = 60.0,
):
    # Import standard modules ...
    import shutil

    # Import sub-functions ...
    from .__ffprobe__ import __ffprobe__
    from .ffprobe import ffprobe
    from .parse_MPLS_file import parse_MPLS_file

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
                    cwd = cwd,
              ensureNFC = ensureNFC,
            ffprobePath = ffprobePath,
               playlist = playlist,
                timeout = timeout,
        )

    # Initialize dictionary ...
    ans = {}

    # Loop over streams ...
    for stream in __ffprobe__[fname][playlist]["streams"]:
        # Skip stream if it is incomplete ...
        if "codec_type" not in stream:
            continue

        # Skip stream if it is not audio ...
        if stream["codec_type"].strip().lower() != "audio":
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
                    # Loop over audio stream list names ...
                    for name in ["PrimaryAudioStreamEntries", "SecondaryAudioStreamEntries"]:
                        # Loop over AudioStreamEntries ...
                        for AudioStreamEntry in PlayItem["STNTable"][name]:
                            # Check keys ...
                            if "StreamEntry" in AudioStreamEntry and "StreamAttributes" in AudioStreamEntry:
                                # Check keys ...
                                if "RefToStreamPID" in AudioStreamEntry["StreamEntry"] and "LanguageCode" in AudioStreamEntry["StreamAttributes"]:
                                    # Loop over streams ...
                                    for stream in ans:
                                        # Check if this is the stream ...
                                        if AudioStreamEntry["StreamEntry"]["RefToStreamPID"] == ans[stream]["id"]:
                                            # Add language code to the stream
                                            # information ...
                                            ans[stream]["langcode"] = AudioStreamEntry["StreamAttributes"]["LanguageCode"]

    # Make sure that each stream has a language code ...
    for stream in ans:
        if "langcode" not in ans[stream]:
            ans[stream]["langcode"] = "?"

    # Return dictionary ...
    return ans
