def return_dict_of_media_audio_streams(fname, playlist = None):
    # Check input ...
    if fname.startswith("bluray:") and playlist is None:
        raise Exception("a Blu-ray was specified but no playlist was supplied")

    # Import modules ...
    import json
    import subprocess

    # Load sub-functions ...
    from .parse_MPLS_file import parse_MPLS_file

    # Create empty dictionary ...
    ans = {}

    # Find stream info ...
    if fname.startswith("bluray:"):
        proc = subprocess.Popen(
            [
                "ffprobe",
                "-loglevel", "quiet",
                "-probesize", "3G",
                "-analyzeduration", "1800M",
                "-print_format", "json",
                "-show_streams",
                "-playlist", "{0:d}".format(playlist),
                fname
            ],
            stderr = subprocess.PIPE,
            stdout = subprocess.PIPE
        )
        stdout, stderr = proc.communicate()
        if proc.returncode != 0:
            print("WARNING: \"ffprobe\" command failed on playlist \"{0:d}\" of \"{1:s}\"".format(playlist, fname))
            return ans
    else:
        proc = subprocess.Popen(
            [
                "ffprobe",
                "-loglevel", "quiet",
                "-probesize", "3G",
                "-analyzeduration", "1800M",
                "-print_format", "json",
                "-show_streams",
                fname
            ],
            stderr = subprocess.PIPE,
            stdout = subprocess.PIPE
        )
        stdout, stderr = proc.communicate()
        if proc.returncode != 0:
            # HACK: Fallback and attempt to load it as a raw M-JPEG stream.
            proc = subprocess.Popen(
                [
                    "ffprobe",
                    "-loglevel", "quiet",
                    "-probesize", "3G",
                    "-analyzeduration", "1800M",
                    "-print_format", "json",
                    "-show_streams",
                    "-f", "mjpeg",
                    fname
                ],
                stderr = subprocess.PIPE,
                stdout = subprocess.PIPE
            )
            stdout, stderr = proc.communicate()
            if proc.returncode != 0:
                print("WARNING: \"ffprobe\" command failed on \"{0:s}\"".format(fname))
                return ans

    # Loop over streams ...
    for stream in json.loads(stdout)["streams"]:
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
        nfo = parse_MPLS_file(
            br = fname[len("bluray:"):],
            ip = playlist
        )

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
                                    for stream in ans.keys():
                                        # Check if this is the stream ...
                                        if AudioStreamEntry["StreamEntry"]["RefToStreamPID"] == ans[stream]["id"]:
                                            # Add language code to the stream information ...
                                            ans[stream]["langcode"] = AudioStreamEntry["StreamAttributes"]["LanguageCode"]

    # Make sure that each stream has a language code ...
    for stream in ans.keys():
        if "langcode" not in ans[stream]:
            ans[stream]["langcode"] = "?"

    # Return dictionary ...
    return ans
