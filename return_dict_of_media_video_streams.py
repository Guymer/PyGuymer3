# -*- coding: utf-8 -*-

def return_dict_of_media_video_streams(fname, playlist = None):
    # Check input ...
    if fname.startswith("bluray:") and playlist is None:
        raise Exception("a Blu-ray was specified but no playlist was supplied")

    # Import modules ...
    import json
    import subprocess

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

        # Skip stream if it is not video ...
        if stream["codec_type"].strip().lower() != "video":
            continue

        # Append information ...
        ans[str(stream["index"])] = stream

    # Return dictionary ...
    return ans
