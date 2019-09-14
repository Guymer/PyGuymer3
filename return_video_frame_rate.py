def return_video_frame_rate(fname, playlist = None):
    # Check input ...
    if fname.startswith("bluray:") and playlist is None:
        raise Exception("a Blu-ray was specified but no playlist was supplied")

    # Import modules ...
    import json
    import subprocess

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
            encoding = "utf-8",
            stderr = subprocess.PIPE,
            stdout = subprocess.PIPE
        )
        stdout, stderr = proc.communicate()
        if proc.returncode != 0:
            raise Exception("\"ffprobe\" command failed")
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
            encoding = "utf-8",
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
                encoding = "utf-8",
                stderr = subprocess.PIPE,
                stdout = subprocess.PIPE
            )
            stdout, stderr = proc.communicate()
            if proc.returncode != 0:
                raise Exception("\"ffprobe\" command failed")

    # Loop over streams ...
    for stream in json.loads(stdout)["streams"]:
        # Skip stream if it is not video ...
        if stream["codec_type"].strip().lower() != "video":
            continue

        # Check format ...
        if "/" not in stream["avg_frame_rate"]:
            raise Exception("\"avg_frame_rate\" did not contain a \"/\"")

        # Return frame rate ...
        a = stream["avg_frame_rate"].split("/")[0]                              # [#]
        b = stream["avg_frame_rate"].split("/")[1]                              # [s]
        fps = -1.0                                                              # [Hz]
        if int(b) != 0:
            fps = float(a) / float(b)                                           # [Hz]
        return fps

    # Return error ...
    return -1.0
