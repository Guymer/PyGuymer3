def ffprobe(fname, playlist = -1):
    """
    This function will run "ffprobe" on a file and return the stream information
    as a dictionary.

    fname -- the file to be surveyed
    playlist -- the playlist within the Blu-ray folder structure to be surveyed
    """

    # Import standard modules ...
    import json
    import os
    import subprocess

    # Check input ...
    if fname.startswith("bluray:") and playlist < 0:
        raise Exception("a Blu-ray was specified but no playlist was supplied")

    # Check if it is a Blu-ray ...
    if fname.startswith("bluray:"):
        # Find stream info ...
        stdout = subprocess.check_output(
            [
                "ffprobe",
                "-loglevel", "quiet",
                "-probesize", "3G",
                "-analyzeduration", "1800M",
                "-print_format", "json",
                "-show_format",
                "-show_streams",
                "-playlist", "{0:d}".format(playlist),
                fname
            ],
            encoding = "utf-8",
            stderr = open(os.devnull, "wt")
        )
    else:
        # Attempt to survey the file ...
        try:
            # Find stream info ...
            stdout = subprocess.check_output(
                [
                    "ffprobe",
                    "-loglevel", "quiet",
                    "-probesize", "3G",
                    "-analyzeduration", "1800M",
                    "-print_format", "json",
                    "-show_format",
                    "-show_streams",
                    fname
                ],
                encoding = "utf-8",
                stderr = open(os.devnull, "wt")
            )
        except subprocess.CalledProcessError:
            # Fallback and attempt to find stream info as a raw M-JPEG stream ...
            stdout = subprocess.check_output(
                [
                    "ffprobe",
                    "-loglevel", "quiet",
                    "-probesize", "3G",
                    "-analyzeduration", "1800M",
                    "-print_format", "json",
                    "-show_format",
                    "-show_streams",
                    "-f", "mjpeg",
                    fname
                ],
                encoding = "utf-8",
                stderr = open(os.devnull, "wt")
            )

    # Return ffprobe output as dictionary ...
    return json.loads(stdout)
