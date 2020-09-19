def ffprobe(fname, playlist = -1):
    """
    This function will run "ffprobe" on a file and return the format and stream
    information as a dictionary.

    fname -- the file to be surveyed
    playlist -- the playlist within the Blu-ray folder structure to be surveyed
    """

    # Import standard modules ...
    import json
    import shutil
    import subprocess

    # Check input ...
    if fname.startswith("bluray:") and playlist < 0:
        raise Exception("a Blu-ray was specified but no playlist was supplied")

    # Check that "ffprobe" is installed ...
    if shutil.which("ffprobe") is None:
        raise Exception("\"ffprobe\" is not installed")

    # Check if it is a Blu-ray ...
    if fname.startswith("bluray:"):
        # Find stream info ...
        stderrout = subprocess.check_output(
            [
                "ffprobe",
                "-hide_banner",
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
            stderr = subprocess.STDOUT
        )
    else:
        # Attempt to survey the file ...
        try:
            # Find stream info ...
            stderrout = subprocess.check_output(
                [
                    "ffprobe",
                    "-hide_banner",
                    "-loglevel", "quiet",
                    "-probesize", "3G",
                    "-analyzeduration", "1800M",
                    "-print_format", "json",
                    "-show_format",
                    "-show_streams",
                    fname
                ],
                encoding = "utf-8",
                stderr = subprocess.STDOUT
            )
        except subprocess.CalledProcessError:
            # Fallback and attempt to find stream info as a raw M-JPEG stream ...
            stderrout = subprocess.check_output(
                [
                    "ffprobe",
                    "-hide_banner",
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
                stderr = subprocess.STDOUT
            )

    # Return ffprobe output as dictionary ...
    return json.loads(stderrout)
