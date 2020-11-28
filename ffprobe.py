def ffprobe(fname, playlist = -1):
    """
    This function will run "ffprobe" on a file and return the format and stream
    information as a dictionary.

    fname -- the file to be surveyed
    playlist -- the playlist within the Blu-ray folder structure to be surveyed
    """

    # Import standard modules ...
    import json
    import os
    import shutil
    import subprocess

    # Check input ...
    if fname.startswith("bluray:") and playlist < 0:
        raise Exception("a Blu-ray was specified but no playlist was supplied") from None

    # Check that "ffprobe" is installed ...
    if shutil.which("ffprobe") is None:
        raise Exception("\"ffprobe\" is not installed") from None

    # Check if it is a Blu-ray ...
    if fname.startswith("bluray:"):
        # Find stream info ...
        # NOTE: Sometimes "ffprobe" appears to work fine but even with
        #       "-loglevel quiet" it sometimes outputs things like:
        #           disc.c:424: error opening file CERTIFICATE/id.bdmv
        #           disc.c:424: error opening file CERTIFICATE/BACKUP/id.bdmv
        #           bluray.c:255: 00008.m2ts: no timestamp for SPN 0 (got 0). clip 90000-7467995.
        #       ... to standard error, hence I have to only attempt to parse
        #       standard out as JSON rather than both standard error and
        #       standard out together.
        stdout = subprocess.check_output(
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
            stderr = open(os.devnull, "wt")
        )
    else:
        # Attempt to survey the file ...
        try:
            # Find stream info ...
            # NOTE: Sometimes "ffprobe" appears to work fine but even with
            #       "-loglevel quiet" it sometimes outputs things like:
            #           disc.c:424: error opening file CERTIFICATE/id.bdmv
            #           disc.c:424: error opening file CERTIFICATE/BACKUP/id.bdmv
            #           bluray.c:255: 00008.m2ts: no timestamp for SPN 0 (got 0). clip 90000-7467995.
            #       ... to standard error, hence I have to only attempt to parse
            #       standard out as JSON rather than both standard error and
            #       standard out together.
            stdout = subprocess.check_output(
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
                stderr = open(os.devnull, "wt")
            )
        except subprocess.CalledProcessError:
            # Fallback and attempt to find stream info as a raw M-JPEG stream ...
            # NOTE: Sometimes "ffprobe" appears to work fine but even with
            #       "-loglevel quiet" it sometimes outputs things like:
            #           disc.c:424: error opening file CERTIFICATE/id.bdmv
            #           disc.c:424: error opening file CERTIFICATE/BACKUP/id.bdmv
            #           bluray.c:255: 00008.m2ts: no timestamp for SPN 0 (got 0). clip 90000-7467995.
            #       ... to standard error, hence I have to only attempt to parse
            #       standard out as JSON rather than both standard error and
            #       standard out together.
            stdout = subprocess.check_output(
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
                stderr = open(os.devnull, "wt")
            )

    # Return ffprobe output as dictionary ...
    return json.loads(stdout)