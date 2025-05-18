#!/usr/bin/env python3

# Define function ...
def ffprobe(
    fname,
    /,
    *,
            cwd = None,
      ensureNFC = True,
    ffprobePath = None,
       playlist = -1,
        timeout = 60.0,
):
    """
    Run "ffprobe" on a file and return the format and stream information.

    Parameters
    ----------
    fname : str
        the file to be surveyed
    cwd : str, optional
        the child working directory
    ensureNFC : bool, optional
        ensure that the Unicode encoding is NFC
    ffprobePath : str, optional
        the path to the "ffprobe" binary (if not provided then Python will
        attempt to find the binary itself)
    playlist : int, optional
        the playlist within the Blu-ray folder structure to be surveyed
    timeout : float, optional
        the timeout for any requests/subprocess calls

    Returns
    -------
    ans : dict
        the format and stream information

    Notes
    -----
    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import standard modules ...
    import json
    import shutil
    import subprocess
    import unicodedata

    # **************************************************************************

    # Try to find the paths if the user did not provide them ...
    if ffprobePath is None:
        ffprobePath = shutil.which("ffprobe")
    assert ffprobePath is not None, "\"ffprobe\" is not installed"

    # Check input ...
    if fname.startswith("bluray:") and playlist < 0:
        raise Exception("a Blu-ray was specified but no playlist was supplied") from None

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
        resp = subprocess.run(
            [
                ffprobePath,
                "-hide_banner",
                "-loglevel", "quiet",
                "-probesize", "1G",
                "-analyzeduration", "1800M",
                "-print_format", "json",
                "-show_format",
                "-show_streams",
                "-playlist", f"{playlist:d}",
                fname,
            ],
               check = True,
                 cwd = cwd,
            encoding = "utf-8",
              stderr = subprocess.DEVNULL,
              stdout = subprocess.PIPE,
             timeout = timeout,
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
            resp = subprocess.run(
                [
                    ffprobePath,
                    "-hide_banner",
                    "-loglevel", "quiet",
                    "-probesize", "1G",
                    "-analyzeduration", "1800M",
                    "-print_format", "json",
                    "-show_format",
                    "-show_streams",
                    fname,
                ],
                   check = True,
                     cwd = cwd,
                encoding = "utf-8",
                  stderr = subprocess.DEVNULL,
                  stdout = subprocess.PIPE,
                 timeout = timeout,
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
            resp = subprocess.run(
                [
                    ffprobePath,
                    "-hide_banner",
                    "-loglevel", "quiet",
                    "-probesize", "1G",
                    "-analyzeduration", "1800M",
                    "-print_format", "json",
                    "-show_format",
                    "-show_streams",
                    "-f", "mjpeg",
                    fname,
                ],
                   check = True,
                     cwd = cwd,
                encoding = "utf-8",
                  stderr = subprocess.DEVNULL,
                  stdout = subprocess.PIPE,
                 timeout = timeout,
            )

    # Return ffprobe output as dictionary ...
    if ensureNFC and not unicodedata.is_normalized("NFC", resp.stdout):
        return json.loads(unicodedata.normalize("NFC", resp.stdout))
    return json.loads(resp.stdout)
