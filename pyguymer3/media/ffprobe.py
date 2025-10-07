#!/usr/bin/env python3

# Define function ...
def ffprobe(
    fName,
    /,
    *,
       cacheDir = "~/.cache/pyguymer3",
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
    fName : str
        the file to be surveyed
    cache : str, optional
        if a string, then it is the path to the local cache of "ffprobe" JSON
        output so as to save time in future calls
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
    import gzip
    import hashlib
    import json
    import os
    import shutil
    import subprocess
    import unicodedata

    # **************************************************************************

    # Try to find the paths if the user did not provide them ...
    if ffprobePath is None:
        ffprobePath = shutil.which("ffprobe")
    assert ffprobePath is not None, "\"ffprobe\" is not installed"

    # Check input ...
    if fName.startswith("bluray:") and playlist < 0:
        raise Exception("a Blu-ray was specified but no playlist was supplied") from None
    if not fName.startswith("bluray:") and playlist >= 0:
        raise Exception("a Blu-ray was not specified but a playlist was supplied") from None

    # Convert path to absolute path ...
    if playlist >= 0:
        fName = fName.removeprefix("bluray:")
    fName = os.path.abspath(os.path.expanduser(fName))
    if playlist >= 0:
        fName = f"bluray:{fName}"

    # Check if the user wants to use a cache ...
    if isinstance(cacheDir, str):
        # Expand cache path ...
        cacheDir = os.path.expanduser(cacheDir)
        cacheDir = f"{cacheDir}/ffprobe"
        if playlist >= 0:
            cacheDir = f"{cacheDir}/playlist={playlist:d}"
        else:
            cacheDir = f"{cacheDir}/playlist=none"

        # Deduce sidecar file name ...
        cacheFile = f"{hashlib.sha256(fName.encode(), usedforsecurity = False).hexdigest()}.json.gz"

        # Place sidecar file in some sub-directories to avoid directories having
        # too many members ...
        cacheDir = f"{cacheDir}/{cacheFile[0]}/{cacheFile[1]}"
        cacheFile = f"{cacheDir}/{cacheFile[2:]}"

        # Make directory if it is missing ...
        if not os.path.exists(cacheDir):
            os.makedirs(cacheDir)

        # Return the answer if the sidecar file exists ...
        if os.path.exists(cacheFile):
            with gzip.open(cacheFile, encoding = "utf-8", mode = "rt") as gzObj:
                return json.load(gzObj)

    # **************************************************************************

    # Check if it is a Blu-ray ...
    if fName.startswith("bluray:"):
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
                fName,
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
                    fName,
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
                    fName,
                ],
                   check = True,
                     cwd = cwd,
                encoding = "utf-8",
                  stderr = subprocess.DEVNULL,
                  stdout = subprocess.PIPE,
                 timeout = timeout,
            )

    # Parse the JSON ...
    if ensureNFC and not unicodedata.is_normalized("NFC", resp.stdout):
        ans = json.loads(unicodedata.normalize("NFC", resp.stdout))
    else:
        ans = json.loads(resp.stdout)

    # Check if the user wants to use a cache ...
    if isinstance(cacheDir, str):
        # Cache the answer ...
        with gzip.open(cacheFile, compresslevel = 9, encoding = "utf-8", mode = "wt") as gzObj:
            json.dump(
                ans,
                gzObj,
                ensure_ascii = False,
                   sort_keys = True,
            )

    # Return the answer ...
    return ans
