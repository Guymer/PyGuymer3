#!/usr/bin/env python3

# Subclass dictionary ...
class ffprobeCachedDict(dict):
    """
    This class is a subclass of the in-built `dict()` class but it provides a
    `__missing__` method to populate the dictionary from the (cached) output of
    the "ffprobe" command run on the dictionary key.

    Parameters
    ----------
    cacheDir : str, optional
        the path to the local cache of "ffprobe" JSON output so as to save time
        in future calls
    debug : bool, optional
        print debug messages
    ensureNFC : bool, optional
        ensure that the Unicode encoding is NFC
    ffprobePath : str, optional
        the path to the "ffprobe" binary (if not provided then Python will
        attempt to find the binary itself)
    timeout : float, optional
        the timeout for any requests/subprocess calls

    Notes
    -----
    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Set default settings ...
    cacheDir = "~/.cache/pyguymer3"
    debug = __debug__
    ensureNFC = True
    ffprobePath = None
    timeout = 60.0                                                              # [s]

    # Define what to do if a key is missing in the dictionary ...
    def __missing__(self, key):
        # Import standard modules ...
        import gzip
        import hashlib
        import json
        import os
        import shutil
        import subprocess
        import unicodedata

        # **********************************************************************

        # Check input ...
        assert isinstance(key, str), "dictionary key is not a str()"

        # Try to find the path to "ffprobe" if the user did not provide it ...
        if self.ffprobePath is None:
            self.ffprobePath = shutil.which("ffprobe")
        assert self.ffprobePath is not None, "\"ffprobe\" is not installed"

        # **********************************************************************

        # Extract file name and playlist ...
        # NOTE: There are two formats for the keys in this dictionary:
        #         * Blu-rays are like "bluray:/path/to/bluray:123" where "123"
        #           is the playlist within the Blu-ray; and
        #         * files are like "/path/to/file.ext:-1".
        if key.startswith("bluray:"):
            assert key.count(":") == 2, f"Blu-ray key \"{key}\" does not contain exactly 2 \":\" characters"
            fName = key.split(":")[1]
            playlist = int(key.split(":")[2])
            assert playlist >= 0, f"Blu-ray key \"{key}\" does not have a playlist specified"
        else:
            assert key.count(":") == 1, f"file key \"{key}\" does not contain exactly 1 \":\" character"
            fName = key.split(":")[0]
            playlist = int(key.split(":")[1])
            assert playlist == -1, f"file key \"{key}\" has a playlist specified"

        # Expand file name to absolute path ...
        fName = os.path.realpath(os.path.abspath(os.path.expanduser(fName)))

        # Expand cache directory to absolute path ...
        cacheDir = os.path.realpath(os.path.abspath(os.path.expanduser(self.cacheDir)))
        cacheDir = f"{cacheDir}/ffprobe"
        if key.startswith("bluray:"):
            cacheDir = f"{cacheDir}/playlist={playlist:d}"
        else:
            cacheDir = f"{cacheDir}/playlist=none"

        # Deduce sidecar file name ...
        cacheFile = f'{hashlib.sha256(fName.encode(), usedforsecurity = False).hexdigest()}.json.gz'

        # Place sidecar file in some sub-directories to avoid directories having
        # too many members ...
        cacheDir = f"{cacheDir}/{cacheFile[0]}/{cacheFile[1]}"
        cacheFile = f"{cacheDir}/{cacheFile[2:]}"

        # Make cache directory if it is missing ...
        if not os.path.exists(cacheDir):
            os.makedirs(cacheDir)

        # Check if the sidecar file exists ...
        if os.path.exists(cacheFile):
            if self.debug:
                print(f"Loading \"{cacheFile}\" to populate self[\"{key}\"] ...")

            # Load the answer (from previous instances of Python) ...
            with gzip.open(cacheFile, encoding = "utf-8", mode = "rt") as gzObj:
                ans = json.load(gzObj)
        else:
            if self.debug:
                print(f"Running \"{self.ffprobePath}\" on \"{fName}\" ...")

            # Check if it is a Blu-ray ...
            if key.startswith("bluray:"):
                # Find stream info ...
                # NOTE: Sometimes "ffprobe" appears to work fine but even with
                #       "-loglevel quiet" it sometimes outputs things like:
                #           disc.c:424: error opening file CERTIFICATE/id.bdmv
                #           disc.c:424: error opening file CERTIFICATE/BACKUP/id.bdmv
                #           bluray.c:255: 00008.m2ts: no timestamp for SPN 0 (got 0). clip 90000-7467995.
                #       ... to standard error, hence I have to only attempt to
                #       parse standard out as JSON rather than both standard
                #       error and standard out together.
                resp = subprocess.run(
                    [
                        self.ffprobePath,
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
                    encoding = "utf-8",
                      stderr = subprocess.DEVNULL,
                      stdout = subprocess.PIPE,
                     timeout = self.timeout,
                )
            else:
                # Attempt to survey the file ...
                try:
                    # Find stream info ...
                    # NOTE: Sometimes "ffprobe" appears to work fine but even
                    #       with "-loglevel quiet" it sometimes outputs things
                    #       like:
                    #           disc.c:424: error opening file CERTIFICATE/id.bdmv
                    #           disc.c:424: error opening file CERTIFICATE/BACKUP/id.bdmv
                    #           bluray.c:255: 00008.m2ts: no timestamp for SPN 0 (got 0). clip 90000-7467995.
                    #       ... to standard error, hence I have to only attempt
                    #       to parse standard out as JSON rather than both
                    #       standard error and standard out together.
                    resp = subprocess.run(
                        [
                            self.ffprobePath,
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
                        encoding = "utf-8",
                          stderr = subprocess.DEVNULL,
                          stdout = subprocess.PIPE,
                         timeout = self.timeout,
                    )
                except subprocess.CalledProcessError:
                    # Fallback and attempt to find stream info as a raw M-JPEG
                    # stream ...
                    # NOTE: Sometimes "ffprobe" appears to work fine but even
                    #       with "-loglevel quiet" it sometimes outputs things
                    #       like:
                    #           disc.c:424: error opening file CERTIFICATE/id.bdmv
                    #           disc.c:424: error opening file CERTIFICATE/BACKUP/id.bdmv
                    #           bluray.c:255: 00008.m2ts: no timestamp for SPN 0 (got 0). clip 90000-7467995.
                    #       ... to standard error, hence I have to only attempt
                    #       to parse standard out as JSON rather than both
                    #       standard error and standard out together.
                    resp = subprocess.run(
                        [
                            self.ffprobePath,
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
                        encoding = "utf-8",
                          stderr = subprocess.DEVNULL,
                          stdout = subprocess.PIPE,
                         timeout = self.timeout,
                    )

            # Parse the JSON ...
            if self.ensureNFC and not unicodedata.is_normalized("NFC", resp.stdout):
                ans = json.loads(unicodedata.normalize("NFC", resp.stdout))
            else:
                ans = json.loads(resp.stdout)

            if self.debug:
                print(f"Saving \"{cacheFile}\" to populate self[\"{key}\"] in future ...")

            # Save the answer (for future instances of Python) ...
            with gzip.open(cacheFile, compresslevel = 9, encoding = "utf-8", mode = "wt") as gzObj:
                json.dump(
                    ans,
                    gzObj,
                    ensure_ascii = False,
                       sort_keys = True,
                )

        # Populate the dictionary (for future calls in this instance of Python) ...
        self[key] = ans

        # Return the answer ...
        return ans

# Initialize global (subclassed) dictionary ...
__ffprobe__ = ffprobeCachedDict()
