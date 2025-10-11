#!/usr/bin/env python3

# Subclass dictionary ...
class exiftoolCachedDict(dict):
    """
    This class is a subclass of the in-built `dict()` class but it provides a
    `__missing__` method to populate the dictionary from the (cached) output of
    the "exiftool" command run on the dictionary key.

    Parameters
    ----------
    cacheDir : str, optional
        the path to the local cache of "exiftool" JSON output so as to save time
        in future calls
    compressed : bool, optional
        the file is compressed
    debug : bool, optional
        print debug messages
    ensureNFC : bool, optional
        ensure that the Unicode encoding is NFC
    exiftoolPath : str, optional
        the path to the "exiftool" binary (if not provided then Python will
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
    compressed = False
    debug = __debug__
    ensureNFC = True
    exiftoolPath = None
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

        # Try to find the path to "exiftool" if the user did not provide it ...
        if self.exiftoolPath is None:
            self.exiftoolPath = shutil.which("exiftool")
        assert self.exiftoolPath is not None, "\"exiftool\" is not installed"

        # **********************************************************************

        # Expand dictionary key to absolute path ...
        fName = os.path.realpath(os.path.abspath(os.path.expanduser(key)))

        # Expand cache directory to absolute path ...
        cacheDir = os.path.realpath(os.path.abspath(os.path.expanduser(self.cacheDir)))
        cacheDir = f"{cacheDir}/exiftool"

        # Deduce sidecar file name ...
        cacheFile = f"{hashlib.sha256(fName.encode(), usedforsecurity = False).hexdigest()}.json.gz"

        # Place sidecar file in some sub-directories to avoid directories having
        # too many members ...
        cacheDir = f"{cacheDir}/{cacheFile[0]}/{cacheFile[1]}"
        cacheFile = f"{cacheDir}/{cacheFile[2:]}"

        # Make cache directory if it is missing ...
        if not os.path.exists(cacheDir):
            os.makedirs(cacheDir)

        # Check if the sidecar file exists ...
        if os.path.exists(cacheFile):
            # Check if the file is newer than the sidecar file ...
            if os.path.getmtime(fName) > os.path.getmtime(cacheFile):
                # Remove sidecar file ...
                os.remove(cacheFile)

        # Check if the sidecar file exists ...
        if os.path.exists(cacheFile):
            if self.debug:
                print(f"Loading \"{cacheFile}\" to populate self[\"{key}\"] ...")

            # Load the answer (from previous instances of Python) ...
            with gzip.open(cacheFile, encoding = "utf-8", mode = "rt") as gzObj:
                ans = json.load(gzObj)
        else:
            if self.debug:
                print(f"Running \"{self.exiftoolPath}\" on \"{fName}\" ...")

            # Create "exiftool" command ...
            cmd = [
                self.exiftoolPath,
                "-api", "largefilesupport=1",
                "-json",
            ]
            if self.compressed:
                cmd += [
                    "-zip",
                ]
            cmd += [
                "-coordFormat", "%+.12f",
                "-dateFormat", "%Y-%m-%dT%H:%M:%S.%.6f",                        # This should be the same as datetime.isoformat(sep = "T", timespec = "microseconds").
                "-groupNames",
                "-struct",
                "--printConv",
                fName,
            ]

            # Find metadata ...
            # NOTE: Don't merge standard out and standard error together as the
            #       result will probably not be valid JSON if standard error is
            #       not empty.
            resp = subprocess.run(
                cmd,
                   check = True,
                encoding = "utf-8",
                  stderr = subprocess.DEVNULL,
                  stdout = subprocess.PIPE,
                 timeout = self.timeout,
            )

            # Parse the JSON ...
            if self.ensureNFC and not unicodedata.is_normalized("NFC", resp.stdout):
                ans = json.loads(unicodedata.normalize("NFC", resp.stdout))[0]
            else:
                ans = json.loads(resp.stdout)[0]

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
__exiftool__ = exiftoolCachedDict()
