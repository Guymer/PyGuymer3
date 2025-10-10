#!/usr/bin/env python3

# Subclass dictionary ...
class exifreadCachedDict(dict):
    """
    This class is a subclass of the in-built `dict()` class but it provides a
    `__missing__` method to populate the dictionary from the (cached) output of
    the "exifread" module run on the dictionary key.

    Parameters
    ----------
    cacheDir : str, optional
        the path to the local cache of "exifread" JSON output so as to save time
        in future calls
    debug : bool, optional
        print debug messages

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

        # Import special modules ...
        try:
            import exifread
        except:
            raise Exception("\"exifread\" is not installed; run \"pip install --user ExifRead\"") from None

        # **********************************************************************

        # Check input ...
        assert isinstance(key, str), "dictionary key is not a str()"

        # **********************************************************************

        # Expand dictionary key to absolute path ...
        fName = os.path.realpath(os.path.abspath(os.path.expanduser(key)))

        # Expand cache directory to absolute path ...
        cacheDir = os.path.realpath(os.path.abspath(os.path.expanduser(self.cacheDir)))
        cacheDir = f"{cacheDir}/exifread"

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
            if self.debug:
                print(f"Loading \"{cacheFile}\" to populate self[\"{key}\"] ...")

            # Load the answer (from previous instances of Python) ...
            with gzip.open(cacheFile, encoding = "utf-8", mode = "rt") as gzObj:
                ans = json.load(gzObj)
        else:
            if self.debug:
                print(f"Running \"exifread\" module on \"{fName}\" ...")

            # Find metadata ...
            with open(fName, "rb") as fObj:
                ans = exifread.process_file(
                    fObj,
                        builtin_types = True,
                              details = False,
                    extract_thumbnail = False,
                )

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
__exifread__ = exifreadCachedDict()
