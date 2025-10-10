#!/usr/bin/env python3

# Subclass dictionary ...
class pillowCachedDict(dict):
    """
    This class is a subclass of the in-built `dict()` class but it provides a
    `__missing__` method to populate the dictionary from the (cached) output of
    the "pillow" module run on the dictionary key.

    Parameters
    ----------
    cacheDir : str, optional
        the path to the local cache of "pillow" JSON output so as to save time
        in future calls
    compressed : bool, optional
        the file is compressed
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
    compressed = False
    debug = __debug__

    # Define what to do if a key is missing in the dictionary ...
    def __missing__(self, key):
        # Import standard modules ...
        import gzip
        import hashlib
        import json
        import os

        # Import special modules ...
        try:
            import PIL
            import PIL.Image
            PIL.Image.MAX_IMAGE_PIXELS = 1024 * 1024 * 1024                     # [px]
        except:
            raise Exception("\"PIL\" is not installed; run \"pip install --user Pillow\"") from None

        # **********************************************************************

        # Check input ...
        assert isinstance(key, str), "dictionary key is not a str()"

        # **********************************************************************

        # Expand dictionary key to absolute path ...
        fName = os.path.realpath(os.path.abspath(os.path.expanduser(key)))

        # Expand cache directory to absolute path ...
        cacheDir = os.path.realpath(os.path.abspath(os.path.expanduser(self.cacheDir)))
        cacheDir = f"{cacheDir}/pillow"

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
                print(f"Running \"pillow\" module on \"{fName}\" ...")

            # Initialize answer ...
            ans = {}

            # Check if the file is compressed ...
            if self.compressed:
                # Open compressed file ...
                with gzip.open(fName, mode = "rb") as gzObj:
                    # Open image ...
                    with PIL.Image.open(gzObj) as iObj:
                        # Loop over attributes ...
                        for attr in dir(iObj):
                            # Skip attributes which are private ...
                            if attr.startswith("_"):
                                continue

                            # Try to create short-hand ...
                            try:
                                value = getattr(iObj, attr)
                            except:
                                continue

                            # Skip attributes which are callable ...
                            if callable(value):
                                continue

                            # Populate answer ...
                            if hasattr(value, "named"):
                                ans[attr] = value.named()
                            else:
                                ans[attr] = value
            else:
                # Open image ...
                with PIL.Image.open(fName) as iObj:
                    # Loop over attributes ...
                    for attr in dir(iObj):
                        # Skip attributes which are private ...
                        if attr.startswith("_"):
                            continue

                        # Try to create short-hand ...
                        try:
                            value = getattr(iObj, attr)
                        except:
                            continue

                        # Skip attributes which are callable ...
                        if callable(value):
                            continue

                        # Populate answer ...
                        if hasattr(value, "named"):
                            ans[attr] = value.named()
                        else:
                            ans[attr] = value

            if self.debug:
                print(f"Saving \"{cacheFile}\" to populate self[\"{key}\"] in future ...")

            # Save the answer (for future instances of Python) ...
            with gzip.open(cacheFile, compresslevel = 9, encoding = "utf-8", mode = "wt") as gzObj:
                json.dump(
                    ans,
                    gzObj,
                         default = lambda x: f"un-serializable object ({repr(type(x))})",
                    ensure_ascii = False,
                       sort_keys = True,
                )

        # Populate the dictionary (for future calls in this instance of Python) ...
        self[key] = ans

        # Return the answer ...
        return ans

# Initialize global (subclassed) dictionary ...
__pillow__ = pillowCachedDict()
