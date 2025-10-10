#!/usr/bin/env python3

# Subclass dictionary ...
class lsdvdCachedDict(dict):
    """
    This class is a subclass of the in-built `dict()` class but it provides a
    `__missing__` method to populate the dictionary from the (cached) output of
    the "lsdvd" command run on the dictionary key.

    Parameters
    ----------
    cacheDir : str, optional
        the path to the local cache of "lsdvd" JSON output so as to save time in
        future calls
    debug : bool, optional
        print debug messages
    ensureNFC : bool, optional
        ensure that the Unicode encoding is NFC
    lsdvdPath : str, optional
        the path to the "lsdvd" binary (if not provided then Python will attempt
        to find the binary itself)
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
    lsdvdPath = None
    timeout = 60.0                                                              # [s]

    # Define what to do if a key is missing in the dictionary ...
    def __missing__(self, key):
        # Import standard modules ...
        import gzip
        import hashlib
        import html
        import json
        import os
        import shutil
        import subprocess
        import unicodedata

        # Import special modules ...
        try:
            import lxml
            import lxml.etree
        except:
            raise Exception("\"lxml\" is not installed; run \"pip install --user lxml\"") from None

        # Import sub-functions ...
        from ..elem2dict import elem2dict

        # **********************************************************************

        # Check input ...
        assert isinstance(key, str), "dictionary key is not a str()"

        # Try to find the path to "lsdvd" if the user did not provide it ...
        if self.lsdvdPath is None:
            self.lsdvdPath = shutil.which("lsdvd")
        assert self.lsdvdPath is not None, "\"lsdvd\" is not installed"

        # **********************************************************************

        # Expand dictionary key to absolute path ...
        fName = os.path.realpath(os.path.abspath(os.path.expanduser(key)))

        # Expand cache directory to absolute path ...
        cacheDir = os.path.realpath(os.path.abspath(os.path.expanduser(self.cacheDir)))
        cacheDir = f"{cacheDir}/lsdvd"

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

            # Load the answer (from either previous instances of Python or
            # previous calls in this instance of Python) ...
            with gzip.open(cacheFile, encoding = "utf-8", mode = "rt") as gzObj:
                ans = json.load(gzObj)
        else:
            if self.debug:
                print(f"Running \"{self.lsdvdPath}\" on \"{fName}\" ...")

            # Find track info ...
            # NOTE: "lsdvd" specifies the output encoding in the accompanying
            #       XML header, however, this is a lie. By inspection of
            #       "oxml.c" in the "lsdvd" source code it appears that the XML
            #       header is hard-coded and that "lsdvd" does not perform any
            #       checks to make sure that the output is either valid XML or
            #       valid UTF-8. Therefore, I must load it as a byte sequence
            #       and manually convert it to a UTF-8 string whilst replacing
            #       the invalid UTF-8 bytes (and remove the XML header).
            # NOTE: Don't merge standard out and standard error together as the
            #       result will probably not be valid XML if standard error is
            #       not empty.
            resp = subprocess.run(
                [
                    self.lsdvdPath,
                    "-x",
                    "-Ox",
                    fName,
                ],
                   check = True,
                encoding = "utf-8",
                  errors = "replace",
                  stderr = subprocess.DEVNULL,
                  stdout = subprocess.PIPE,
                 timeout = self.timeout,
            )
            stdout = ""
            for line in resp.stdout.splitlines():
                if line.startswith("libdvdread:"):
                    continue
                stdout += f"{line}\n"
            stdout = stdout.removeprefix("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")

            # Fix the file name itself ...
            stdout = stdout.replace(f"<device>{fName}</device>", f"<device>{html.escape(fName)}</device>")

            # Fix common errors ...
            stdout = stdout.replace("<df>Pan&Scan</df>", "<df>Pan&amp;Scan</df>")
            stdout = stdout.replace("<df>P&S + Letter</df>", "<df>P&amp;S + Letter</df>")

            # Parse the XML ...
            if self.ensureNFC and not unicodedata.is_normalized("NFC", stdout):
                ans = lxml.etree.XML(unicodedata.normalize("NFC", stdout))
            else:
                ans = lxml.etree.XML(stdout)

            # Convert the XML tree to a simplified dictionary ...
            ans = elem2dict(
                ans,
                   debug = False,
                simplify = True,
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
__lsdvd__ = lsdvdCachedDict()
