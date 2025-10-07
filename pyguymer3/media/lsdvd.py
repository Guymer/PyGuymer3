#!/usr/bin/env python3

# Define function ...
def lsdvd(
    fName,
    /,
    *,
     cacheDir = "~/.cache/pyguymer3",
          cwd = None,
        debug = __debug__,
    ensureNFC = True,
    lsdvdPath = None,
      timeout = 60.0,
):
    """
    Run "lsdvd" on a file and return the format and stream information.

    Parameters
    ----------
    fName : str
        the file to be surveyed
    cacheDir : str, optional
        if a string, then it is the path to the local cache of "lsdvd" JSON
        output so as to save time in future calls
    cwd : str, optional
        the child working directory
    debug : bool, optional
        print debug messages
    ensureNFC : bool, optional
        ensure that the Unicode encoding is NFC
    lsdvdPath : str, optional
        the path to the "lsdvd" binary (if not provided then Python will attempt
        to find the binary itself)
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

    # **************************************************************************

    # Try to find the paths if the user did not provide them ...
    if lsdvdPath is None:
        lsdvdPath = shutil.which("lsdvd")
    assert lsdvdPath is not None, "\"lsdvd\" is not installed"

    # Convert path to absolute path ...
    fName = os.path.abspath(os.path.expanduser(fName))

    # Check if the user wants to use a cache ...
    if isinstance(cacheDir, str):
        # Expand cache path ...
        cacheDir = os.path.expanduser(cacheDir)
        cacheDir = f"{cacheDir}/lsdvd"

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

    # Find track info ...
    # NOTE: "lsdvd" specifies the output encoding in the accompanying XML
    #       header, however, this is a lie. By inspection of "oxml.c" in the
    #       "lsdvd" source code it appears that the XML header is hard-coded and
    #       that "lsdvd" does not perform any checks to make sure that the
    #       output is either valid XML or valid UTF-8. Therefore, I must load it
    #       as a byte sequence and manually convert it to a UTF-8 string whilst
    #       replacing the invalid UTF-8 bytes (and remove the XML header).
    # NOTE: Don't merge standard out and standard error together as the result
    #       will probably not be valid XML if standard error is not empty.
    resp = subprocess.run(
        [
            lsdvdPath,
            "-x",
            "-Ox",
            fName,
        ],
           check = True,
             cwd = cwd,
        encoding = "utf-8",
          errors = "replace",
          stderr = subprocess.DEVNULL,
          stdout = subprocess.PIPE,
         timeout = timeout,
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
    if ensureNFC and not unicodedata.is_normalized("NFC", stdout):
        ans = lxml.etree.XML(unicodedata.normalize("NFC", stdout))
    else:
        ans = lxml.etree.XML(stdout)

    # Convert the XML tree to a simplified dictionary ...
    ans = elem2dict(
        ans,
           debug = debug,
        simplify = True,
    )

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
