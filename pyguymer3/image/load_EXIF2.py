#!/usr/bin/env python3

# Define function ...
def load_EXIF2(
    fName,
    /,
    *,
        cacheDir = "~/.cache/pyguymer3",
      compressed = False,
             cwd = None,
       ensureNFC = True,
    exiftoolPath = None,
         timeout = 60.0,
):
    """
    Run "exiftool" on a file and return the metadata.

    Parameters
    ----------
    fName : str
        the file to be surveyed
    cacheDir : str, optional
        if a string, then it is the path to the local cache of "exiftool" JSON
        output so as to save time in future calls
    compressed : bool, optional
        the file is compressed
    cwd : str, optional
        the child working directory
    ensureNFC : bool, optional
        ensure that the Unicode encoding is NFC
    exiftoolPath : str, optional
        the path to the "exiftool" binary (if not provided then Python will attempt
        to find the binary itself)
    timeout : float, optional
        the timeout for any requests/subprocess calls

    Returns
    -------
    ans : dict
        the metadata

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
    if exiftoolPath is None:
        exiftoolPath = shutil.which("exiftool")
    assert exiftoolPath is not None, "\"exiftool\" is not installed"

    # Convert path to absolute path ...
    fName = os.path.realpath(os.path.abspath(os.path.expanduser(fName)))

    # Check if the user wants to use a cache ...
    if isinstance(cacheDir, str):
        # Expand cache path ...
        cacheDir = os.path.expanduser(cacheDir)
        cacheDir = f"{cacheDir}/exiftool"

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

    # Create "exiftool" command ...
    cmd = [
        exiftoolPath,
        "-api", "largefilesupport=1",
        "-json",
    ]
    if compressed:
        cmd += [
            "-zip",
        ]
    cmd += [
        "-coordFormat", "%+.12f",
        "-dateFormat", "%Y-%m-%dT%H:%M:%S.%.6f",                                # This should be the same as datetime.isoformat(sep = "T", timespec = "microseconds").
        "-groupNames",
        "-struct",
        "--printConv",
        fName,
    ]

    # Find metadata ...
    # NOTE: Don't merge standard out and standard error together as the result
    #       will probably not be valid JSON if standard error is not empty.
    resp = subprocess.run(
        cmd,
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
                ans[0],
                gzObj,
                ensure_ascii = False,
                   sort_keys = True,
            )

    # Return the answer ...
    return ans[0]
