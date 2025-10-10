#!/usr/bin/env python3

# Define function ...
def load_EXIF2(
    fName,
    /,
    *,
        cacheDir = "~/.cache/pyguymer3",
      compressed = False,
           debug = __debug__,
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
    debug : bool, optional
        print debug messages
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

    # Import global (subclassed) dictionary ...
    from .__exiftool__ import __exiftool__

    # **************************************************************************

    # Configure global (subclassed) dictionary ...
    # NOTE: If I blindly set "__exiftool__.exiftoolPath" to "exiftoolPath" each
    #       time then I would clobber any previous calls to "shutil.which()"
    #       performed by the global (subclassed) dictionary itself.
    __exiftool__.cacheDir = cacheDir
    __exiftool__.compressed = compressed
    __exiftool__.debug = debug
    __exiftool__.ensureNFC = ensureNFC
    if exiftoolPath is not None:
        __exiftool__.exiftoolPath = exiftoolPath
    __exiftool__.timeout = timeout                                              # [s]

    # **************************************************************************

    # Return the answer ...
    return __exiftool__[fName]
