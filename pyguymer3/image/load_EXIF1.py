#!/usr/bin/env python3

# Define function ...
def load_EXIF1(
    fName,
    /,
    *,
    cacheDir = "~/.cache/pyguymer3",
       debug = __debug__,
):
    """
    Run "exifread" on a file and return the metadata.

    Parameters
    ----------
    fName : str
        the file to be surveyed
    cacheDir : str, optional
        if a string, then it is the path to the local cache of "exifread" JSON
        output so as to save time in future calls
    debug : bool, optional
        print debug messages

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
    from .__exifread__ import __exifread__

    # **************************************************************************

    # Configure global (subclassed) dictionary ...
    __exifread__.cacheDir = cacheDir
    __exifread__.debug = debug

    # **************************************************************************

    # Return the answer ...
    return __exifread__[fName]
