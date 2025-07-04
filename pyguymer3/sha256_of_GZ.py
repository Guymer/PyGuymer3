#!/usr/bin/env python3

# Define function ...
def sha256_of_GZ(
    fname,
    /,
    *,
                 chunksize = 1048576,
    ignoreModificationTime = True,
):
    """Find the SHA-256 hash of a GZ file

    This function returns the SHA-256 hash of the passed GZ file as if the first
    "Modification Time" field is set to zero. Using this function it is possible
    to discover that the only binary difference between two different GZ files
    is the first "Modification Time" field.

    If this function is told not to ignore the first "Modification Time" field
    then this function will return the SHA-256 identically to any other method.

    Parameters
    ----------
    fname : str
        the input GZ file name
    chunksize : int, optional
        the size of the chunks of any files which are read in (in bytes)
    ignoreModificationTime : bool, optional
        ignore the first "Modification Time" field

    Returns
    -------
    hexdigest : str
        The hash hexdigest of the input GZ file.

    Notes
    -----
    The following websites have some very useful information on how to parse GZ
    files:

    - https://en.wikipedia.org/wiki/Gzip#File_format
    - https://tools.ietf.org/html/rfc1952.html#page-5

    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import standard modules ...
    import hashlib
    import struct

    # **************************************************************************

    # Construct a hash object ...
    hObj = hashlib.sha256()

    # Open input GZ file read-only ...
    with open(fname, "rb") as fObj:
        # Attempt to read 2 bytes and pass them to the hash object ...
        src = fObj.read(2)
        hObj.update(src)

        # Check that this is a GZ file ...
        if src != b"\x1f\x8b":
            raise Exception(f"\"{fname}\" is not a GZ") from None

        # Pass 2 bytes to the hash object ...
        hObj.update(fObj.read(2))

        # Check what the user wants to do ...
        if ignoreModificationTime:
            # Pass 0 as a little-endian un-signed 32-bit integer to the hash
            # object ...
            fObj.read(4)
            hObj.update(struct.pack(">I", 0))
        else:
            # Pass 4 bytes to the hash object ...
            hObj.update(fObj.read(4))

        # Pass the rest of the file to the hash object using chunks ...
        while True:
            chunk = fObj.read(chunksize)
            if len(chunk) == 0:
                break
            hObj.update(chunk)

    # Return hash hexdigest ...
    return hObj.hexdigest()
