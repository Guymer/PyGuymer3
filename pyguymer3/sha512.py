#!/usr/bin/env python3

# Define function ...
def sha512(
    fname,
    /,
    *,
    chunksize = 1048576,
):
    """Find the SHA-512 hash of a file

    This function returns the SHA-512 hash of the passed file.

    Parameters
    ----------
    fname : str
        the input file name
    chunksize : int, optional
        the size of the chunks of any files which are read in (in bytes)

    Returns
    -------
    hexdigest : str
        The hash hexdigest of the input file.

    Notes
    -----
    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import standard modules ...
    import hashlib

    # Create hash object ...
    hobj = hashlib.sha512()

    # Open input file as bytes ...
    with open(fname, "rb") as fObj:
        # Start infinite loop ...
        while True:
            # Read a chunk ...
            chunk = fObj.read(chunksize)

            # Stop looping if this chunk is empty ...
            if len(chunk) == 0:
                break

            # Update hash object with chunk ...
            hobj.update(chunk)

    # Return answer ...
    return hobj.hexdigest()
