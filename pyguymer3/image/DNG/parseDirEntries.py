#!/usr/bin/env python3

# Define function ...
def parseDirEntries(
    fObj,
    endian,
    /,
    *,
    debug = __debug__,
      gps = False,
):
    """Parse the directory entries.

    Parameters
    ----------
    fObj : io.BufferedReader
        The open DNG file object.
    endian : str
        The endianness of the DNG file.
    debug : bool, optional
        Print debug messages.
    gps : bool, optional
        Is this directory GPS metadata?

    Returns
    -------
    dirEntries : dict
        The parsed directory entries.

    Notes
    -----
    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import standard modules ...
    import struct

    # Import sub-functions ...
    from .tagId2tagName import tagId2tagName
    from .typeId2typeName import typeId2typeName

    # **************************************************************************

    # Find the number of directory entries ...
    numDirEntries, = struct.unpack(
        f"{endian}1H",
        fObj.read(2),
    )                                                                           # [#]

    # **************************************************************************

    # Initialize dictionary ...
    dirEntries = {}

    # Loop over directory entries ...
    for _ in range(numDirEntries):
        # Initialize dictionary ...
        dirEntry = {}

        # **********************************************************************

        # Find the tag ID, match the tag ID with its name and clean up ...
        tagId, = struct.unpack(
            f"{endian}1H",
            fObj.read(2),
        )
        tagName = tagId2tagName(
            tagId,
            debug = debug,
              gps = gps,
        )
        del tagId

        # **********************************************************************

        # Find the type ID, match the type ID with its name and clean up ...
        typeId, = struct.unpack(
            f"{endian}1H",
            fObj.read(2),
        )
        dirEntry["typeName"] = typeId2typeName(
            typeId,
            debug = debug,
        )
        del typeId

        # **********************************************************************

        # Find the number of values ...
        dirEntry["count"], = struct.unpack(
            f"{endian}1I",
            fObj.read(4),
        )                                                                       # [#]

        # Populate the value or offset ...
        dirEntry["valueOrOffset"] = fObj.read(4)

        # **********************************************************************

        # If both the tag name and the type name are recognised then populate
        # the dictionary and clean up ...
        if tagName is not None and dirEntry["typeName"] is not None:
            dirEntries[tagName] = dirEntry
        del dirEntry, tagName

    # Clean up ...
    del numDirEntries

    # **************************************************************************

    # Return answer ...
    return dirEntries
