#!/usr/bin/env python3

# Define function ...
def parse_DNG_file(
    fName,
    /,
    *,
     debug = __debug__,
    errors = "strict",
    prefix = None,
    render = True,
):
    """Parse all the metadata in a DNG file and calculate some hashes of the
    image data streams.

    Parameters
    ----------
    fName : str
        The path to the DNG file.
    debug : bool, optional
        Print debug messages.
    errors : str, optional
        How to treat errors when decoding bytes to characters.
    prefix : None or str, optional
        If not None, then all JPEG data streams are saved individually with this
        path prefix.
    render : bool, optional
        Render some of the raw metadata as more useful data types or keys.

    Returns
    -------
    dirEntries : dict
        The processed parsed directory entries.

    Notes
    -----
    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import standard modules ...
    import os
    import struct

    # Import sub-functions ...
    from .DNG import parseDirEntries
    from .DNG import processDirEntries

    # **************************************************************************

    if debug:
        print(f"DEBUG: Parsing \"{fName}\" ...")

    # Open file ...
    with open(
        fName,
        mode = "rb",
    ) as fObj:
        # Match the magic marker ...
        match fObj.read(4):
            # Catch little endianness ...
            case b"\x49\x49\x2a\x00":
                endian = "<"

            # Catch big endianness ...
            case b"\x4d\x4d\x00\x2a":
                endian = ">"

            # Catch everything else ...
            case _:
                raise Exception(f"\"{fName}\" is not a DNG file") from None

        # **********************************************************************

        # Find the offset to the first directory, move to it and clean up ...
        ifd0offset, = struct.unpack(
            f"{endian}1I",
            fObj.read(4),
        )                                                                       # [B]
        fObj.seek(ifd0offset, os.SEEK_SET)
        del ifd0offset

        # **********************************************************************

        # Parse the first directory entries ...
        dirEntries = parseDirEntries(
            fObj,
            endian,
            debug = debug,
              gps = False,
        )

        # **********************************************************************

        # Find the offset to the next directory ...
        ifdNoffset, = struct.unpack(
            f"{endian}1I",
            fObj.read(4),
        )                                                                       # [B]

        # **********************************************************************

        # Process the first directory entries in to more useful data ...
        dirEntries = processDirEntries(
            dirEntries,
            fObj,
            endian,
             debug = debug,
            errors = errors,
            prefix = prefix,
            render = render,
        )

        # **********************************************************************

        # Check if there are sub-directories in the first directory entries ...
        if "SubIFDs" in dirEntries:
            # Catch the special case of there only being one sub-directory in
            # the first directory entries ...
            if isinstance(dirEntries["SubIFDs"], int):
                dirEntries["SubIFDs"] = [dirEntries["SubIFDs"]]

            # Initialize list ...
            subDirEntries = []

            # Loop over sub-directory offsets ...
            for subIfd0offset in dirEntries["SubIFDs"]:
                # Move to the offset ...
                fObj.seek(subIfd0offset, os.SEEK_SET)

                # **************************************************************

                # Parse the first sub-directory entries ...
                subDirEntry = parseDirEntries(
                    fObj,
                    endian,
                    debug = debug,
                      gps = False,
                )

                # **************************************************************

                # Find the offset to the next sub-directory ...
                subIfdNoffset, = struct.unpack(
                    f"{endian}1I",
                    fObj.read(4),
                )                                                               # [B]

                # **************************************************************

                # Process the first sub-directory entries in to more useful data ...
                subDirEntry = processDirEntries(
                    subDirEntry,
                    fObj,
                    endian,
                     debug = debug,
                    errors = errors,
                    prefix = prefix,
                    render = render,
                )

                # Populate the list and clean up ...
                subDirEntries.append(subDirEntry)
                del subDirEntry

                # **************************************************************

                # Check if there are any more sub-directories and clean up ...
                if subIfdNoffset != 0 and debug:
                    print(f"DEBUG: There is another sub-directory at {subIfdNoffset:,d} bytes, only the first one has been parsed.")
                del subIfdNoffset

            # Populate the value and clean up ...
            dirEntries["SubIFDs"] = subDirEntries
            del subDirEntries

        # **********************************************************************

        # Check if there are any more directories and clean up ...
        if ifdNoffset != 0 and debug:
            print(f"DEBUG: There is another directory at {ifdNoffset:,d} bytes, only the first one has been parsed.")
        del ifdNoffset

    # **************************************************************************

    # Return answer ...
    return dirEntries
