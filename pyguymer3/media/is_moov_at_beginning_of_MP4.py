#!/usr/bin/env python3

# Define function ...
def is_moov_at_beginning_of_MP4(
    fname,
    /,
):
    # NOTE: The following websites have some very useful information on how to
    #       parse MP4 files - the first just forgot to say that integers are
    #       big-endian.
    #         * http://atomicparsley.sourceforge.net/mpeg-4files.html
    #         * https://developer.apple.com/library/archive/documentation/QuickTime/QTFF/QTFFChap2/qtff2.html
    #         * https://wiki.multimedia.cx/index.php/QuickTime_container

    # Import standard modules ...
    import os
    import re
    import struct

    # Open MP4 read-only ...
    with open(fname, "rb") as fObj:
        # Create short-hand ...
        fsize = os.path.getsize(fname)                                          # [B]

        # Set triggers ...
        foundFTYP = False
        foundMDAT = False
        foundMOOV = False

        # Loop over entire contents of MP4 ...
        while fObj.tell() < fsize:
            # Attempt to read 4 bytes as a big-endian un-signed 32-bit integer ...
            val, = struct.unpack(">I", fObj.read(4))                            # [B]
            off = 4                                                             # [B]

            # Extract atom name ...
            name = fObj.read(4).decode("utf-8")
            off += 4                                                            # [B]

            # Check that it matches the pattern ...
            if re.match(r"[a-z][a-z][a-z][a-z]", name) is None:
                raise Exception(f"\"{name}\" is not an atom name in \"{fname}\"") from None

            # Check that it is a MP4 file ...
            if not foundFTYP and name != "ftyp":
                raise Exception(f"\"{fname}\" is not a MP4") from None

            # Set trigger ...
            foundFTYP = True

            # Check if it is the MDAT atom ...
            if name == "mdat":
                foundMDAT = True

            # Check if it is the MOOV atom ...
            if name == "moov":
                foundMOOV = True
                if foundMDAT:
                    return False
                return True

            # Check the length ...
            if val == 0:
                # NOTE: This atom runs until EOF.

                # Stop looping ...
                break

            # Check the length ...
            if val == 1:
                # NOTE: This atom has 64-bit sizes.

                # Attempt to read 8 bytes as a big-endian un-signed 64-bit
                # integer ...
                val, = struct.unpack(">Q", fObj.read(8))                        # [B]
                off += 8                                                        # [B]

            # Skip to the end of the atom ...
            fObj.seek(val - off, os.SEEK_CUR)

    # Catch possible errors ...
    if not foundMDAT:
        raise Exception(f"did not find mdat atom in \"{fname}\"") from None
    if not foundMOOV:
        raise Exception(f"did not find moov atom in \"{fname}\"") from None

    # Return answer ...
    # NOTE: This cannot happen, but PyLint can't figure that out, so I have
    #       added a return statement to shut PyLint up.
    return False
