def is_moov_at_beginning_of_MP4(fname):
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
    with open(fname, "rb") as fobj:
        # Create short-hand ...
        fsize = os.path.getsize(fname)

        # Set triggers ...
        foundFTYP = False
        foundMDAT = False
        foundMOOV = False

        # Loop over entire contents of MP4 ...
        while fobj.tell() < fsize:
            # Attempt to read 4 bytes as a big-endian un-signed 32-bit integer ...
            val, = struct.unpack(">I", fobj.read(4))                            # [B]
            off = 4

            # Extract atom name ...
            name = fobj.read(4).decode("utf-8")
            off += 4

            # Check that it matches the pattern ...
            if re.match(r"[a-z][a-z][a-z][a-z]", name) is None:
                raise Exception("\"{0:s}\" is not an atom name in \"{1:s}\"".format(name, fname))

            # Check that it is a MP4 file ...
            if not foundFTYP and name != "ftyp":
                raise Exception("\"{0:s}\" is not a MP4".format(fname))
            else:
                foundFTYP = True

            # Check if it is the MDAT atom ...
            if name == "mdat":
                foundMDAT = True

            # Check if it is the MOOV atom ...
            if name == "moov":
                foundMOOV = True
                if foundMDAT:
                    return False
                else:
                    return True

            # Check the length ...
            if val == 0:
                # NOTE: This atom runs until EOF.

                # Stop looping ...
                break
            elif val == 1:
                # NOTE: This atom has 64-bit sizes.

                # Attempt to read 8 bytes as a big-endian un-signed 64-bit
                # integer ...
                val, = struct.unpack(">Q", fobj.read(8))                        # [B]
                off += 8

            # Skip to the end of the atom ...
            fobj.seek(val - off, os.SEEK_CUR)

    # Catch possible errors ...
    if not foundMDAT:
        raise Exception("did not find mdat atom in \"{0:s}\"".format(fname))
    if not foundMOOV:
        raise Exception("did not find moov atom in \"{0:s}\"".format(fname))
