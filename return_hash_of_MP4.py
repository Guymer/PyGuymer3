def return_hash_of_MP4(fname, ignoreModificationTime = True):
    """
    This function returns the SHA512 hash of the passed MP4 file as if the
    "Modification Time" field (in the "mvhd" atom in the "moov" atom) is set to
    zero. Using this function it is possible to discover that the only binary
    difference between two different MP4 files is the "Modification Time" field
    (in the "mvhd" atom in the "moov" atom).

    If the optional second argument is passed as False then this function will
    return the SHA512 identically to any other method.
    """

    # NOTE: The following websites have some very useful information on how to
    #       parse MP4 files - the first just forgot to say that integers are
    #       big-endian.
    #         * http://atomicparsley.sourceforge.net/mpeg-4files.html
    #         * https://developer.apple.com/library/archive/documentation/QuickTime/QTFF/QTFFChap2/qtff2.html
    #         * https://wiki.multimedia.cx/index.php/QuickTime_container

    # Import standard modules ...
    import hashlib
    import os
    import re
    import struct

    # Open MP4 read-only ...
    with open(fname, "rb") as fobj:
        # Construct a hash object ...
        hobj = hashlib.sha512()

        # Create short-hand ...
        fsize = os.path.getsize(fname)                                          # [B]

        # Set triggers ...
        foundFTYP = False
        foundMOOV = False
        foundMVHD = False

        # Loop over entire contents of MP4 ...
        while fobj.tell() < fsize:
            # Attempt to read 4 bytes as a big-endian un-signed 32-bit integer
            # and pass them to the hash object ...
            src = fobj.read(4)
            val, = struct.unpack(">I", src)                                     # [B]
            hobj.update(src)
            off = 4                                                             # [B]

            # Extract atom name and pass it to the hash object ...
            src = fobj.read(4)
            name = src.decode("utf-8")
            hobj.update(src)
            off += 4                                                            # [B]

            # Check that it matches the pattern ...
            if re.match(r"[a-z][a-z][a-z][a-z]", name) is None:
                raise Exception("\"{0:s}\" is not an atom name in \"{1:s}\"".format(name, fname)) from None

            # Check that it is a MP4 file ...
            if not foundFTYP and name != "ftyp":
                raise Exception("\"{0:s}\" is not a MP4".format(fname)) from None
            else:
                foundFTYP = True

            # Check the length ...
            if val == 0:
                # NOTE: This atom runs until EOF.

                # Pass the rest of the atom to the hash object ...
                hobj.update(fobj.read())

                # Stop looping ...
                break
            elif val == 1:
                # NOTE: This atom has 64-bit sizes.

                # Attempt to read 8 bytes as a big-endian un-signed 64-bit
                # integer and pass them to the hash object ...
                src = fobj.read(8)
                val, = struct.unpack(">Q", src)                                 # [B]
                hobj.update(src)
                off += 8                                                        # [B]

            # Create short-hand ...
            rem = val - off                                                     # [B]

            # Check if it is the MOOV atom ...
            if name == "moov":
                # Set trigger ...
                foundMOOV = True

                # Save starting position ...
                pos = fobj.tell()

                # Loop over remaining contents of MOOV atom ...
                while fobj.tell() - pos < rem:
                    # Attempt to read 4 bytes as a big-endian un-signed 32-bit
                    # integer and pass them to the hash object ...
                    src = fobj.read(4)
                    val, = struct.unpack(">I", src)                             # [B]
                    hobj.update(src)
                    off2 = 4                                                    # [B]

                    # Extract atom name and pass it to the hash object ...
                    src = fobj.read(4)
                    name = src.decode("utf-8")
                    hobj.update(src)
                    off2 += 4                                                   # [B]

                    # Check that it matches the pattern ...
                    if re.match(r"[a-z][a-z][a-z][a-z]", name) is None:
                        raise Exception("\"{0:s}\" is not an atom name in \"{1:s}\"".format(name, fname)) from None

                    # Check the length ...
                    if val == 0:
                        # NOTE: This atom runs until EOF.

                        # Pass the rest of the atom to the hash object ...
                        hobj.update(fobj.read())

                        # Stop looping ...
                        break
                    elif val == 1:
                        # NOTE: This atom has 64-bit sizes.

                        # Attempt to read 8 bytes as a big-endian un-signed
                        # 64-bit integer and pass them to the hash object ...
                        src = fobj.read(8)
                        val, = struct.unpack(">Q", src)                         # [B]
                        hobj.update(src)
                        off2 += 8                                               # [B]

                    # Create short-hand ...
                    rem2 = val - off                                            # [B]

                    # Check if it is the MVHD atom ...
                    if name == "mvhd":
                        # Set trigger ...
                        foundMVHD = True

                        # Check that it is the correct size ...
                        if rem2 != 100:
                            raise Exception("the \"mvhd\" atom in \"{0:s}\" is not the correct size".format(fname)) from None

                        # Pass the rest of the atom to the hash object (except
                        # the "Modification Time" for which instead pass 0 as a
                        # big-endian un-signed 32-bit integer) ...
                        # NOTE: See Figure 2-3 of https://developer.apple.com/library/archive/documentation/QuickTime/QTFF/QTFFChap2/qtff2.html
                        hobj.update(fobj.read(8))
                        if ignoreModificationTime:
                            fobj.read(4)
                            hobj.update(struct.pack(">I", 0))
                        else:
                            hobj.update(fobj.read(4))
                        hobj.update(fobj.read(88))
                    else:
                        # Pass the rest of the atom to the hash object ...
                        hobj.update(fobj.read(rem2))
            else:
                # Pass the rest of the atom to the hash object ...
                hobj.update(fobj.read(rem))

    # Catch possible errors ...
    if not foundMOOV:
        raise Exception("did not find moov atom in \"{0:s}\"".format(fname)) from None
    if not foundMVHD:
        raise Exception("did not find mvhd atom in \"{0:s}\"".format(fname)) from None

    # Return hash hexdigest ...
    return hobj.hexdigest()