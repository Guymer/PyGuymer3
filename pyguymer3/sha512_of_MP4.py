#!/usr/bin/env python3

# Define function ...
def sha512_of_MP4(
    fname,
    /,
    *,
                 chunksize = 1048576,
    ignoreModificationTime = True,
):
    """Find the SHA-512 hash of a MP4 file

    This function returns the SHA-512 hash of the passed MP4 file as if the
    "Modification Time" field (in the "mvhd" atom in the "moov" atom) is set to
    zero. Using this function it is possible to discover that the only binary
    difference between two different MP4 files is the "Modification Time" field
    (in the "mvhd" atom in the "moov" atom).

    If this function is told not to ignore the "Modification Time" field (in the
    "mvhd" atom in the "moov" atom) then this function will return the SHA-512
    identically to any other method.

    Parameters
    ----------
    fname : str
        the input MP4 file name
    chunksize : int, optional
        the size of the chunks of any files which are read in (in bytes)
    ignoreModificationTime : bool, optional
        ignore the "Modification Time" field (in the "mvhd" atom in the "moov"
        atom)

    Returns
    -------
    hexdigest : str
        The hash hexdigest of the input MP4 file name.

    Notes
    -----
    The following websites have some very useful information on how to parse MP4
    files - the first just forgot to say that integers are big-endian:

    - http://atomicparsley.sourceforge.net/mpeg-4files.html
    - https://developer.apple.com/library/archive/documentation/QuickTime/QTFF/QTFFChap2/qtff2.html
    - https://wiki.multimedia.cx/index.php/QuickTime_container

    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import standard modules ...
    import hashlib
    import os
    import re
    import struct

    # **************************************************************************

    # Construct a hash object ...
    hObj = hashlib.sha512()

    # Create short-hand ...
    fsize = os.path.getsize(fname)                                              # [B]

    # Open input MP4 file read-only ...
    with open(fname, "rb") as fObj:
        # Set triggers ...
        foundFTYP = False
        foundMOOV = False
        foundMVHD = False

        # Loop over entire contents of MP4 ...
        while fObj.tell() < fsize:
            # Attempt to read 4 bytes as a big-endian un-signed 32-bit integer
            # and pass it to the hash object ...
            src = fObj.read(4)
            val, = struct.unpack(">I", src)                                     # [B]
            hObj.update(src)
            off1 = 4                                                            # [B]

            # Extract atom name and pass it to the hash object ...
            src = fObj.read(4)
            name = src.decode("utf-8")
            hObj.update(src)
            off1 += 4                                                           # [B]

            # Check that the atom name matches the pattern ...
            if re.match(r"[a-z][a-z][a-z][a-z]", name) is None:
                raise Exception(f"\"{name}\" is not an atom name in \"{fname}\"") from None

            # Check that the input MP4 file is a MP4 file ...
            if not foundFTYP and name != "ftyp":
                raise Exception(f"\"{fname}\" is not a MP4") from None

            # Set trigger ...
            foundFTYP = True

            # Check the length ...
            if val == 0:
                # NOTE: This atom runs until EOF.

                # Pass the rest of the atom to the hash object using chunks ...
                while True:
                    chunk = fObj.read(chunksize)
                    if len(chunk) == 0:
                        break
                    hObj.update(chunk)

                # Stop looping ...
                break

            # Check the length ...
            if val == 1:
                # NOTE: This atom has 64-bit sizes.

                # Attempt to read 8 bytes as a big-endian un-signed 64-bit
                # integer and pass it to the hash object ...
                src = fObj.read(8)
                val, = struct.unpack(">Q", src)                                 # [B]
                hObj.update(src)
                off1 += 8                                                       # [B]

            # Create short-hand ...
            rem1 = val - off1                                                   # [B]

            # Check if it is the MOOV atom ...
            if name == "moov":
                # Set trigger ...
                foundMOOV = True

                # Save starting position ...
                pos = fObj.tell()

                # Loop over remaining contents of MOOV atom ...
                while fObj.tell() - pos < rem1:
                    # Attempt to read 4 bytes as a big-endian un-signed 32-bit
                    # integer and pass it to the hash object ...
                    src = fObj.read(4)
                    val, = struct.unpack(">I", src)                             # [B]
                    hObj.update(src)
                    off2 = 4                                                    # [B]

                    # Extract atom name and pass it to the hash object ...
                    src = fObj.read(4)
                    name = src.decode("utf-8")
                    hObj.update(src)
                    off2 += 4                                                   # [B]

                    # Check that the atom name matches the pattern ...
                    if re.match(r"[a-z][a-z][a-z][a-z]", name) is None:
                        raise Exception(f"\"{name}\" is not an atom name in \"{fname}\"") from None

                    # Check the length ...
                    if val == 0:
                        # NOTE: This atom runs until EOF.

                        # Pass the rest of the atom to the hash object using
                        # chunks ...
                        while True:
                            chunk = fObj.read(chunksize)
                            if len(chunk) == 0:
                                break
                            hObj.update(chunk)

                        # Stop looping ...
                        break

                    # Check the length ...
                    if val == 1:
                        # NOTE: This atom has 64-bit sizes.

                        # Attempt to read 8 bytes as a big-endian un-signed
                        # 64-bit integer and pass it to the hash object ...
                        src = fObj.read(8)
                        val, = struct.unpack(">Q", src)                         # [B]
                        hObj.update(src)
                        off2 += 8                                               # [B]

                    # Create short-hand ...
                    rem2 = val - off1                                           # [B]

                    # Check if it is the MVHD atom ...
                    if name == "mvhd":
                        # Set trigger ...
                        foundMVHD = True

                        # Check that it is the correct size ...
                        if rem2 != 100:
                            raise Exception(f"the \"mvhd\" atom in \"{fname}\" is not the correct size") from None

                        # Pass the rest of the atom to the hash object (except
                        # the "Modification Time" for which instead pass 0 as a
                        # big-endian un-signed 32-bit integer) ...
                        # NOTE: See Figure 2-3 of https://developer.apple.com/library/archive/documentation/QuickTime/QTFF/QTFFChap2/qtff2.html
                        hObj.update(fObj.read(8))
                        if ignoreModificationTime:
                            fObj.read(4)
                            hObj.update(struct.pack(">I", 0))
                        else:
                            hObj.update(fObj.read(4))
                        hObj.update(fObj.read(88))
                    else:
                        # Pass the rest of the atom to the hash object using
                        # chunks ...
                        while True:
                            if rem2 == 0:
                                break
                            if rem2 <= chunksize:
                                hObj.update(fObj.read(rem2))
                                break
                            hObj.update(fObj.read(chunksize))
                            rem2 -= chunksize                                   # [B]
            else:
                # Pass the rest of the atom to the hash object using chunks ...
                while True:
                    if rem1 == 0:
                        break
                    if rem1 <= chunksize:
                        hObj.update(fObj.read(rem1))
                        break
                    hObj.update(fObj.read(chunksize))
                    rem1 -= chunksize                                           # [B]

    # Catch possible errors ...
    if not foundMOOV:
        raise Exception(f"did not find \"moov\" atom in \"{fname}\"") from None
    if not foundMVHD:
        raise Exception(f"did not find \"mvhd\" atom in \"{fname}\"") from None

    # Return hash hexdigest ...
    return hObj.hexdigest()
