def return_hash_of_GZ(fname, ignoreModificationTime = True):
    """
    This function returns the SHA512 hash of the passed GZ file as if the first
    "Modification Time" field is set to zero. Using this function it is possible
    to discover that the only binary difference between two different GZ files
    is the first "Modification Time" field.

    Note that a GZ file may contain multiple "members"; this function only finds
    the "Modification Time" field for the *first* "member".

    If the optional second argument is passed as False then this function will
    return the SHA512 identically to any other method.
    """

    # NOTE: The following websites have some very useful information on how to
    #       parse GZ files.
    #         * https://en.wikipedia.org/wiki/Gzip#File_format
    #         * https://tools.ietf.org/html/rfc1952.html#page-5

    # Import standard modules ...
    import hashlib
    import struct

    # Open GZ read-only ...
    with open(fname, "rb") as fobj:
        # Construct a hash object ...
        hobj = hashlib.sha512()

        # Attempt to read 2 bytes and pass them to the hash object ...
        src = fobj.read(2)
        hobj.update(src)

        # Check that this is a GZ file ...
        if src != b"\x1f\x8b":
            raise Exception(f"\"{fname}\" is not a GZ") from None

        # Pass 2 bytes to the hash object ...
        hobj.update(fobj.read(2))

        # Check what the user wants to do ...
        if ignoreModificationTime:
            # Pass 0 as a little-endian un-signed 32-bit integer to the hash
            # object ...
            fobj.read(4)
            hobj.update(struct.pack(">I", 0))
        else:
            # Pass 2 bytes to the hash object ...
            hobj.update(fobj.read(4))

        # Pass the rest of the file to the hash object ...
        hobj.update(fobj.read())

    # Return hash hexdigest ...
    return hobj.hexdigest()
