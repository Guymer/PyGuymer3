def print_FLAC_blocks(fname):
    # NOTE: The following website has some very useful information on how to
    #       parse FLAC files.
    #         * https://xiph.org/flac/format.html

    # Import standard modules ...
    import os

    # Load sub-functions ...
    from .convert_bytes_to_pretty_bytes import convert_bytes_to_pretty_bytes

    # List block types ...
    blocks = {
        0 : "STREAMINFO",
        1 : "PADDING",
        2 : "APPLICATION",
        3 : "SEEKTABLE",
        4 : "VORBIS_COMMENT",
        5 : "CUESHEET",
        6 : "PICTURE",
        127 : "INVALID",
    }

    # Open FLAC read-only ...
    with open(fname, "rb") as fobj:
        # Create short-hand ...
        fsize = os.path.getsize(fname)                                          # [B]

        # Read magic marker and raise exception if it is not expected ...
        if fobj.read(4).decode("utf-8") != "fLaC":
            raise Exception("\"{0:s}\" is not a FLAC".format(fname))

        # Initialize flag ...
        last = False

        # Loop over entire contents of FLAC ...
        while fobj.tell() < fsize:
            # Attempt to read 1 byte as a big-endian un-signed integer ...
            name = int.from_bytes(fobj.read(1), byteorder = "big", signed = False)

            # Check if this integer has the "last block" flag set and take it
            # off ...
            if name > 127:
                last = True
                name -= 128

            # Attempt to read 3 bytes as a big-endian un-signed integer ...
            val = int.from_bytes(fobj.read(3), byteorder = "big", signed = False)   # [B]

            # Print summary ...
            size, units = convert_bytes_to_pretty_bytes(val + 4)
            print("{0:s} is {1:6.1f} {2:3s} long".format(blocks.get(name, "RESERVED"), size, units))

            # Check if this is the last block before the frames ...
            if last:
                # Stop looping ...
                break

            # Skip to the end of the block ...
            fobj.seek(val, os.SEEK_CUR)
