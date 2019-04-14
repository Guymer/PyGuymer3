# -*- coding: utf-8 -*-

def print_MP4_atoms(fname = "missing"):
    # NOTE: The following websites have some very useful information on how to
    #       parse MP4 files - the first just forgot to say that integers are
    #       big-endian.
    #         * http://atomicparsley.sourceforge.net/mpeg-4files.html
    #         * https://wiki.multimedia.cx/index.php/QuickTime_container

    # Import modules ...
    import os
    import re
    import struct

    # Load sub-functions ...
    from .convert_bytes_to_pretty_bytes import convert_bytes_to_pretty_bytes

    # Open MP4 read-only ...
    with open(fname, "rb") as fobj:
        # Set trigger ...
        foundFTYP = False

        # Loop over entire contents of MP4 ...
        while fobj.tell() < os.path.getsize(fname):
            # Attempt to read 4 bytes as a big-endian un-signed 32 bit integer ...
            val, = struct.unpack(">I", fobj.read(4))                           # [B]
            off = 4

            # Extract atom name ...
            name = fobj.read(4)
            off += 4

            # Check that it matches the pattern ...
            if re.match(r"[a-z][a-z][a-z][a-z]", name) is None:
                raise Exception("\"{0:s}\" is not an atom name in \"{1:s}\"".format(name, fname))

            # Check that it is a MP4 file ...
            if not foundFTYP and name != "ftyp":
                raise Exception("\"{0:s}\" is not a MP4".format(fname))
            else:
                foundFTYP = True

            # Check the length ...
            if val == 0:
                # NOTE: This atom runs until EOF.

                # Print summary ...
                print("{0:s} is the remainder of the file".format(name))

                # Stop looping ...
                break
            elif val == 1:
                # NOTE: This atom has 64-bit sizes.

                # Attempt to read 8 bytes as a big-endian un-signed 64 bit integer ...
                val, = struct.unpack(">Q", fobj.read(8))                       # [B]
                off += 8

                # Print summary ...
                size, units = convert_bytes_to_pretty_bytes(val)
                print("{0:s} is {1:6.1f} {2:3s} long (as a 64-bit atom)".format(name, size, units))
            else:
                # Print summary ...
                size, units = convert_bytes_to_pretty_bytes(val)
                print("{0:s} is {1:6.1f} {2:3s} long (as a 32-bit atom)".format(name, size, units))

            # Skip to the end of the atom ...
            fobj.seek(val - off, os.SEEK_CUR)
