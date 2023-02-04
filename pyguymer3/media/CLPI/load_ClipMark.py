#!/usr/bin/env python3

# Define function ...
def load_ClipMark(fObj, /):
    # NOTE: See https://github.com/lw/BluRay/wiki/ClipMark

    # Import standard modules ...
    import struct

    # Initialize variables ...
    ans = {}

    # Read the binary data ...
    ans["Length"], = struct.unpack(">I", fObj.read(4))
    bytesStart = fObj.tell()

    # Pad out the read ...
    bytesEnd = fObj.tell()
    bytesPassed = bytesEnd - bytesStart
    if bytesPassed < ans["Length"]:
        l = ans["Length"] - bytesPassed
        fObj.read(l)
    elif bytesPassed > ans["Length"]:
        raise Exception("read more bytes than the length") from None

    # Return answer ...
    return ans
