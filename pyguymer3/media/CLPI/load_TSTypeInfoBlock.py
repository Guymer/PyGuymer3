#!/usr/bin/env python3

# Define function ...
def load_TSTypeInfoBlock(fObj, /):
    # NOTE: See https://github.com/lw/BluRay/blob/master/src/TSTypeInfoBlock.vala

    # Import standard modules ...
    import struct

    # Initialize variables ...
    ans = {}

    # Read the binary data ...
    ans["Length"], = struct.unpack(">H", fObj.read(2))
    BytesStart = fObj.tell()

    # Pad out the read ...
    BytesEnd = fObj.tell()
    bytesPassed = BytesEnd - BytesStart
    if bytesPassed < ans["Length"]:
        l = ans["Length"] - bytesPassed
        fObj.read(l)
    elif bytesPassed > ans["Length"]:
        raise Exception("read more bytes than the length") from None

    # Return answer ...
    return ans
