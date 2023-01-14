#!/usr/bin/env python3

# Define function ...
def load_TSTypeInfoBlock(fObj):
    # NOTE: see https://github.com/lw/BluRay/blob/master/src/TSTypeInfoBlock.vala

    # Import standard modules ...
    import struct

    # Initialize variables ...
    ans = {}

    # Read the binary data ...
    ans["Length"], = struct.unpack(">H", fObj.read(2))

    BytesStart = fObj.tell()

    fObj.read(ans["Length"])

    BytesEnd = fObj.tell()
    BytesPassed = BytesEnd - BytesStart
    if BytesPassed < ans["Length"]:
        l = ans["Length"] - BytesPassed
        fObj.read(l)
        print("load_ClipInfo: skip %d bytes" % l)
    elif BytesPassed > ans["Length"]:
        print("load_ClipInfo: incorrect length")

    # Return answer ...
    return ans
