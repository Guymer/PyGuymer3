#!/usr/bin/env python3

# Define function ...
def load_ExtensionData(fObj):
    # NOTE: see https://github.com/lw/BluRay/wiki/ExtensionData

    # Import standard modules ...
    import struct

    # Initialize variables ...
    ans = dict()

    # Read the binary data ...
    ans["Length"], = struct.unpack(">I", fObj.read(4))
    BytesStart = fObj.tell()

    if ans["Length"] != 0:
        ans["DataBlockStartAddress"], = struct.unpack(">I", fObj.read(4))
        fObj.read(3)
        ans["NumberOfExtDataEntries"], = struct.unpack(">B", fObj.read(1))
        ans["ExtDataEntries"] = list()
        for i in range(ans["NumberOfExtDataEntries"]):
            tmp = dict()
            tmp["ExtDataType"], = struct.unpack(">H", fObj.read(2))
            tmp["ExtDataVersion"], = struct.unpack(">H", fObj.read(2))
            tmp["ExtDataStartAddress"], = struct.unpack(">I", fObj.read(4))
            tmp["ExtDataLength"], = struct.unpack(">I", fObj.read(4))
            ans["ExtDataEntries"].append(tmp)

        # NOTE: ExtDataEntries is not implemented

    # Pad out the read ...
    BytesEnd = fObj.tell()
    BytesPassed = BytesEnd - BytesStart
    if BytesPassed < ans["Length"]:
        l = ans["Length"] - BytesPassed
        fObj.read(l)
    elif BytesPassed > ans["Length"]:
        print("load_ClipInfo: incorrect length")

    # Return answer ...
    return ans
