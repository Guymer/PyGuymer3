#!/usr/bin/env python3

# Define function ...
def load_ExtensionData(
    fObj,
    /,
):
    # NOTE: See https://github.com/lw/BluRay/wiki/ExtensionData

    # Import standard modules ...
    import struct

    # Initialize answer and find the current position ...
    ans = {}
    pos = fObj.tell()                                                           # [B]

    # Read the binary data ...
    ans["Length"], = struct.unpack(">I", fObj.read(4))                          # [B]
    if ans["Length"] != 0:
        ans["DataBlockStartAddress"], = struct.unpack(">I", fObj.read(4))
        fObj.read(3)
        ans["NumberOfExtDataEntries"], = struct.unpack(">B", fObj.read(1))
        ans["ExtDataEntries"] = []
        for _ in range(ans["NumberOfExtDataEntries"]):
            tmp = {}
            tmp["ExtDataType"], = struct.unpack(">H", fObj.read(2))
            tmp["ExtDataVersion"], = struct.unpack(">H", fObj.read(2))
            tmp["ExtDataStartAddress"], = struct.unpack(">I", fObj.read(4))
            tmp["ExtDataLength"], = struct.unpack(">I", fObj.read(4))           # [B]
            ans["ExtDataEntries"].append(tmp)

        # NOTE: ExtDataEntries is not implemented

    # Skip ahead to the end of the data structure ...
    fObj.seek(pos + ans["Length"] + 4)

    # Return answer ...
    return ans
