#!/usr/bin/env python3

# Define function ...
def load_SubPath(fObj):
    # NOTE: See https://github.com/lw/BluRay/wiki/SubPath

    # Import standard modules ...
    import struct

    # Import sub-functions ...
    from .load_SubPlayItem import load_SubPlayItem

    # Initialize answer and find the current position ...
    ans = {}
    pos = fObj.tell()                                                           # [B]

    # Read the binary data ...
    ans["Length"], = struct.unpack(">I", fObj.read(4))                          # [B]
    if ans["Length"] != 0:
        fObj.read(1)
        ans["SubPathType"], = struct.unpack(">B", fObj.read(1))
        ans["MiscFlags1"], = struct.unpack(">H", fObj.read(2))
        fObj.read(1)
        ans["NumberOfSubPlayItems"], = struct.unpack(">B", fObj.read(1))
        ans["SubPlayItems"] = []
        for _ in range(ans["NumberOfSubPlayItems"]):
            ans["SubPlayItems"].append(load_SubPlayItem(fObj))

    # Skip ahead to the end of the data structure ...
    fObj.seek(pos + ans["Length"] + 4)

    # Return answer ...
    return ans
