#!/usr/bin/env python3

# Define function ...
def load_SubPath(fObj, kwArgCheck = None, debug = False, errors = "strict", indent = 0):
    # NOTE: see https://github.com/lw/BluRay/wiki/SubPath

    # Import standard modules ...
    import struct

    # Import sub-functions ...
    from .load_SubPlayItem import load_SubPlayItem

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Initialize answer and find it current position ...
    ans = {}
    pos = fObj.tell()                                                           # [B]
    if debug:
        print("DEBUG:{:s} {:s}() called at {:,d} bytes".format(indent * "  ", __name__, pos), end = "")

    # Read the binary data ...
    ans["Length"], = struct.unpack(">I", fObj.read(4))                          # [B]
    if debug:
        print(" and is {:,d} bytes long".format(ans["Length"] + 4))
    if ans["Length"] != 0:
        fObj.read(1)
        ans["SubPathType"], = struct.unpack(">B", fObj.read(1))
        ans["MiscFlags1"], = struct.unpack(">H", fObj.read(2))
        fObj.read(1)
        ans["NumberOfSubPlayItems"], = struct.unpack(">B", fObj.read(1))
        ans["SubPlayItems"] = []
        for i in range(ans["NumberOfSubPlayItems"]):
            ans["SubPlayItems"].append(load_SubPlayItem(fObj, debug = debug, errors = errors, indent = indent + 1))

    # Skip ahead to the end of the data structure ...
    fObj.seek(pos + ans["Length"] + 4)

    # Return answer ...
    return ans
