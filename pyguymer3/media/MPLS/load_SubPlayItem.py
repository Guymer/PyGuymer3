#!/usr/bin/env python3

# Define function ...
def load_SubPlayItem(fObj, kwArgCheck = None, debug = False, errors = "strict", indent = 0):
    # NOTE: see https://github.com/lw/BluRay/wiki/SubPlayItem

    # Import standard modules ...
    import struct

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Initialize answer and find it current position ...
    ans = {}
    pos = fObj.tell()                                                           # [B]
    if debug:
        print("DEBUG:{:s} {:s}() called at {:,d} bytes".format(indent * "  ", __name__, pos), end = "")

    # Read the binary data ...
    ans["Length"], = struct.unpack(">H", fObj.read(2))                          # [B]
    if debug:
        print(" and is {:,d} bytes long".format(ans["Length"] + 2))
    if ans["Length"] != 0:
        ans["ClipInformationFileName"] = fObj.read(5).decode("utf-8", errors = errors)
        ans["ClipCodecIdentifier"] = fObj.read(4).decode("utf-8", errors = errors)
        ans["MiscFlags1"], = struct.unpack(">I", fObj.read(4))
        ans["RefToSTCID"], = struct.unpack(">B", fObj.read(1))
        ans["INTime"], = struct.unpack(">I", fObj.read(4))
        ans["OUTTime"], = struct.unpack(">I", fObj.read(4))
        ans["SyncPlayItemID"], = struct.unpack(">H", fObj.read(2))
        ans["SyncStartPTS"], = struct.unpack(">I", fObj.read(4))

        # NOTE: IsMultiClipEntries is not implemented

    # Skip ahead to the end of the data structure ...
    fObj.seek(pos + ans["Length"] + 2)

    # Return answer ...
    return ans
