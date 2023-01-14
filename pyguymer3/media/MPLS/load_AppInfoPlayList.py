#!/usr/bin/env python3

# Define function ...
def load_AppInfoPlayList(fObj, kwArgCheck = None, debug = False, errors = "strict", indent = 0):
    # NOTE: see https://github.com/lw/BluRay/wiki/AppInfoPlayList

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
    ans["Length"], = struct.unpack(">I", fObj.read(4))                          # [B]
    if debug:
        print(" and is {:,d} bytes long".format(ans["Length"] + 4))
    if ans["Length"] != 0:
        fObj.read(1)
        ans["PlaybackType"], = struct.unpack(">B", fObj.read(1))
        if ans["PlaybackType"] in [int(0x02), int(0x03)]:
            ans["PlaybackCount"], = struct.unpack(">H", fObj.read(2))
        else:
            fObj.read(2)
        ans["UOMaskTable"], = struct.unpack(">Q", fObj.read(8))
        ans["MiscFlags"], = struct.unpack(">H", fObj.read(2))

    # Skip ahead to the end of the data structure ...
    fObj.seek(pos + ans["Length"] + 4)

    # Return answer ...
    return ans
