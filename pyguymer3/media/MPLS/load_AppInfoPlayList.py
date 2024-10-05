#!/usr/bin/env python3

# Define function ...
def load_AppInfoPlayList(
    fObj,
    /,
):
    # NOTE: See https://github.com/lw/BluRay/wiki/AppInfoPlayList

    # Import standard modules ...
    import struct

    # Initialize answer and find the current position ...
    ans = {}
    pos = fObj.tell()                                                           # [B]

    # Read the binary data ...
    ans["Length"], = struct.unpack(">I", fObj.read(4))                          # [B]
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
