#!/usr/bin/env python3

# Define function ...
def load_SubPlayItem(
    fObj,
    /,
):
    # NOTE: See https://github.com/lw/BluRay/wiki/SubPlayItem

    # Import standard modules ...
    import struct

    # Initialize answer and find the current position ...
    ans = {}
    pos = fObj.tell()                                                           # [B]

    # Read the binary data ...
    ans["Length"], = struct.unpack(">H", fObj.read(2))                          # [B]
    if ans["Length"] != 0:
        ans["ClipInformationFileName"] = fObj.read(5).decode("utf-8")
        ans["ClipCodecIdentifier"] = fObj.read(4).decode("utf-8")
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
