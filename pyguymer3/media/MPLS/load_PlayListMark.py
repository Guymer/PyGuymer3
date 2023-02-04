#!/usr/bin/env python3

# Define function ...
def load_PlayListMark(fObj, /):
    # NOTE: See https://github.com/lw/BluRay/wiki/PlayListMark

    # Import standard modules ...
    import struct

    # Initialize answer and find the current position ...
    ans = {}
    pos = fObj.tell()                                                           # [B]

    # Read the binary data ...
    ans["Length"], = struct.unpack(">I", fObj.read(4))                          # [B]
    if ans["Length"] != 0:
        ans["NumberOfPlayListMarks"], = struct.unpack(">H", fObj.read(2))
        ans["PlayListMarks"] = []
        for _ in range(ans["NumberOfPlayListMarks"]):
            tmp = {}
            fObj.read(1)
            tmp["MarkType"], = struct.unpack(">B", fObj.read(1))
            tmp["RefToPlayItemID"], = struct.unpack(">H", fObj.read(2))
            tmp["MarkTimeStamp"], = struct.unpack(">I", fObj.read(4))
            tmp["EntryESPID"], = struct.unpack(">H", fObj.read(2))
            tmp["Duration"], = struct.unpack(">I", fObj.read(4))
            ans["PlayListMarks"].append(tmp)

    # Skip ahead to the end of the data structure ...
    fObj.seek(pos + ans["Length"] + 4)

    # Return answer ...
    return ans
