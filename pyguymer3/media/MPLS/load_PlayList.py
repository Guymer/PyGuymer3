#!/usr/bin/env python3

# Define function ...
def load_PlayList(
    fObj,
    /,
):
    # NOTE: See https://github.com/lw/BluRay/wiki/PlayList

    # Import standard modules ...
    import struct

    # Import sub-functions ...
    from .load_PlayItem import load_PlayItem
    from .load_SubPath import load_SubPath

    # Initialize answer and find the current position ...
    ans = {}
    pos = fObj.tell()                                                           # [B]

    # Read the binary data ...
    ans["Length"], = struct.unpack(">I", fObj.read(4))                          # [B]
    if ans["Length"] != 0:
        fObj.read(2)
        ans["NumberOfPlayItems"], = struct.unpack(">H", fObj.read(2))
        ans["NumberOfSubPaths"], = struct.unpack(">H", fObj.read(2))

        # Loop over PlayItems ...
        ans["PlayItems"] = []
        for _ in range(ans["NumberOfPlayItems"]):
            # Load PlayItem section and append to PlayItems list ...
            ans["PlayItems"].append(load_PlayItem(fObj))

        # Loop over SubPaths ...
        ans["SubPaths"] = []
        for _ in range(ans["NumberOfSubPaths"]):
            # Load SubPath section and append to SubPaths list ...
            ans["SubPaths"].append(load_SubPath(fObj))

    # Skip ahead to the end of the data structure ...
    fObj.seek(pos + ans["Length"] + 4)

    # Return answer ...
    return ans
