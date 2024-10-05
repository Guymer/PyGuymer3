#!/usr/bin/env python3

# Define function ...
def load_PlayItem(
    fObj,
    /,
):
    # NOTE: See https://github.com/lw/BluRay/wiki/PlayItem

    # Import standard modules ...
    import struct

    # Import sub-functions ...
    from .load_STNTable import load_STNTable

    # Initialize answer and find the current position ...
    ans = {}
    pos = fObj.tell()                                                           # [B]

    # Read the binary data ...
    ans["Length"], = struct.unpack(">H", fObj.read(2))                          # [B]
    if ans["Length"] != 0:
        ans["ClipInformationFileName"] = fObj.read(5).decode("utf-8")
        ans["ClipCodecIdentifier"] = fObj.read(4).decode("utf-8")
        ans["MiscFlags1"], = struct.unpack(">H", fObj.read(2))
        ans["IsMultiAngle"] = bool(ans["MiscFlags1"]&(1<<16-1-11))
        ans["RefToSTCID"], = struct.unpack(">B", fObj.read(1))
        ans["INTime"], = struct.unpack(">I", fObj.read(4))
        ans["OUTTime"], = struct.unpack(">I", fObj.read(4))
        ans["UOMaskTable"], = struct.unpack(">Q", fObj.read(8))
        ans["MiscFlags2"], = struct.unpack(">B", fObj.read(1))
        ans["StillMode"], = struct.unpack(">B", fObj.read(1))
        if ans["StillMode"] in [int(0x01)]:
            ans["StillTime"], = struct.unpack(">H", fObj.read(2))
        else:
            fObj.read(2)
        if ans["IsMultiAngle"]:
            ans["NumberOfAngles"], = struct.unpack(">B", fObj.read(1))
            ans["MiscFlags3"], = struct.unpack(">B", fObj.read(1))
            ans["Angles"] = []
            for _ in range(ans["NumberOfAngles"] - 1):
                tmp = {}
                tmp["ClipInformationFileName"] = fObj.read(5).decode("utf-8")
                tmp["ClipCodecIdentifier"] = fObj.read(4).decode("utf-8")
                tmp["RefToSTCID"], = struct.unpack(">B", fObj.read(1))
                ans["Angles"].append(tmp)

        # Load STNTable section ...
        ans["STNTable"] = load_STNTable(fObj)

    # Skip ahead to the end of the data structure ...
    fObj.seek(pos + ans["Length"] + 2)

    # Return answer ...
    return ans
