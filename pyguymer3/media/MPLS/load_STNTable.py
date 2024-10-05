#!/usr/bin/env python3

# Define function ...
def load_STNTable(
    fObj,
    /,
):
    # NOTE: See https://github.com/lw/BluRay/wiki/STNTable

    # Import standard modules ...
    import struct

    # Import sub-functions ...
    from .load_StreamAttributes import load_StreamAttributes
    from .load_StreamEntry import load_StreamEntry

    # Initialize answer and find the current position ...
    ans = {}
    pos = fObj.tell()                                                           # [B]

    # Read the binary data ...
    ans["Length"], = struct.unpack(">H", fObj.read(2))                          # [B]
    if ans["Length"] != 0:
        fObj.read(2)
        ans["NumberOfPrimaryVideoStreamEntries"], = struct.unpack(">B", fObj.read(1))
        ans["NumberOfPrimaryAudioStreamEntries"], = struct.unpack(">B", fObj.read(1))
        ans["NumberOfPrimaryPGStreamEntries"], = struct.unpack(">B", fObj.read(1))
        ans["NumberOfPrimaryIGStreamEntries"], = struct.unpack(">B", fObj.read(1))
        ans["NumberOfSecondaryAudioStreamEntries"], = struct.unpack(">B", fObj.read(1))
        ans["NumberOfSecondaryVideoStreamEntries"], = struct.unpack(">B", fObj.read(1))
        ans["NumberOfSecondaryPGStreamEntries"], = struct.unpack(">B", fObj.read(1))
        ans["NumberOfDVStreamEntries"], = struct.unpack(">B", fObj.read(1))
        fObj.read(4)

        # Loop over stream list names ...
        for name in ["PrimaryVideoStreamEntries", "PrimaryAudioStreamEntries", "PrimaryPGStreamEntries", "SecondaryPGStreamEntries", "PrimaryIGStreamEntries", "SecondaryAudioStreamEntries", "SecondaryVideoStreamEntries", "DVStreamEntries"]:
            # Loop over entries and add to list ...
            ans[name] = []
            for _ in range(ans[f"NumberOf{name}"]):
                tmp = {}
                tmp["StreamEntry"] = load_StreamEntry(fObj)
                tmp["StreamAttributes"] = load_StreamAttributes(fObj)
                ans[name].append(tmp)

    # Skip ahead to the end of the data structure ...
    fObj.seek(pos + ans["Length"] + 2)

    # Return answer ...
    return ans
