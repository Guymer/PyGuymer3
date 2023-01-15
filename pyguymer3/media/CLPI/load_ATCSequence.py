#!/usr/bin/env python3

# Define function ...
def load_ATCSequence(fObj):
    # NOTE: See https://github.com/lw/BluRay/wiki/SequenceInfo

    # Import standard modules ...
    import struct

    # Import sub-functions ...
    from .load_STCSequence import load_STCSequence

    # Initialize variables ...
    ans = {}

    # Read the binary data ...
    ans["SPNATCStart"], = struct.unpack(">I", fObj.read(4))
    ans["NumberOfSTCSequences"], = struct.unpack(">B", fObj.read(1))
    ans["OffsetSTCID"], = struct.unpack(">B", fObj.read(1))

    # Read the binary data ...
    ans["STCSequences"] = []
    for _ in range(ans["NumberOfSTCSequences"]):
        ans["STCSequences"].append(load_STCSequence(fObj))

    # Return answer ...
    return ans
