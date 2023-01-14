#!/usr/bin/env python3

# Define function ...
def load_ATCSequence(fObj):
    # NOTE: see https://github.com/lw/BluRay/wiki/SequenceInfo

    # Import standard modules ...
    import struct

    # Import sub-functions ...
    from .load_STCSequence import load_STCSequence

    # Initialize variables ...
    ans = dict()

    # Read the binary data ...
    ans["SPNATCStart"], = struct.unpack(">I", fObj.read(4))
    ans["NumberOfSTCSequences"], = struct.unpack(">B", fObj.read(1))
    ans["OffsetSTCID"], = struct.unpack(">B", fObj.read(1))

    # Load STCSequences section ...
    ans["STCSequences"] = list()
    for i in range(ans["NumberOfSTCSequences"]):
        # Load STCSequence section and append to STCSequences list ...
        res = load_STCSequence(fObj)
        ans["STCSequences"].append(res)

    # Return answer ...
    return ans
