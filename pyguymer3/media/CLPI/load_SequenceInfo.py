#!/usr/bin/env python3

# Define function ...
def load_SequenceInfo(fObj):
    # NOTE: see https://github.com/lw/BluRay/wiki/SequenceInfo

    # Import standard modules ...
    import struct

    # Import sub-functions ...
    from .load_ATCSequence import load_ATCSequence

    # Initialize variables ...
    ans = dict()

    # Read the binary data ...
    ans["Length"], = struct.unpack(">I", fObj.read(4))
    BytesStart = fObj.tell()

    fObj.read(1)
    ans["NumberOfATCSequences"], = struct.unpack(">B", fObj.read(1))

    # Loop over PlayItems ...
    ans["ATCSequences"] = []
    for i in range(ans["NumberOfATCSequences"]):
        # Load ATCSequence section and append to ATCSequences list ...
        res = load_ATCSequence(fObj)
        ans["ATCSequences"].append(res)

    # Pad out the read ...
    BytesEnd = fObj.tell()
    BytesPassed = BytesEnd - BytesStart
    if BytesPassed < ans["Length"]:
        l = ans["Length"] - BytesPassed
        fObj.read(l)
    elif BytesPassed > ans["Length"]:
        print("load_SequenceInfo: incorrect length")

    # Return answer ...
    return ans
