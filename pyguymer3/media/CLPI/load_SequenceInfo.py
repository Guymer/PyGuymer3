#!/usr/bin/env python3

# Define function ...
def load_SequenceInfo(
    fObj,
    /,
):
    # NOTE: See https://github.com/lw/BluRay/wiki/SequenceInfo

    # Import standard modules ...
    import struct

    # Import sub-functions ...
    from .load_ATCSequence import load_ATCSequence

    # Initialize variables ...
    ans = {}

    # Read the binary data ...
    ans["Length"], = struct.unpack(">I", fObj.read(4))
    BytesStart = fObj.tell()

    # Read the binary data ...
    fObj.read(1)
    ans["NumberOfATCSequences"], = struct.unpack(">B", fObj.read(1))

    # Read the binary data ...
    ans["ATCSequences"] = []
    for _ in range(ans["NumberOfATCSequences"]):
        ans["ATCSequences"].append(load_ATCSequence(fObj))

    # Pad out the read ...
    BytesEnd = fObj.tell()
    bytesPassed = BytesEnd - BytesStart
    if bytesPassed < ans["Length"]:
        l = ans["Length"] - bytesPassed
        fObj.read(l)
    elif bytesPassed > ans["Length"]:
        raise Exception("read more bytes than the length") from None

    # Return answer ...
    return ans
