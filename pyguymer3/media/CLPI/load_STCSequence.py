#!/usr/bin/env python3

# Define function ...
def load_STCSequence(fObj, /):
    # NOTE: See https://github.com/lw/BluRay/wiki/SequenceInfo

    # Import standard modules ...
    import struct

    # Initialize variables ...
    ans = {}

    # Read the binary data ...
    ans["PCRPID"], = struct.unpack(">H", fObj.read(2))
    ans["SPNSTCStart"] = struct.unpack(">I", fObj.read(4))
    ans["PresentationStartTime"] = struct.unpack(">I", fObj.read(4))
    ans["PresentationEndTime"], = struct.unpack(">I", fObj.read(4))

    # Return answer ...
    return ans
