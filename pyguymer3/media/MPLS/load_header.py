#!/usr/bin/env python3

# Define function ...
def load_header(fObj):
    # NOTE: See https://github.com/lw/BluRay/wiki/MPLS

    # Import standard modules ...
    import struct

    # Initialize answer and find the current position ...
    ans = {}
    pos = fObj.tell()                                                           # [B]

    # Check everything is going to be OK ...
    if pos != 0:
        raise Exception("\"load_header()\" should only be called at the start of the MPLS file") from None

    # Read the binary data ...
    ans["TypeIndicator"] = fObj.read(4).decode("utf-8", errors = "strict")
    ans["VersionNumber"] = fObj.read(4).decode("utf-8", errors = "strict")
    ans["PlayListStartAddress"], = struct.unpack(">I", fObj.read(4))
    ans["PlayListMarkStartAddress"], = struct.unpack(">I", fObj.read(4))
    ans["ExtensionDataStartAddress"], = struct.unpack(">I", fObj.read(4))
    fObj.read(20)

    # Return answer ...
    return ans
