#!/usr/bin/env python3

# Define function ...
def load_header(fObj):
    # NOTE: see https://github.com/lw/BluRay/wiki/CLPI

    # Import standard modules ...
    import struct

    # Initialize variables ...
    ans = dict()

    # Read the binary data ...
    ans["TypeIndicator"] = fObj.read(4).decode("utf-8")
    ans["VersionNumber"] = fObj.read(4).decode("utf-8")
    ans["SequenceInfoStartAddress"], = struct.unpack(">I", fObj.read(4))
    ans["ProgramInfoStartAddress"], = struct.unpack(">I", fObj.read(4))
    ans["CPIStartAddress"], = struct.unpack(">I", fObj.read(4))
    ans["ClipMarkStartAddress"], = struct.unpack(">I", fObj.read(4))
    ans["ExtensionDataStartAddress"], = struct.unpack(">I", fObj.read(4))
    fObj.read(12)

    # Return answer ...
    return ans
