#!/usr/bin/env python3

# Define function ...
def load_ExtensionData(
    fObj,
    /,
):
    # NOTE: See https://github.com/lw/BluRay/wiki/ExtensionData

    # Import standard modules ...
    import struct

    # Initialize variables ...
    ans = {}

    # Read the binary data ...
    ans["Length"], = struct.unpack(">I", fObj.read(4))
    BytesStart = fObj.tell()

    # Check if things need reading ...
    if ans["Length"] != 0:
        # Read the binary data ...
        ans["DataBlockStartAddress"], = struct.unpack(">I", fObj.read(4))
        fObj.read(3)
        ans["NumberOfExtDataEntries"], = struct.unpack(">B", fObj.read(1))
        ans["ExtDataEntries"] = []
        for _ in range(ans["NumberOfExtDataEntries"]):
            tmp = {}
            tmp["ExtDataType"], = struct.unpack(">H", fObj.read(2))
            tmp["ExtDataVersion"], = struct.unpack(">H", fObj.read(2))
            tmp["ExtDataStartAddress"], = struct.unpack(">I", fObj.read(4))
            tmp["ExtDataLength"], = struct.unpack(">I", fObj.read(4))
            ans["ExtDataEntries"].append(tmp)

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
