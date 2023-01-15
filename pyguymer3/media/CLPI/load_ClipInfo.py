#!/usr/bin/env python3

# Define function ...
def load_ClipInfo(fObj):
    # NOTE: See https://github.com/lw/BluRay/wiki/ClipInfo

    # Import standard modules ...
    import struct

    # Import sub-functions ...
    from .load_TSTypeInfoBlock import load_TSTypeInfoBlock

    # Initialize variables ...
    ans = {}

    # Read the binary data ...
    ans["Length"], = struct.unpack(">I", fObj.read(4))
    BytesStart = fObj.tell()

    # Read the binary data ...
    fObj.read(2)
    ans["ClipStreamType"], = struct.unpack(">B", fObj.read(1))
    ans["ApplicationType"], = struct.unpack(">B", fObj.read(1))
    ans["IsCC5"], = struct.unpack(">I", fObj.read(4))
    ans["TSRecordingRate"], = struct.unpack(">I", fObj.read(4))
    ans["NumberOfSourcePackets"], = struct.unpack(">I", fObj.read(4))
    fObj.read(128)
    ans["TSTypeInfoBlock"] = load_TSTypeInfoBlock(fObj)

    # Check if CC5 ...
    if ans["IsCC5"] == 1:
        # Read the binary data ...
        fObj.read(1)
        ans["FollowingClipStreamType"], = struct.unpack(">B", fObj.read(1))
        fObj.read(4)
        ans["FollowingClipInformationFileName"] = fObj.read(5).decode("utf-8")
        ans["FollowingClipCodecIdentifier"] = fObj.read(1)
        ans["FollowingClipCodecIdentifier"] = ans["FollowingClipCodecIdentifier"].decode("utf-8")
        fObj.read(1)

    # Pad out the read ...
    BytesEnd = fObj.tell()
    BytesPassed = BytesEnd - BytesStart
    if BytesPassed < ans["Length"]:
        l = ans["Length"] - BytesPassed
        fObj.read(l)
    elif BytesPassed > ans["Length"]:
        raise Exception("read more bytes than the length") from None

    # Return answer ...
    return ans
