#!/usr/bin/env python3

# Define function ...
def load_ClipInfo(fObj):
    # NOTE: see https://github.com/lw/BluRay/wiki/ClipInfo

    # Import standard modules ...
    import struct

    # Import sub-functions ...
    from .load_TSTypeInfoBlock import load_TSTypeInfoBlock

    # Initialize variables ...
    ans = {}

    # Read the binary data ...
    ans["Length"], = struct.unpack(">I", fObj.read(4))
    BytesStart = fObj.tell()

    fObj.read(2)
    ans["ClipStreamType"], = struct.unpack(">B", fObj.read(1))
    ans["ApplicationType"], = struct.unpack(">B", fObj.read(1))
    ans["IsCC5"], = struct.unpack(">I", fObj.read(4))
    ans["TSRecordingRate"], = struct.unpack(">I", fObj.read(4))
    ans["NumberOfSourcePackets"], = struct.unpack(">I", fObj.read(4))
    fObj.read(128)
    ans["TSTypeInfoBlock"] = load_TSTypeInfoBlock(fObj)

    if ans["IsCC5"] == 1:
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
        print("load_ClipInfo: incorrect length")

    # Return answer ...
    return ans
