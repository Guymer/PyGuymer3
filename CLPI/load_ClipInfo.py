def load_ClipInfo(fobj):
    # NOTE: see https://github.com/lerks/BluRay/wiki/ClipInfo

    # Import standard modules ...
    import struct

    # Load sub-functions ...
    from .load_TSTypeInfoBlock import load_TSTypeInfoBlock

    # Initialize variables ...
    ans = {}

    # Read the binary data ...
    ans["Length"], = struct.unpack(">I", fobj.read(4))
    BytesStart = fobj.tell()

    fobj.read(2)
    ans["ClipStreamType"], = struct.unpack(">B", fobj.read(1))
    ans["ApplicationType"], = struct.unpack(">B", fobj.read(1))
    ans["IsCC5"], = struct.unpack(">I", fobj.read(4))
    ans["TSRecordingRate"], = struct.unpack(">I", fobj.read(4))
    ans["NumberOfSourcePackets"], = struct.unpack(">I", fobj.read(4))
    fobj.read(128)
    ans["TSTypeInfoBlock"] = load_TSTypeInfoBlock(fobj)

    if ans["IsCC5"] == 1:
        fobj.read(1)
        ans["FollowingClipStreamType"], = struct.unpack(">B", fobj.read(1))
        fobj.read(4)
        ans["FollowingClipInformationFileName"] = fobj.read(5).decode("utf-8")
        ans["FollowingClipCodecIdentifier"] = fobj.read(1)
        ans["FollowingClipCodecIdentifier"] = ans["FollowingClipCodecIdentifier"].decode("utf-8")
        fobj.read(1)

    # Pad out the read ...
    BytesEnd = fobj.tell()
    BytesPassed = BytesEnd - BytesStart
    if BytesPassed < ans["Length"]:
        l = ans["Length"] - BytesPassed
        fobj.read(l)
    elif BytesPassed > ans["Length"]:
        print("load_ClipInfo: incorrect length")

    # Return answer ...
    return ans
