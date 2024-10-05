#!/usr/bin/env python3

# Define function ...
def load_StreamCodingInfo(
    fObj,
    /,
):
    # NOTE: See https://github.com/lw/BluRay/wiki/StreamCodingInfo

    # Import standard modules ...
    import struct

    # Initialize variables ...
    ans = {}

    # Read the binary data ...
    ans["Length"], = struct.unpack(">B", fObj.read(1))
    BytesStart = fObj.tell()

    # Read the binary data ...
    ans["StreamCodingType"], = struct.unpack(">B", fObj.read(1))
    if ans["StreamCodingType"] in {0x02, 0x1B, 0xEA}:
        tmp_VideoFormat_FrameRate, = struct.unpack(">B", fObj.read(1))
        ans["VideoFormat"] = (tmp_VideoFormat_FrameRate >> 4) & 0xF
        ans["FrameRate"] = tmp_VideoFormat_FrameRate & 0xF
        tmp_VideoAspect_reserved_OCFlag_reserved, = struct.unpack(">B", fObj.read(1))
        ans["VideoAspect"] = (tmp_VideoAspect_reserved_OCFlag_reserved >> 4) & 0xF
        ans["OCFlag"] = (tmp_VideoAspect_reserved_OCFlag_reserved >> 1) & 0x1
    elif ans["StreamCodingType"] in {0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0xA1, 0xA2}:
        tmp_AudioFormat_SampleRate, = struct.unpack(">B", fObj.read(1))
        ans["AudioFormat"] = (tmp_AudioFormat_SampleRate >> 4) & 0xF
        ans["SampleRate"] = tmp_AudioFormat_SampleRate & 0xF
        ans["Language"] = fObj.read(3).decode("utf-8")
    elif ans["StreamCodingType"] in {0x90, 0x91}:
        ans["Language"] = fObj.read(3).decode("utf-8")
    elif ans["StreamCodingType"] in {0x92}:
        ans["CharCode"], = struct.unpack(">B", fObj.read(1))
        ans["Language"] = fObj.read(3).decode("utf-8")

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
