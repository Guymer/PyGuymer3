def load_StreamCodingInfo(fobj):
    # NOTE: see https://github.com/lw/BluRay/wiki/StreamCodingInfo

    # Import standard modules ...
    import struct

    # Initialize variables ...
    ans = {}

    # Read the binary data ...
    ans["Length"], = struct.unpack(">B", fobj.read(1))
    BytesStart = fobj.tell()

    ans["StreamCodingType"], = struct.unpack(">B", fobj.read(1))
    if ans["StreamCodingType"] in {0x02, 0x1B, 0xEA}:
        # VideoFormat 4 Bits, FrameRate 4 Bits
        tmp_VideoFormat_FrameRate, = struct.unpack(">B", fobj.read(1))
        ans["VideoFormat"] = (tmp_VideoFormat_FrameRate >> 4) & 0xF
        ans["FrameRate"] = tmp_VideoFormat_FrameRate & 0xF
        # VideoAspect 4 Bits, reserved 2 Bits, OCFlag 1 Bit, reserved 1 Bit
        tmp_VideoAspect_reserved_OCFlag_reserved, = struct.unpack(">B", fobj.read(1))
        ans["VideoAspect"] = (tmp_VideoAspect_reserved_OCFlag_reserved >> 4) & 0xF
        ans["OCFlag"] = (tmp_VideoAspect_reserved_OCFlag_reserved >> 1) & 0x1
    elif ans["StreamCodingType"] in {0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0xA1, 0xA2}:
        # AudioFormat 4 Bits, SampleRate 4 Bits
        tmp_AudioFormat_SampleRate, = struct.unpack(">B", fobj.read(1))
        ans["AudioFormat"] = (tmp_AudioFormat_SampleRate >> 4) & 0xF
        ans["SampleRate"] = tmp_AudioFormat_SampleRate & 0xF
        ans["Language"] = fobj.read(3).decode("utf-8")
    elif ans["StreamCodingType"] in {0x90, 0x91}:
        ans["Language"] = fobj.read(3).decode("utf-8")
    elif ans["StreamCodingType"] in {0x92}:
        ans["CharCode"], = struct.unpack(">B", fobj.read(1))
        ans["Language"] = fobj.read(3).decode("utf-8")

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