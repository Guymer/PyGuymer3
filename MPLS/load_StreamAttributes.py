def load_StreamAttributes(fobj, debug = False, indent = 0):
    # NOTE: see https://github.com/lw/BluRay/wiki/StreamAttributes

    # Import standard modules ...
    import struct

    # Initialize answer and find it current position ...
    ans = {}
    pos = fobj.tell()                                                           # [B]
    if debug:
        print("DEBUG:{:s} {:s}() called at {:,d} bytes".format(indent * "  ", __name__, pos))

    # Read the binary data ...
    ans["Length"], = struct.unpack(">B", fobj.read(1))                          # [B]
    ans["StreamCodingType"], = struct.unpack(">B", fobj.read(1))
    if ans["StreamCodingType"] in [int(0x01), int(0x02), int(0x1B), int(0xEA)]:
        ans["VideoFormat+FrameRate"], = struct.unpack(">B", fobj.read(1))
    elif ans["StreamCodingType"] in [int(0x24)]:
        ans["VideoFormat+FrameRate"], = struct.unpack(">B", fobj.read(1))
        ans["DynamicRangeType+ColorSpace"], = struct.unpack(">B", fobj.read(1))
        ans["MiscFlags1"], = struct.unpack(">B", fobj.read(1))
    elif ans["StreamCodingType"] in [int(0x03), int(0x04), int(0x80), int(0x81), int(0x82), int(0x83), int(0x84), int(0x85), int(0x86), int(0xA1), int(0xA2)]:
        ans["AudioFormat+SampleRate"], = struct.unpack(">B", fobj.read(1))
        ans["LanguageCode"] = fobj.read(3).decode("utf-8")
    elif ans["StreamCodingType"] in [int(0x90), int(0x91)]:
        ans["LanguageCode"] = fobj.read(3).decode("utf-8")
    elif ans["StreamCodingType"] in [int(0x92)]:
        ans["CharacterCode"] = struct.unpack(">B", fobj.read(1))
        # NOTE: see https://github.com/lw/BluRay/wiki/StreamAttributes#charactercode
        if ans["CharacterCode"] in [int(0x01)]:
            ans["LanguageCode"] = fobj.read(3).decode("utf-8")
        elif ans["CharacterCode"] in [int(0x02)]:
            ans["LanguageCode"] = fobj.read(3).decode("utf_16_be")
        elif ans["CharacterCode"] in [int(0x03)]:
            ans["LanguageCode"] = fobj.read(3).decode("shift_jis")
        elif ans["CharacterCode"] in [int(0x04)]:
            ans["LanguageCode"] = fobj.read(3).decode("euc_kr")
        elif ans["CharacterCode"] in [int(0x05)]:
            ans["LanguageCode"] = fobj.read(3).decode("gb18030")
        elif ans["CharacterCode"] in [int(0x06)]:
            ans["LanguageCode"] = fobj.read(3).decode("gb2312")
        elif ans["CharacterCode"] in [int(0x07)]:
            ans["LanguageCode"] = fobj.read(3).decode("big5")
        else:
            ans["LanguageCode"] = fobj.read(3)

    # Skip ahead to the end of the data structure ...
    fobj.seek(pos + ans["Length"] + 1)

    # Return answer ...
    return ans
