#!/usr/bin/env python3

# Define function ...
def load_StreamAttributes(fObj, kwArgCheck = None, debug = False, errors = "strict", indent = 0):
    # NOTE: see https://github.com/lw/BluRay/wiki/StreamAttributes

    # Import standard modules ...
    import struct

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Initialize answer and find it current position ...
    ans = {}
    pos = fObj.tell()                                                           # [B]
    if debug:
        print("DEBUG:{:s} {:s}() called at {:,d} bytes".format(indent * "  ", __name__, pos), end = "")

    # Read the binary data ...
    ans["Length"], = struct.unpack(">B", fObj.read(1))                          # [B]
    if debug:
        print(" and is {:,d} bytes long".format(ans["Length"] + 1))
    if ans["Length"] != 0:
        ans["StreamCodingType"], = struct.unpack(">B", fObj.read(1))
        if ans["StreamCodingType"] in [int(0x01), int(0x02), int(0x1B), int(0xEA)]:
            ans["VideoFormat+FrameRate"], = struct.unpack(">B", fObj.read(1))
        elif ans["StreamCodingType"] in [int(0x24)]:
            ans["VideoFormat+FrameRate"], = struct.unpack(">B", fObj.read(1))
            ans["DynamicRangeType+ColorSpace"], = struct.unpack(">B", fObj.read(1))
            ans["MiscFlags1"], = struct.unpack(">B", fObj.read(1))
        elif ans["StreamCodingType"] in [int(0x03), int(0x04), int(0x80), int(0x81), int(0x82), int(0x83), int(0x84), int(0x85), int(0x86), int(0xA1), int(0xA2)]:
            ans["AudioFormat+SampleRate"], = struct.unpack(">B", fObj.read(1))
            ans["LanguageCode"] = fObj.read(3).decode("utf-8", errors = errors)
        elif ans["StreamCodingType"] in [int(0x90), int(0x91)]:
            ans["LanguageCode"] = fObj.read(3).decode("utf-8", errors = errors)
        elif ans["StreamCodingType"] in [int(0x92)]:
            ans["CharacterCode"] = struct.unpack(">B", fObj.read(1))
            # NOTE: see https://github.com/lw/BluRay/wiki/StreamAttributes#charactercode
            if ans["CharacterCode"] in [int(0x01)]:
                ans["LanguageCode"] = fObj.read(3).decode("utf-8", errors = errors)
            elif ans["CharacterCode"] in [int(0x02)]:
                ans["LanguageCode"] = fObj.read(3).decode("utf_16_be", errors = errors)
            elif ans["CharacterCode"] in [int(0x03)]:
                ans["LanguageCode"] = fObj.read(3).decode("shift_jis", errors = errors)
            elif ans["CharacterCode"] in [int(0x04)]:
                ans["LanguageCode"] = fObj.read(3).decode("euc_kr", errors = errors)
            elif ans["CharacterCode"] in [int(0x05)]:
                ans["LanguageCode"] = fObj.read(3).decode("gb18030", errors = errors)
            elif ans["CharacterCode"] in [int(0x06)]:
                ans["LanguageCode"] = fObj.read(3).decode("gb2312", errors = errors)
            elif ans["CharacterCode"] in [int(0x07)]:
                ans["LanguageCode"] = fObj.read(3).decode("big5", errors = errors)
            else:
                print("WARNING: \"CharacterCode\" was not a recognised value", ans["CharacterCode"])
                ans["LanguageCode"] = fObj.read(3)
        else:
            print("WARNING: \"StreamCodingType\" was not a recognised value", ans["StreamCodingType"])

    # Skip ahead to the end of the data structure ...
    fObj.seek(pos + ans["Length"] + 1)

    # Return answer ...
    return ans
