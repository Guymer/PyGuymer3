def load_StreamAttributes(fobj, length2, length2a, length2b):
    # NOTE: see https://github.com/lerks/BluRay/wiki/StreamAttributes

    # Import standard modules ...
    import struct

    # Initialize variables ...
    ans = {}
    length2c = 0                                                                                                        # [B]

    # Read the binary data ...
    ans["Length"], = struct.unpack(">B", fobj.read(1));                                                                 length2 += 1; length2a += 1; length2b += 1
    if ans["Length"] != 0:
        ans["StreamCodingType"], = struct.unpack(">B", fobj.read(1));                                                   length2 += 1; length2a += 1; length2b += 1; length2c += 1
        if ans["StreamCodingType"] in [int(0x02), int(0x1B), int(0xEA)]:
            ans["VideoFormat+FrameRate"], = struct.unpack(">B", fobj.read(1));                                          length2 += 1; length2a += 1; length2b += 1; length2c += 1
        if ans["StreamCodingType"] in [int(0x80), int(0x81), int(0x82), int(0x83), int(0x84), int(0x85), int(0x86), int(0xA1), int(0xA2)]:
            ans["AudioFormat+SampleRate"], = struct.unpack(">B", fobj.read(1));                                         length2 += 1; length2a += 1; length2b += 1; length2c += 1
            ans["LanguageCode"] = fobj.read(3).decode("utf-8");                                                         length2 += 3; length2a += 3; length2b += 3; length2c += 3
        if ans["StreamCodingType"] in [int(0x90), int(0x91)]:
            ans["LanguageCode"] = fobj.read(3).decode("utf-8");                                                         length2 += 3; length2a += 3; length2b += 3; length2c += 3
        if ans["StreamCodingType"] in [int(0x92)]:
            ans["CharacterCode"] = fobj.read(1).decode("utf-8");                                                        length2 += 1; length2a += 1; length2b += 1; length2c += 1
            ans["LanguageCode"] = fobj.read(3).decode("utf-8");                                                         length2 += 3; length2a += 3; length2b += 3; length2c += 3

        # Pad out the read ...
        if length2c != ans["Length"]:
            l = ans["Length"] - length2c                                                                                # [B]
            fobj.read(l);                                                                                               length2 += l; length2a += l; length2b += l; length2c += l

    # Return answer ...
    return ans, length2, length2a, length2b, length2c
