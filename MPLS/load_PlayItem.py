def load_PlayItem(fobj, length2):
    # NOTE: see https://github.com/lerks/BluRay/wiki/PlayItem

    # Import modules ...
    import struct

    # Load sub-functions ...
    from .load_STNTable import load_STNTable

    # Initialize variables ...
    ans = {}
    length2a = 0                                                                                                        # [B]

    # Read the binary data ...
    ans["Length"], = struct.unpack(">H", fobj.read(2));                                                                 length2 += 2
    ans["ClipInformationFileName"] = fobj.read(5).decode("utf-8");                                                      length2 += 5; length2a += 5
    ans["ClipCodecIdentifier"] = fobj.read(4).decode("utf-8");                                                          length2 += 4; length2a += 4
    ans["MiscFlags1"], = struct.unpack(">H", fobj.read(2));                                                             length2 += 2; length2a += 2
    ans["IsMultiAngle"] = bool(ans["MiscFlags1"]&(1<<11))
    ans["RefToSTCID"], = struct.unpack(">B", fobj.read(1));                                                             length2 += 1; length2a += 1
    ans["INTime"], = struct.unpack(">I", fobj.read(4));                                                                 length2 += 4; length2a += 4
    ans["OUTTime"], = struct.unpack(">I", fobj.read(4));                                                                length2 += 4; length2a += 4
    ans["UOMaskTable"], = struct.unpack(">Q", fobj.read(8));                                                            length2 += 8; length2a += 8
    ans["MiscFlags2"], = struct.unpack(">B", fobj.read(1));                                                             length2 += 1; length2a += 1
    ans["StillMode"], = struct.unpack(">B", fobj.read(1));                                                              length2 += 1; length2a += 1
    if ans["StillMode"] == int(0x01):
        ans["StillTime"], = struct.unpack(">H", fobj.read(2));                                                          length2 += 2; length2a += 2
    else:
        fobj.read(2);                                                                                                   length2 += 2; length2a += 2
    if ans["IsMultiAngle"]:
        raise Exception("IsMultiAngle has not been implemented as the specification is not byte-aligned (IsDifferentAudios is 6-bit and IsSeamlessAngleChange is 1-bit)")

    # Load STNTable section ...
    res, length2, length2a, length2b = load_STNTable(fobj, length2, length2a)
    ans["STNTable"] = res

    # Pad out the read ...
    if length2a != ans["Length"]:
        l = ans["Length"] - length2a                                                                                    # [B]
        fobj.read(l);                                                                                                   length2 += l; length2a += l

    # Return answer ...
    return ans, length2, length2a
