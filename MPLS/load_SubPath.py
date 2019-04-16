def load_SubPath(fobj, length2):
    # NOTE: see https://github.com/lerks/BluRay/wiki/SubPath

    # Import modules ...
    import struct

    # Load sub-functions ...
    from .load_SubPlayItem import load_SubPlayItem

    # Initialize variables ...
    ans = {}
    length2a = 0                                                                                                        # [B]

    # Read the binary data ...
    ans["Length"], = struct.unpack(">I", fobj.read(4));                                                               length2 += 4
    fobj.read(1);                                                                                                       length2 += 1; length2a += 1
    ans["SubPathType"], = struct.unpack(">B", fobj.read(1));                                                          length2 += 1; length2a += 1
    ans["MiscFlags1"], = struct.unpack(">H", fobj.read(2));                                                           length2 += 2; length2a += 2
    ans["NumberOfSubPlayItems"], = struct.unpack(">B", fobj.read(1));                                                 length2 += 1; length2a += 1
    ans["SubPlayItems"] = []
    for i in range(ans["NumberOfSubPlayItems"]):
        res, length2, length2a, length2b = load_SubPlayItem(fobj, length2, length2a)
        ans["SubPlayItems"].append(res)

    # Pad out the read ...
    if length2a != ans["Length"]:
        l = ans["Length"] - length2a                                                                                   # [B]
        fobj.read(l);                                                                                                   length2 += l; length2a += l

    return ans, length2, length2a
