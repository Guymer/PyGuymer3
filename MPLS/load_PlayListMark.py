def load_PlayListMark(fobj):
    # NOTE: see https://github.com/lerks/BluRay/wiki/PlayListMark

    # Import modules ...
    import struct

    # Initialize variables ...
    ans = {}
    length3 = 0                                                                                                         # [B]

    # Read the binary data ...
    ans["Length"], = struct.unpack(">I", fobj.read(4))
    ans["NumberOfPlayListMarks"], = struct.unpack(">H", fobj.read(2));                                                length3 += 2
    ans["PlayListMarks"] = []
    for i in range(ans["NumberOfPlayListMarks"]):
        tmp = {}
        fobj.read(1);                                                                                                   length3 += 1
        tmp["MarkType"], = struct.unpack(">B", fobj.read(1));                                                         length3 += 1
        tmp["RefToPlayItemID"], = struct.unpack(">H", fobj.read(2));                                                  length3 += 2
        tmp["MarkTimeStamp"], = struct.unpack(">I", fobj.read(4));                                                    length3 += 4
        tmp["EntryESPID"], = struct.unpack(">H", fobj.read(2));                                                       length3 += 2
        tmp["Duration"], = struct.unpack(">I", fobj.read(4));                                                         length3 += 4
        ans["PlayListMarks"].append(tmp)

    # Pad out the read ...
    if length3 != ans["Length"]:
        l = ans["Length"] - length3                                                                                    # [B]
        fobj.read(l);                                                                                                   length3 += l

    # Return answer ...
    return ans, length3
