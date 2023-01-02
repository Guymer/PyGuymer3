def load_PlayListMark(fObj, kwArgCheck = None, debug = False, errors = "strict", indent = 0):
    # NOTE: see https://github.com/lw/BluRay/wiki/PlayListMark

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
    ans["Length"], = struct.unpack(">I", fObj.read(4))                          # [B]
    if debug:
        print(" and is {:,d} bytes long".format(ans["Length"] + 4))
    if ans["Length"] != 0:
        ans["NumberOfPlayListMarks"], = struct.unpack(">H", fObj.read(2))
        ans["PlayListMarks"] = []
        for i in range(ans["NumberOfPlayListMarks"]):
            tmp = {}
            fObj.read(1)
            tmp["MarkType"], = struct.unpack(">B", fObj.read(1))
            tmp["RefToPlayItemID"], = struct.unpack(">H", fObj.read(2))
            tmp["MarkTimeStamp"], = struct.unpack(">I", fObj.read(4))
            tmp["EntryESPID"], = struct.unpack(">H", fObj.read(2))
            tmp["Duration"], = struct.unpack(">I", fObj.read(4))
            ans["PlayListMarks"].append(tmp)

    # Skip ahead to the end of the data structure ...
    fObj.seek(pos + ans["Length"] + 4)

    # Return answer ...
    return ans
