def load_SubPath(fobj, debug = False, indent = 0):
    # NOTE: see https://github.com/lw/BluRay/wiki/SubPath

    # Import standard modules ...
    import struct

    # Load sub-functions ...
    from .load_SubPlayItem import load_SubPlayItem

    # Initialize answer and find it current position ...
    ans = {}
    pos = fobj.tell()                                                           # [B]
    if debug:
        print("DEBUG:{:s} {:s}() called at {:,d} bytes".format(indent * "  ", __name__, pos), end = "")

    # Read the binary data ...
    ans["Length"], = struct.unpack(">I", fobj.read(4))                          # [B]
    if debug:
        print(" and is {:,d} bytes long".format(ans["Length"] + 4))
    fobj.read(1)
    ans["SubPathType"], = struct.unpack(">B", fobj.read(1))
    ans["MiscFlags1"], = struct.unpack(">H", fobj.read(2))
    fobj.read(1)
    ans["NumberOfSubPlayItems"], = struct.unpack(">B", fobj.read(1))
    ans["SubPlayItems"] = []
    for i in range(ans["NumberOfSubPlayItems"]):
        ans["SubPlayItems"].append(load_SubPlayItem(fobj, debug = debug, indent = indent + 1))

    # Skip ahead to the end of the data structure ...
    fobj.seek(pos + ans["Length"] + 4)

    # Return answer ...
    return ans
