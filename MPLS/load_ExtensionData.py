def load_ExtensionData(fobj, debug = False, errors = "strict", indent = 0):
    # NOTE: see https://github.com/lw/BluRay/wiki/ExtensionData

    # Import standard modules ...
    import struct

    # Initialize answer and find it current position ...
    ans = {}
    pos = fobj.tell()                                                           # [B]
    if debug:
        print("DEBUG:{:s} {:s}() called at {:,d} bytes".format(indent * "  ", __name__, pos), end = "")

    # Read the binary data ...
    ans["Length"], = struct.unpack(">I", fobj.read(4))                          # [B]
    if debug:
        print(" and is {:,d} bytes long".format(ans["Length"] + 4))
    if ans["Length"] != 0:
        ans["DataBlockStartAddress"], = struct.unpack(">I", fobj.read(4))
        fobj.read(3)
        ans["NumberOfExtDataEntries"], = struct.unpack(">B", fobj.read(1))
        ans["ExtDataEntries"] = []
        for i in range(ans["NumberOfExtDataEntries"]):
            tmp = {}
            tmp["ExtDataType"], = struct.unpack(">H", fobj.read(2))
            tmp["ExtDataVersion"], = struct.unpack(">H", fobj.read(2))
            tmp["ExtDataStartAddress"], = struct.unpack(">I", fobj.read(4))
            tmp["ExtDataLength"], = struct.unpack(">I", fobj.read(4))
            ans["ExtDataEntries"].append(tmp)

        # NOTE: ExtDataEntries is not implemented

    # Skip ahead to the end of the data structure ...
    fobj.seek(pos + ans["Length"] + 4)

    # Return answer ...
    return ans