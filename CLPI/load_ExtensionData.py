def load_ExtensionData(fobj):
    # NOTE: see https://github.com/lw/BluRay/wiki/ExtensionData

    # Import standard modules ...
    import struct

    # Initialize variables ...
    ans = dict()

    # Read the binary data ...
    ans["Length"], = struct.unpack(">I", fobj.read(4))
    BytesStart = fobj.tell()

    if ans["Length"] != 0:
        ans["DataBlockStartAddress"], = struct.unpack(">I", fobj.read(4))
        fobj.read(3)
        ans["NumberOfExtDataEntries"], = struct.unpack(">B", fobj.read(1))
        ans["ExtDataEntries"] = list()
        for i in range(ans["NumberOfExtDataEntries"]):
            tmp = dict()
            tmp["ExtDataType"], = struct.unpack(">H", fobj.read(2))
            tmp["ExtDataVersion"], = struct.unpack(">H", fobj.read(2))
            tmp["ExtDataStartAddress"], = struct.unpack(">I", fobj.read(4))
            tmp["ExtDataLength"], = struct.unpack(">I", fobj.read(4))
            ans["ExtDataEntries"].append(tmp)

        # NOTE: ExtDataEntries is not implemented

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
