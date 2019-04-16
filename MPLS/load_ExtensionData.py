def load_ExtensionData(fobj):
    # NOTE: see https://github.com/lerks/BluRay/wiki/ExtensionData

    # Import modules ...
    import struct

    # Initialize variables ...
    ans = {}
    length4 = 0                                                                                                         # [B]

    # Read the binary data ...
    ans["Length"], = struct.unpack(">I", fobj.read(4))
    if ans["Length"] != 0:
        ans["DataBlockStartAddress"], = struct.unpack(">I", fobj.read(4));                                            length4 += 4
        fobj.read(3);                                                                                                   length4 += 4
        ans["NumberOfExtDataEntries"], = struct.unpack(">B", fobj.read(1));                                           length4 += 1
        ans["ExtDataEntries"] = []
        for i in range(ans["NumberOfExtDataEntries"]):
            tmp = {}
            tmp["ExtDataType"], = struct.unpack(">H", fobj.read(2));                                                  length4 += 2
            tmp["ExtDataVersion"], = struct.unpack(">H", fobj.read(2));                                               length4 += 2
            tmp["ExtDataStartAddress"], = struct.unpack(">I", fobj.read(4));                                          length4 += 4
            tmp["ExtDataLength"], = struct.unpack(">I", fobj.read(4));                                                length4 += 4
            ans["ExtDataEntries"].append(tmp)

        # NOTE: ExtDataEntries is not implemented

        # Pad out the read ...
        if length4 != ans["Length"]:
            l = ans["Length"] - length4                                                                                # [B]
            fobj.read(l);                                                                                               length4 += l

    # Return answer ...
    return ans, length4
