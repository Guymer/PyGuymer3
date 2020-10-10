def load_PlayItem(fobj, debug = False, indent = 0):
    # NOTE: see https://github.com/lw/BluRay/wiki/PlayItem

    # Import standard modules ...
    import struct

    # Load sub-functions ...
    from .load_STNTable import load_STNTable

    # Initialize answer and find it current position ...
    ans = {}
    pos = fobj.tell()                                                           # [B]
    if debug:
        print("DEBUG:{:s} {:s}() called at {:,d} bytes".format(indent * "  ", __name__, pos))

    # Read the binary data ...
    ans["Length"], = struct.unpack(">H", fobj.read(2))                          # [B]
    ans["ClipInformationFileName"] = fobj.read(5).decode("utf-8")
    ans["ClipCodecIdentifier"] = fobj.read(4).decode("utf-8")
    ans["MiscFlags1"], = struct.unpack(">H", fobj.read(2))
    ans["IsMultiAngle"] = bool(ans["MiscFlags1"]&(1<<11))
    ans["RefToSTCID"], = struct.unpack(">B", fobj.read(1))
    ans["INTime"], = struct.unpack(">I", fobj.read(4))
    ans["OUTTime"], = struct.unpack(">I", fobj.read(4))
    ans["UOMaskTable"], = struct.unpack(">Q", fobj.read(8))
    ans["MiscFlags2"], = struct.unpack(">B", fobj.read(1))
    ans["StillMode"], = struct.unpack(">B", fobj.read(1))
    if ans["StillMode"] in [int(0x01)]:
        ans["StillTime"], = struct.unpack(">H", fobj.read(2))
    else:
        fobj.read(2)
    if ans["IsMultiAngle"]:
        ans["NumberOfAngles"], = struct.unpack(">B", fobj.read(1))
        ans["MiscFlags3"], = struct.unpack(">B", fobj.read(1))
        ans["Angles"] = []
        for i in range(ans["NumberOfAngles"]):
            tmp = {}
            tmp["ClipInformationFileName"] = fobj.read(5).decode("utf-8")
            tmp["ClipCodecIdentifier"] = fobj.read(4).decode("utf-8")
            tmp["RefToSTCID"], = struct.unpack(">B", fobj.read(1))
            ans["Angles"].append(tmp)

    # Load STNTable section ...
    ans["STNTable"] = load_STNTable(fobj, debug = debug, indent = indent + 1)

    # Skip ahead to the end of the data structure ...
    fobj.seek(pos + ans["Length"] + 2)

    # Return answer ...
    return ans
