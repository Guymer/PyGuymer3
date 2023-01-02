def load_ClipMark(fObj):
    # NOTE: see https://github.com/lw/BluRay/wiki/ClipMark

    # Import standard modules ...
    import struct

    # Initialize variables ...
    ans = {}

    # Read the binary data ...
    ans["Length"], = struct.unpack(">I", fObj.read(4))
    bytesStart = fObj.tell()

    if ans["Length"] != 0:
        pass

    # Pad out the read ...
    bytesEnd = fObj.tell()
    bytesPassed = bytesEnd - bytesStart
    if bytesPassed < ans["Length"]:
        l = ans["Length"] - bytesPassed
        fObj.read(l)
        print("load_ClipInfo: skip %d bytes" % l)
    elif bytesPassed > ans["Length"]:
        print("load_ClipInfo: incorrect length")

    # Return answer ...
    return ans
