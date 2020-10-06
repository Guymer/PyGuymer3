def load_ClipMark(fobj):
    # NOTE: see https://github.com/lw/BluRay/wiki/ClipMark

    # Import standard modules ...
    import struct

    # Initialize variables ...
    ans = {}

    # Read the binary data ...
    ans["Length"], = struct.unpack(">I", fobj.read(4))
    bytesStart = fobj.tell()

    if ans["Length"] != 0:
        pass

    # Pad out the read ...
    bytesEnd = fobj.tell()
    bytesPassed = bytesEnd - bytesStart
    if bytesPassed < ans["Length"]:
        l = ans["Length"] - bytesPassed
        fobj.read(l)
        print("load_ClipInfo: skip %d bytes" % l)
    elif bytesPassed > ans["Length"]:
        print("load_ClipInfo: incorrect length")

    # Return answer ...
    return ans
