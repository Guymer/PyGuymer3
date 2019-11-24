def load_TSTypeInfoBlock(fobj):
    # NOTE: see https://github.com/lerks/BluRay/blob/master/src/TSTypeInfoBlock.vala

    # Import modules ...
    import struct

    # Initialize variables ...
    ans = {}

    # Read the binary data ...
    ans["Length"], = struct.unpack(">H", fobj.read(2))

    BytesStart = fobj.tell()

    fobj.read(ans["Length"])

    BytesEnd = fobj.tell()
    BytesPassed = BytesEnd - BytesStart
    if BytesPassed < ans["Length"]:
        l = ans["Length"] - BytesPassed
        fobj.read(l)
        print("load_ClipInfo: skip %d bytes" % l)
    elif BytesPassed > ans["Length"]:
        print("load_ClipInfo: incorrect length")

    # Return answer ...
    return ans
