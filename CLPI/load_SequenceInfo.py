def load_SequenceInfo(fobj):
    # NOTE: see https://github.com/lerks/BluRay/wiki/SequenceInfo

    # Import modules ...
    import struct

    # Load sub-functions ...
    from .load_ATCSequence import load_ATCSequence

    # Initialize variables ...
    ans = dict()

    # Read the binary data ...
    ans["Length"], = struct.unpack(">I", fobj.read(4))
    BytesStart = fobj.tell()

    fobj.read(1)
    ans["NumberOfATCSequences"], = struct.unpack(">B", fobj.read(1))

    # Loop over PlayItems ...
    ans["ATCSequences"] = []
    for i in range(ans["NumberOfATCSequences"]):
        # print('PL' + str(i) + ': ' + str((7578 - fobj.tell()) * 8))
        # Load ATCSequence section and append to ATCSequences list ...
        res = load_ATCSequence(fobj)
        ans["ATCSequences"].append(res)
        # # print(i, fobj.tell() * 8, "剩余长度：", (7578 - fobj.tell()) * 8)

    # Pad out the read ...
    BytesEnd = fobj.tell()
    BytesPassed = BytesEnd - BytesStart
    if BytesPassed < ans["Length"]:
        l = ans["Length"] - BytesPassed
        fobj.read(l);
        print("load_SequenceInfo: skip %d bytes" % l)
    elif BytesPassed > ans["Length"]:
        print("load_SequenceInfo: incorrect length")

    # Return answer ...
    return ans
