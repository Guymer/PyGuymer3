def load_ATCSequence(fobj):
    # NOTE: see https://github.com/lerks/BluRay/wiki/SequenceInfo

    # Import modules ...
    import struct

    # Load sub-functions ...
    from .load_STCSequence import load_STCSequence

    # Initialize variables ...
    ans = dict()

    # Read the binary data ...
    ans["SPNATCStart"], = struct.unpack(">I", fobj.read(4))
    ans["NumberOfSTCSequences"], = struct.unpack(">B", fobj.read(1))
    ans["OffsetSTCID"], = struct.unpack(">B", fobj.read(1))

    # print("\tBefore load STNTable: ", fobj.tell() * 8, "剩余长度：", (7578 - fobj.tell()) * 8)
    # Load STCSequences section ...
    ans["STCSequences"] = list()
    for i in range(ans["NumberOfSTCSequences"]):
        # Load STCSequence section and append to STCSequences list ...
        res = load_STCSequence(fobj)
        ans["STCSequences"].append(res)

    # Return answer ...
    return ans
