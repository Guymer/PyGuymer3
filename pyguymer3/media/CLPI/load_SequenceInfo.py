def load_SequenceInfo(fobj):
    # NOTE: see https://github.com/lw/BluRay/wiki/SequenceInfo

    # Import standard modules ...
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
        # Load ATCSequence section and append to ATCSequences list ...
        res = load_ATCSequence(fobj)
        ans["ATCSequences"].append(res)

    # Pad out the read ...
    BytesEnd = fobj.tell()
    BytesPassed = BytesEnd - BytesStart
    if BytesPassed < ans["Length"]:
        l = ans["Length"] - BytesPassed
        fobj.read(l)
    elif BytesPassed > ans["Length"]:
        print("load_SequenceInfo: incorrect length")

    # Return answer ...
    return ans
