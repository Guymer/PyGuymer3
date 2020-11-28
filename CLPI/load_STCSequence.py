def load_STCSequence(fobj):
    # NOTE: see https://github.com/lw/BluRay/wiki/SequenceInfo

    # Import standard modules ...
    import struct

    # Initialize variables ...
    ans = {}

    # Read the binary data ...
    ans["PCRPID"], = struct.unpack(">H", fobj.read(2))
    ans["SPNSTCStart"] = struct.unpack(">I", fobj.read(4))
    ans["PresentationStartTime"] = struct.unpack(">I", fobj.read(4))
    ans["PresentationEndTime"], = struct.unpack(">I", fobj.read(4))

    # Return answer ...
    return ans