def load_header(fobj):
    # NOTE: see https://github.com/lerks/BluRay/wiki/CLPI

    # Import modules ...
    import struct

    # Initialize variables ...
    ans = dict()

    # Read the binary data ...
    ans["TypeIndicator"] = fobj.read(4).decode("utf-8")
    ans["VersionNumber"] = fobj.read(4).decode("utf-8")
    ans["SequenceInfoStartAddress"], = struct.unpack(">I", fobj.read(4))
    ans["ProgramInfoStartAddress"], = struct.unpack(">I", fobj.read(4))
    ans["CPIStartAddress"], = struct.unpack(">I", fobj.read(4))
    ans["ClipMarkStartAddress"], = struct.unpack(">I", fobj.read(4))
    ans["ExtensionDataStartAddress"], = struct.unpack(">I", fobj.read(4))
    fobj.read(12)

    # Return answer ...
    return ans
