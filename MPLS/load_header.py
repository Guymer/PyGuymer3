def load_header(fobj, debug = False, indent = 0):
    # NOTE: see https://github.com/lw/BluRay/wiki/MPLS

    # Import standard modules ...
    import struct

    # Initialize answer and find it current position ...
    ans = {}
    pos = fobj.tell()                                                           # [B]
    if debug:
        print("DEBUG:{:s} {:s}() called at {:,d} bytes".format(indent * "  ", __name__, pos))

    # Check everything is going to be OK ...
    if pos != 0:
        raise Exception("\"load_header()\" should only be called at the start of the MPLS file")

    # Read the binary data ...
    ans["TypeIndicator"] = fobj.read(4).decode("utf-8")
    ans["VersionNumber"] = fobj.read(4).decode("utf-8")
    ans["PlayListStartAddress"], = struct.unpack(">I", fobj.read(4))
    ans["PlayListMarkStartAddress"], = struct.unpack(">I", fobj.read(4))
    ans["ExtensionDataStartAddress"], = struct.unpack(">I", fobj.read(4))
    fobj.read(20)

    # Return answer ...
    return ans
