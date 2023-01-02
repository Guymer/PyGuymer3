def load_StreamEntry(fObj, kwArgCheck = None, debug = False, errors = "strict", indent = 0):
    # NOTE: see https://github.com/lw/BluRay/wiki/StreamEntry

    # Import standard modules ...
    import struct

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Initialize answer and find it current position ...
    ans = {}
    pos = fObj.tell()                                                           # [B]
    if debug:
        print("DEBUG:{:s} {:s}() called at {:,d} bytes".format(indent * "  ", __name__, pos), end = "")

    # Read the binary data ...
    ans = {}
    ans["Length"], = struct.unpack(">B", fObj.read(1))                          # [B]
    if debug:
        print(" and is {:,d} bytes long".format(ans["Length"] + 1))
    if ans["Length"] != 0:
        ans["StreamType"], = struct.unpack(">B", fObj.read(1))
        if ans["StreamType"] in [int(0x01)]:
            tmp, = struct.unpack(">H", fObj.read(2))
            ans["RefToStreamPID"] = "0x{:<04x}".format(tmp)
        elif ans["StreamType"] in [int(0x02)]:
            ans["RefToSubPathID"], = struct.unpack(">B", fObj.read(1))
            ans["RefToSubClipID"], = struct.unpack(">B", fObj.read(1))
            tmp, = struct.unpack(">H", fObj.read(2))
            ans["RefToStreamPID"] = "0x{:<04x}".format(tmp)
        elif ans["StreamType"] in [int(0x03), int(0x04)]:
            ans["RefToSubPathID"], = struct.unpack(">B", fObj.read(1))
            tmp, = struct.unpack(">H", fObj.read(2))
            ans["RefToStreamPID"] = "0x{:<04x}".format(tmp)
        else:
            print("WARNING: \"StreamType\" was not a recognised value", ans["StreamType"])

    # Skip ahead to the end of the data structure ...
    fObj.seek(pos + ans["Length"] + 1)

    # Return answer ...
    return ans
