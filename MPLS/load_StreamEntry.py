def load_StreamEntry(fobj, debug = False, indent = 0):
    # NOTE: see https://github.com/lw/BluRay/wiki/StreamEntry

    # Import standard modules ...
    import struct

    # Initialize answer and find it current position ...
    ans = {}
    pos = fobj.tell()                                                           # [B]
    if debug:
        print("DEBUG:{:s} {:s}() called at {:,d} bytes".format(indent * "  ", __name__, pos))

    # Read the binary data ...
    ans = {}
    ans["Length"], = struct.unpack(">B", fobj.read(1))                          # [B]
    ans["StreamType"], = struct.unpack(">B", fobj.read(1))
    if ans["StreamType"] in [int(0x01)]:
        tmp, = struct.unpack(">H", fobj.read(2))
        ans["RefToStreamPID"] = "0x{:<04x}".format(tmp)
    elif ans["StreamType"] in [int(0x02)]:
        ans["RefToSubPathID"], = struct.unpack(">B", fobj.read(1))
        ans["RefToSubClipID"], = struct.unpack(">B", fobj.read(1))
        tmp, = struct.unpack(">H", fobj.read(2))
        ans["RefToStreamPID"] = "0x{:<04x}".format(tmp)
    elif ans["StreamType"] in [int(0x03), int(0x04)]:
        ans["RefToSubPathID"], = struct.unpack(">B", fobj.read(1))
        tmp, = struct.unpack(">H", fobj.read(2))
        ans["RefToStreamPID"] = "0x{:<04x}".format(tmp)

    # Skip ahead to the end of the data structure ...
    fobj.seek(pos + ans["Length"] + 1)

    # Return answer ...
    return ans
