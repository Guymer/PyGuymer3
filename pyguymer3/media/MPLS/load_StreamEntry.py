#!/usr/bin/env python3

# Define function ...
def load_StreamEntry(fObj):
    # NOTE: See https://github.com/lw/BluRay/wiki/StreamEntry

    # Import standard modules ...
    import struct

    # Initialize answer and find the current position ...
    ans = {}
    pos = fObj.tell()                                                           # [B]

    # Read the binary data ...
    ans = {}
    ans["Length"], = struct.unpack(">B", fObj.read(1))                          # [B]
    if ans["Length"] != 0:
        ans["StreamType"], = struct.unpack(">B", fObj.read(1))
        if ans["StreamType"] in [int(0x01)]:
            tmp, = struct.unpack(">H", fObj.read(2))
            ans["RefToStreamPID"] = f"0x{tmp:<04x}"
        elif ans["StreamType"] in [int(0x02)]:
            ans["RefToSubPathID"], = struct.unpack(">B", fObj.read(1))
            ans["RefToSubClipID"], = struct.unpack(">B", fObj.read(1))
            tmp, = struct.unpack(">H", fObj.read(2))
            ans["RefToStreamPID"] = f"0x{tmp:<04x}"
        elif ans["StreamType"] in [int(0x03), int(0x04)]:
            ans["RefToSubPathID"], = struct.unpack(">B", fObj.read(1))
            tmp, = struct.unpack(">H", fObj.read(2))
            ans["RefToStreamPID"] = f"0x{tmp:<04x}"
        else:
            print("WARNING: \"StreamType\" was not a recognised value", ans["StreamType"])

    # Skip ahead to the end of the data structure ...
    fObj.seek(pos + ans["Length"] + 1)

    # Return answer ...
    return ans
