def load_StreamEntry(fobj, length2, length2a, length2b):
    # NOTE: see https://github.com/lerks/BluRay/wiki/StreamEntry

    # Import standard modules ...
    import struct

    # Initialize variables ...
    ans = {}
    length2c = 0                                                                                                        # [B]

    # Read the binary data ...
    ans = {}
    ans["Length"], = struct.unpack(">B", fobj.read(1));                                                                 length2 += 1; length2a += 1; length2b += 1
    if ans["Length"] != 0:
        ans["StreamType"], = struct.unpack(">B", fobj.read(1));                                                         length2 += 1; length2a += 1; length2b += 1; length2c += 1
        if ans["StreamType"] == int(0x01):
            tmp, = struct.unpack(">H", fobj.read(2));                                                                   length2 += 2; length2a += 2; length2b += 2; length2c += 2
            ans["RefToStreamPID"] = "0x{0:<04x}".format(tmp)
        if ans["StreamType"] == int(0x02):
            ans["RefToSubPathID"], = struct.unpack(">B", fobj.read(1));                                                 length2 += 1; length2a += 1; length2b += 1; length2c += 1
            ans["RefToSubClipID"], = struct.unpack(">B", fobj.read(1));                                                 length2 += 1; length2a += 1; length2b += 1; length2c += 1
            tmp, = struct.unpack(">H", fobj.read(2));                                                                   length2 += 2; length2a += 2; length2b += 2; length2c += 2
            ans["RefToStreamPID"] = "0x{0:<04x}".format(tmp)
        if ans["StreamType"] == int(0x03):
            tmp, = struct.unpack(">H", fobj.read(2));                                                                   length2 += 2; length2a += 2; length2b += 2; length2c += 2
            ans["RefToStreamPID"] = "0x{0:<04x}".format(tmp)
        if ans["StreamType"] == int(0x04):
            ans["RefToSubPathID"], = struct.unpack(">B", fobj.read(1));                                                 length2 += 1; length2a += 1; length2b += 1; length2c += 1
            ans["RefToSubClipID"], = struct.unpack(">B", fobj.read(1));                                                 length2 += 1; length2a += 1; length2b += 1; length2c += 1
            tmp, = struct.unpack(">H", fobj.read(2));                                                                   length2 += 2; length2a += 2; length2b += 2; length2c += 2
            ans["RefToStreamPID"] = "0x{0:<04x}".format(tmp)

        # Pad out the read ...
        if length2c != ans["Length"]:
            l = ans["Length"] - length2c                                                                                # [B]
            fobj.read(l);                                                                                               length2 += l; length2a += l; length2b += l; length2c += l

    # Return answer ...
    return ans, length2, length2a, length2b, length2c
