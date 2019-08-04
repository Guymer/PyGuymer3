def load_AppInfoPlayList(fobj):
    # NOTE: see https://github.com/lerks/BluRay/wiki/AppInfoPlayList

    # Import modules ...
    import struct

    # Initialize variables ...
    ans = {}
    length1 = 0                                                                                                         # [B]

    # Read the binary data ...
    ans["Length"], = struct.unpack(">I", fobj.read(4))
    fobj.read(1);                                                                                                       length1 += 1
    ans["PlaybackType"], = struct.unpack(">B", fobj.read(1));                                                           length1 += 1
    if ans["PlaybackType"] == int(0x02) or ans["PlaybackType"] == int(0x03):
        ans["PlaybackCount"], = struct.unpack(">H", fobj.read(2));                                                      length1 += 2
    else:
        fobj.read(2);                                                                                                   length1 += 2
    ans["UOMaskTable"], = struct.unpack(">Q", fobj.read(8));                                                            length1 += 8
    ans["MiscFlags"], = struct.unpack(">H", fobj.read(2));                                                              length1 += 2

    # Pad out the read ...
    if length1 != ans["Length"]:
        l = ans["Length"] - length1                                                                                     # [B]
        fobj.read(l);                                                                                                   length1 += l

    # Return answer ...
    return ans, length1
