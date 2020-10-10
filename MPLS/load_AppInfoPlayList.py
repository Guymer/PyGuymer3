def load_AppInfoPlayList(fobj, debug = False, indent = 0):
    # NOTE: see https://github.com/lw/BluRay/wiki/AppInfoPlayList

    # Import standard modules ...
    import struct

    # Initialize answer and find it current position ...
    ans = {}
    pos = fobj.tell()                                                           # [B]
    if debug:
        print("DEBUG:{:s} {:s}() called at {:,d} bytes".format(indent * "  ", __name__, pos), end = "")

    # Read the binary data ...
    ans["Length"], = struct.unpack(">I", fobj.read(4))                          # [B]
    if debug:
        print(" and is {:,d} bytes long".format(ans["Length"] + 4))
    fobj.read(1)
    ans["PlaybackType"], = struct.unpack(">B", fobj.read(1))
    if ans["PlaybackType"] in [int(0x02), int(0x03)]:
        ans["PlaybackCount"], = struct.unpack(">H", fobj.read(2))
    else:
        fobj.read(2)
    ans["UOMaskTable"], = struct.unpack(">Q", fobj.read(8))
    ans["MiscFlags"], = struct.unpack(">H", fobj.read(2))

    # Skip ahead to the end of the data structure ...
    fobj.seek(pos + ans["Length"] + 4)

    # Return answer ...
    return ans
