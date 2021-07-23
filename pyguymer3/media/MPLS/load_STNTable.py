def load_STNTable(fobj, kwArgCheck = None, debug = False, errors = "strict", indent = 0):
    # NOTE: see https://github.com/lw/BluRay/wiki/STNTable

    # Import standard modules ...
    import struct

    # Import sub-functions ...
    from .load_StreamAttributes import load_StreamAttributes
    from .load_StreamEntry import load_StreamEntry

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Initialize answer and find it current position ...
    ans = {}
    pos = fobj.tell()                                                           # [B]
    if debug:
        print("DEBUG:{:s} {:s}() called at {:,d} bytes".format(indent * "  ", __name__, pos), end = "")

    # Read the binary data ...
    ans["Length"], = struct.unpack(">H", fobj.read(2))                          # [B]
    if debug:
        print(" and is {:,d} bytes long".format(ans["Length"] + 2))
    if ans["Length"] != 0:
        fobj.read(2)
        ans["NumberOfPrimaryVideoStreamEntries"], = struct.unpack(">B", fobj.read(1))
        ans["NumberOfPrimaryAudioStreamEntries"], = struct.unpack(">B", fobj.read(1))
        ans["NumberOfPrimaryPGStreamEntries"], = struct.unpack(">B", fobj.read(1))
        ans["NumberOfPrimaryIGStreamEntries"], = struct.unpack(">B", fobj.read(1))
        ans["NumberOfSecondaryAudioStreamEntries"], = struct.unpack(">B", fobj.read(1))
        ans["NumberOfSecondaryVideoStreamEntries"], = struct.unpack(">B", fobj.read(1))
        ans["NumberOfSecondaryPGStreamEntries"], = struct.unpack(">B", fobj.read(1))
        ans["NumberOfDVStreamEntries"], = struct.unpack(">B", fobj.read(1))
        fobj.read(4)

        # Loop over stream list names ...
        for name in ["PrimaryVideoStreamEntries", "PrimaryAudioStreamEntries", "PrimaryPGStreamEntries", "SecondaryPGStreamEntries", "PrimaryIGStreamEntries", "SecondaryAudioStreamEntries", "SecondaryVideoStreamEntries", "DVStreamEntries"]:
            # Loop over entries and add to list ...
            ans[name] = []
            for i in range(ans["NumberOf{:s}".format(name)]):
                tmp = {}
                tmp["StreamEntry"] = load_StreamEntry(fobj, debug = debug, errors = errors, indent = indent + 1)
                tmp["StreamAttributes"] = load_StreamAttributes(fobj, debug = debug, errors = errors, indent = indent + 1)
                ans[name].append(tmp)

    # Skip ahead to the end of the data structure ...
    fobj.seek(pos + ans["Length"] + 2)

    # Return answer ...
    return ans
