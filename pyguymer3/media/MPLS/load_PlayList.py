def load_PlayList(fObj, kwArgCheck = None, debug = False, errors = "strict", indent = 0):
    # NOTE: see https://github.com/lw/BluRay/wiki/PlayList

    # Import standard modules ...
    import struct

    # Import sub-functions ...
    from .load_PlayItem import load_PlayItem
    from .load_SubPath import load_SubPath

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Initialize answer and find it current position ...
    ans = {}
    pos = fObj.tell()                                                           # [B]
    if debug:
        print("DEBUG:{:s} {:s}() called at {:,d} bytes".format(indent * "  ", __name__, pos), end = "")

    # Read the binary data ...
    ans["Length"], = struct.unpack(">I", fObj.read(4))                          # [B]
    if debug:
        print(" and is {:,d} bytes long".format(ans["Length"] + 4))
    if ans["Length"] != 0:
        fObj.read(2)
        ans["NumberOfPlayItems"], = struct.unpack(">H", fObj.read(2))
        ans["NumberOfSubPaths"], = struct.unpack(">H", fObj.read(2))

        # Loop over PlayItems ...
        ans["PlayItems"] = []
        for i in range(ans["NumberOfPlayItems"]):
            # Load PlayItem section and append to PlayItems list ...
            ans["PlayItems"].append(load_PlayItem(fObj, debug = debug, errors = errors, indent = indent + 1))

        # Loop over SubPaths ...
        ans["SubPaths"] = []
        for i in range(ans["NumberOfSubPaths"]):
            # Load SubPath section and append to SubPaths list ...
            ans["SubPaths"].append(load_SubPath(fObj, debug = debug, errors = errors, indent = indent + 1))

    # Skip ahead to the end of the data structure ...
    fObj.seek(pos + ans["Length"] + 4)

    # Return answer ...
    return ans
