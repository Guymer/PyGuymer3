def load_PlayList(fobj):
    # NOTE: see https://github.com/lerks/BluRay/wiki/PlayList

    # Import standard modules ...
    import struct

    # Load sub-functions ...
    from .load_PlayItem import load_PlayItem
    from .load_SubPath import load_SubPath

    # Initialize variables ...
    ans = {}
    length2 = 0                                                                                                         # [B]

    # Read the binary data ...
    ans["Length"], = struct.unpack(">I", fobj.read(4))
    fobj.read(2);                                                                                                       length2 += 2
    ans["NumberOfPlayItems"], = struct.unpack(">H", fobj.read(2));                                                      length2 += 2
    ans["NumberOfSubPaths"], = struct.unpack(">H", fobj.read(2));                                                       length2 += 2

    # Loop over PlayItems ...
    ans["PlayItems"] = []
    for i in range(ans["NumberOfPlayItems"]):
        # Load PlayItem section and append to PlayItems list ...
        res, length2, length2a = load_PlayItem(fobj, length2)
        ans["PlayItems"].append(res)

    # Loop over SubPaths ...
    ans["SubPaths"] = []
    for i in range(ans["NumberOfSubPaths"]):
        # Load SubPath section and append to SubPaths list ...
        res, length2, length2a = load_SubPath(fobj, length2)
        ans["SubPaths"].append(res)

    # Pad out the read ...
    if length2 != ans["Length"]:
        l = ans["Length"] - length2                                                                                     # [B]
        fobj.read(l);                                                                                                   length2 += l

    # Return answer ...
    return ans, length2
