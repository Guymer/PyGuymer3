def parse_MPLS_file(br, ip, kwArgCheck = None, debug = False, errors = "strict", indent = 0):
    # Import standard modules ...
    import os

    # Import sub-functions ...
    from .MPLS.load_header import load_header
    from .MPLS.load_AppInfoPlayList import load_AppInfoPlayList
    from .MPLS.load_ExtensionData import load_ExtensionData
    from .MPLS.load_PlayList import load_PlayList
    from .MPLS.load_PlayListMark import load_PlayListMark

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Create dictionary to hold information ...
    info = {}

    # Open file ...
    with open(os.path.join(br, "BDMV/PLAYLIST/{:05d}.mpls".format(ip)), "rb") as fobj:
        # Load header ...
        info["header"] = load_header(fobj, debug = debug, errors = errors, indent = indent)

        # Load AppInfoPlayList section ...
        info["AppInfoPlayList"] = load_AppInfoPlayList(fobj, debug = debug, errors = errors, indent = indent)

        # Load PlayList section ...
        if info["header"]["PlayListStartAddress"] != 0:
            fobj.seek(info["header"]["PlayListStartAddress"])
            info["PlayList"] = load_PlayList(fobj, debug = debug, errors = errors, indent = indent)

        # Load PlayListMark section ...
        if info["header"]["PlayListMarkStartAddress"] != 0:
            fobj.seek(info["header"]["PlayListMarkStartAddress"])
            info["PlayListMark"] = load_PlayListMark(fobj, debug = debug, errors = errors, indent = indent)

        # Load ExtensionData section ...
        if info["header"]["ExtensionDataStartAddress"] != 0:
            fobj.seek(info["header"]["ExtensionDataStartAddress"])
            info["ExtensionData"] = load_ExtensionData(fobj, debug = debug, errors = errors, indent = indent)

    # Return answer ...
    return info
