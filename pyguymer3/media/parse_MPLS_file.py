#!/usr/bin/env python3

# Define function ...
def parse_MPLS_file(br, ip, kwArgCheck = None, debug = False, errors = "strict", indent = 0):
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
    with open(f"{br}/BDMV/PLAYLIST/{ip:05d}.mpls", "rb") as fObj:
        # Load header ...
        info["header"] = load_header(fObj, debug = debug, errors = errors, indent = indent)

        # Load AppInfoPlayList section ...
        info["AppInfoPlayList"] = load_AppInfoPlayList(fObj, debug = debug, errors = errors, indent = indent)

        # Load PlayList section ...
        if info["header"]["PlayListStartAddress"] != 0:
            fObj.seek(info["header"]["PlayListStartAddress"])
            info["PlayList"] = load_PlayList(fObj, debug = debug, errors = errors, indent = indent)

        # Load PlayListMark section ...
        if info["header"]["PlayListMarkStartAddress"] != 0:
            fObj.seek(info["header"]["PlayListMarkStartAddress"])
            info["PlayListMark"] = load_PlayListMark(fObj, debug = debug, errors = errors, indent = indent)

        # Load ExtensionData section ...
        if info["header"]["ExtensionDataStartAddress"] != 0:
            fObj.seek(info["header"]["ExtensionDataStartAddress"])
            info["ExtensionData"] = load_ExtensionData(fObj, debug = debug, errors = errors, indent = indent)

    # Return answer ...
    return info
