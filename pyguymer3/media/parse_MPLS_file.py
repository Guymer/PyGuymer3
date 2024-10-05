#!/usr/bin/env python3

# Define function ...
def parse_MPLS_file(
    br,
    ip,
    /,
):
    # Import sub-functions ...
    from .MPLS import load_header
    from .MPLS import load_AppInfoPlayList
    from .MPLS import load_ExtensionData
    from .MPLS import load_PlayList
    from .MPLS import load_PlayListMark

    # Create dictionary to hold information ...
    info = {}

    # Open file ...
    with open(f"{br}/BDMV/PLAYLIST/{ip:05d}.mpls", "rb") as fObj:
        # Load header ...
        info["header"] = load_header(fObj)

        # Load AppInfoPlayList section ...
        info["AppInfoPlayList"] = load_AppInfoPlayList(fObj)

        # Load PlayList section ...
        if info["header"]["PlayListStartAddress"] != 0:
            fObj.seek(info["header"]["PlayListStartAddress"])
            info["PlayList"] = load_PlayList(fObj)

        # Load PlayListMark section ...
        if info["header"]["PlayListMarkStartAddress"] != 0:
            fObj.seek(info["header"]["PlayListMarkStartAddress"])
            info["PlayListMark"] = load_PlayListMark(fObj)

        # Load ExtensionData section ...
        if info["header"]["ExtensionDataStartAddress"] != 0:
            fObj.seek(info["header"]["ExtensionDataStartAddress"])
            info["ExtensionData"] = load_ExtensionData(fObj)

    # Return answer ...
    return info
