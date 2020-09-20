def parse_MPLS_file(br, ip):
    # Import standard modules ...
    import os

    # Load sub-functions ...
    from .MPLS.load_header import load_header
    from .MPLS.load_AppInfoPlayList import load_AppInfoPlayList
    from .MPLS.load_ExtensionData import load_ExtensionData
    from .MPLS.load_PlayList import load_PlayList
    from .MPLS.load_PlayListMark import load_PlayListMark

    # Create dictionary to hold information ...
    info = {}

    # Open file ...
    with open(os.path.join(br, "BDMV/PLAYLIST/{0:05d}.mpls".format(ip)), "rb") as fobj:
        # Load header ...
        res, length0 = load_header(fobj)
        info["header"] = res

        # Load AppInfoPlayList section ...
        res, length1 = load_AppInfoPlayList(fobj)
        info["AppInfoPlayList"] = res

        # Load PlayList section ...
        if info["header"]["PlayListStartAddress"] != 0:
            fobj.seek(info["header"]["PlayListStartAddress"], os.SEEK_SET)
            res, length2 = load_PlayList(fobj)
            info["PlayList"] = res

        # Load PlayListMark section ...
        if info["header"]["PlayListMarkStartAddress"] != 0:
            fobj.seek(info["header"]["PlayListMarkStartAddress"], os.SEEK_SET)
            res, length3 = load_PlayListMark(fobj)
            info["PlayListMark"] = res

        # Load ExtensionData section ...
        if info["header"]["ExtensionDataStartAddress"] != 0:
            fobj.seek(info["header"]["ExtensionDataStartAddress"], os.SEEK_SET)
            res, length4 = load_ExtensionData(fobj)
            info["ExtensionData"] = res

    # Return answer ...
    return info
