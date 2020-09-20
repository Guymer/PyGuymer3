def parse_MPLS_file(br, ip):
    # Import standard modules ...
    import os

    # Load sub-functions ...
    from .MPLS import MPLS

    # Create dictionary to hold information ...
    info = {}

    # Open file ...
    with open(os.path.join(br, "BDMV/PLAYLIST/{0:05d}.mpls".format(ip)), "rb") as fobj:
        # Load header ...
        res, length0 = MPLS.load_header(fobj)
        info["header"] = res

        # Load AppInfoPlayList section ...
        res, length1 = MPLS.load_AppInfoPlayList(fobj)
        info["AppInfoPlayList"] = res

        # Load PlayList section ...
        if info["header"]["PlayListStartAddress"] != 0:
            fobj.seek(info["header"]["PlayListStartAddress"], os.SEEK_SET)
            res, length2 = MPLS.load_PlayList(fobj)
            info["PlayList"] = res

        # Load PlayListMark section ...
        if info["header"]["PlayListMarkStartAddress"] != 0:
            fobj.seek(info["header"]["PlayListMarkStartAddress"], os.SEEK_SET)
            res, length3 = MPLS.load_PlayListMark(fobj)
            info["PlayListMark"] = res

        # Load ExtensionData section ...
        if info["header"]["ExtensionDataStartAddress"] != 0:
            fobj.seek(info["header"]["ExtensionDataStartAddress"], os.SEEK_SET)
            res, length4 = MPLS.load_ExtensionData(fobj)
            info["ExtensionData"] = res

    # Return answer ...
    return info
