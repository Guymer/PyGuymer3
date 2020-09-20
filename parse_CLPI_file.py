def parse_CLPI_file(br, ip):
    # Import standard modules ...
    import os

    # Load sub-functions ...
    from .CLPI.load_header import load_header
    from .CLPI.load_ClipInfo import load_ClipInfo
    from .CLPI.load_ClipMark import load_ClipMark
    from .CLPI.load_CPI import load_CPI
    from .CLPI.load_ExtensionData import load_ExtensionData
    from .CLPI.load_ProgramInfo import load_ProgramInfo
    from .CLPI.load_SequenceInfo import load_SequenceInfo

    # Create dictionary to hold information ...
    info = {}

    # Open file ...
    with open(os.path.join(br, "BDMV/CLIPINF/{0:05d}.clpi".format(ip)), "rb") as fobj:
        # Load header ...
        res = load_header(fobj)
        info["header"] = res

        # Load ClipInfo section ...
        res = load_ClipInfo(fobj)
        info["ClipInfo"] = res

        # Load SequenceInfo section ...
        if info["header"]["SequenceInfoStartAddress"] != 0:
            fobj.seek(info["header"]["SequenceInfoStartAddress"], os.SEEK_SET)
            res = load_SequenceInfo(fobj)
            info["SequenceInfo"] = res

        # Load ProgramInfo section ...
        if info["header"]["ProgramInfoStartAddress"] != 0:
            fobj.seek(info["header"]["ProgramInfoStartAddress"], os.SEEK_SET)
            res = load_ProgramInfo(fobj)
            info["ProgramInfo"] = res

        # Load CPI section ...
        if info["header"]["CPIStartAddress"] != 0:
            fobj.seek(info["header"]["CPIStartAddress"], os.SEEK_SET)
            res = load_CPI(fobj)
            info["CPI"] = res

        # Load ClipMark section ...
        if info["header"]["ClipMarkStartAddress"] != 0:
            fobj.seek(info["header"]["ClipMarkStartAddress"], os.SEEK_SET)
            res = load_ClipMark(fobj)
            info["ClipMark"] = res

        # Load ExtensionData section ...
        if info["header"]["ExtensionDataStartAddress"] != 0:
            fobj.seek(info["header"]["ExtensionDataStartAddress"], os.SEEK_SET)
            res = load_ExtensionData(fobj)
            info["ExtensionData"] = res

    # Return answer ...
    return info
