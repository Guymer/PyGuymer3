def parse_CLPI_file(br, ip):
    # Import sub-functions ...
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
    with open(f"{br}/BDMV/CLIPINF/{ip:05d}.clpi", "rb") as fObj:
        # Load header ...
        res = load_header(fObj)
        info["header"] = res

        # Load ClipInfo section ...
        res = load_ClipInfo(fObj)
        info["ClipInfo"] = res

        # Load SequenceInfo section ...
        if info["header"]["SequenceInfoStartAddress"] != 0:
            fObj.seek(info["header"]["SequenceInfoStartAddress"])
            res = load_SequenceInfo(fObj)
            info["SequenceInfo"] = res

        # Load ProgramInfo section ...
        if info["header"]["ProgramInfoStartAddress"] != 0:
            fObj.seek(info["header"]["ProgramInfoStartAddress"])
            res = load_ProgramInfo(fObj)
            info["ProgramInfo"] = res

        # Load CPI section ...
        if info["header"]["CPIStartAddress"] != 0:
            fObj.seek(info["header"]["CPIStartAddress"])
            res = load_CPI(fObj)
            info["CPI"] = res

        # Load ClipMark section ...
        if info["header"]["ClipMarkStartAddress"] != 0:
            fObj.seek(info["header"]["ClipMarkStartAddress"])
            res = load_ClipMark(fObj)
            info["ClipMark"] = res

        # Load ExtensionData section ...
        if info["header"]["ExtensionDataStartAddress"] != 0:
            fObj.seek(info["header"]["ExtensionDataStartAddress"])
            res = load_ExtensionData(fObj)
            info["ExtensionData"] = res

    # Return answer ...
    return info
