#!/usr/bin/env python3

# Define function ...
def parse_CLPI_file(
    br,
    ip,
    /,
):
    # Import sub-functions ...
    from .CLPI import load_header
    from .CLPI import load_ClipInfo
    from .CLPI import load_ClipMark
    from .CLPI import load_CPI
    from .CLPI import load_ExtensionData
    from .CLPI import load_ProgramInfo
    from .CLPI import load_SequenceInfo

    # Create dictionary to hold information ...
    info = {}

    # Open file ...
    with open(f"{br}/BDMV/CLIPINF/{ip:05d}.clpi", "rb") as fObj:
        # Load header ...
        info["header"] = load_header(fObj)

        # Load ClipInfo section ...
        info["ClipInfo"] = load_ClipInfo(fObj)

        # Load SequenceInfo section ...
        if info["header"]["SequenceInfoStartAddress"] != 0:
            fObj.seek(info["header"]["SequenceInfoStartAddress"])
            info["SequenceInfo"] = load_SequenceInfo(fObj)

        # Load ProgramInfo section ...
        if info["header"]["ProgramInfoStartAddress"] != 0:
            fObj.seek(info["header"]["ProgramInfoStartAddress"])
            info["ProgramInfo"] = load_ProgramInfo(fObj)

        # Load CPI section ...
        if info["header"]["CPIStartAddress"] != 0:
            fObj.seek(info["header"]["CPIStartAddress"])
            info["CPI"] = load_CPI(fObj)

        # Load ClipMark section ...
        if info["header"]["ClipMarkStartAddress"] != 0:
            fObj.seek(info["header"]["ClipMarkStartAddress"])
            info["ClipMark"] = load_ClipMark(fObj)

        # Load ExtensionData section ...
        if info["header"]["ExtensionDataStartAddress"] != 0:
            fObj.seek(info["header"]["ExtensionDataStartAddress"])
            info["ExtensionData"] = load_ExtensionData(fObj)

    # Return answer ...
    return info
