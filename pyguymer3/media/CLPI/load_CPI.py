#!/usr/bin/env python3

# Define function ...
def load_CPI(fObj):
    # NOTE: See https://github.com/lw/BluRay/wiki/CPI
    # NOTE: See https://patents.google.com/patent/US20090238539

    # Import standard modules ...
    import struct

    # Initialize variables ...
    ans = {}

    # Read the binary data ...
    ans["Length"], = struct.unpack(">I", fObj.read(4))
    bytesStart = fObj.tell()

    # Check if things need reading ...
    if ans["Length"] != 0:
        # Read the binary data ...
        ans["CPIType"], = struct.unpack(">H", fObj.read(2))

        # Read the binary data ...
        blkEPMapStart = fObj.tell()
        ans["NumberOfStreamPIDEntries"], = struct.unpack(">H", fObj.read(2))
        ans["StreamPIDEntries"] = []
        for _ in range(ans["NumberOfStreamPIDEntries"]):
            tmp_dic = {}
            tmp_dic["StreamPID"], = struct.unpack(">H", fObj.read(2))
            tmp_EPStreamType_NumberOfEPCoarseEntriesH2, = struct.unpack(">H", fObj.read(2))
            tmp_NumberOfEPCoarseEntriesL14_NumberOfEPFineEntriesH2, = struct.unpack(">H", fObj.read(2))
            tmp_NumberOfEPFineEntriesL16, = struct.unpack(">H", fObj.read(2))
            tmp_dic["EPStreamType"] = (tmp_EPStreamType_NumberOfEPCoarseEntriesH2 >> 2) & 0xF
            tmp_NumberOfEPCoarseEntriesH2 = (tmp_EPStreamType_NumberOfEPCoarseEntriesH2 & 0b11) << 14
            tmp_NumberOfEPCoarseEntriesL14 = (tmp_NumberOfEPCoarseEntriesL14_NumberOfEPFineEntriesH2 >> 2) & 0x3FFF
            tmp_dic["NumberOfEPCoarseEntries"] = tmp_NumberOfEPCoarseEntriesH2 + tmp_NumberOfEPCoarseEntriesL14
            tmp_NumberOfEPFineEntriesH2 = (tmp_NumberOfEPCoarseEntriesL14_NumberOfEPFineEntriesH2 & 0b11) << 16
            tmp_dic["NumberOfEPFineEntries"] = tmp_NumberOfEPFineEntriesH2 + tmp_NumberOfEPFineEntriesL16
            tmp_dic["EPMapForOneStreamPIDStartAddress"], = struct.unpack(">I", fObj.read(4))
            ans["StreamPIDEntries"].append(tmp_dic)

        # Read the binary data ...
        ans["EPMapForOneStreamPID"] = []
        for i in range(ans["NumberOfStreamPIDEntries"]):
            fObj.seek(blkEPMapStart + ans["StreamPIDEntries"][i]["EPMapForOneStreamPIDStartAddress"])
            tmp2_dic = {}
            tmp2_dic["EPFineTableStartAddress"], = struct.unpack(">I", fObj.read(4))
            tmp2_dic["EPCoarseEntries"] = []
            for _ in range(ans["StreamPIDEntries"][i]["NumberOfEPCoarseEntries"]):
                tmp_RefToEPFineID_H16, = struct.unpack(">H", fObj.read(2))
                tmp_RefToEPFineID_L2_PTSEPCoarse_14, = struct.unpack(">H", fObj.read(2))
                tmp_RefToEPFineID_L2 = (tmp_RefToEPFineID_L2_PTSEPCoarse_14 >> 14) & 0b11
                tmp_PTSEPCoarse = tmp_RefToEPFineID_L2_PTSEPCoarse_14 & 0x3FFF
                tmp3_dic = {}
                tmp3_dic["RefToEPFineID"] = tmp_RefToEPFineID_H16 + tmp_RefToEPFineID_L2
                tmp3_dic["PTSEPCoarse"] = tmp_PTSEPCoarse
                tmp3_dic["SPNEPCoarse"], = struct.unpack(">I", fObj.read(4))
                tmp2_dic["EPCoarseEntries"].append(tmp3_dic)
            tmp2_dic["EPFineTable"] = []
            fObj.seek(blkEPMapStart + ans["StreamPIDEntries"][i]["EPMapForOneStreamPIDStartAddress"] + tmp2_dic["EPFineTableStartAddress"])
            for _ in range(ans["StreamPIDEntries"][i]["NumberOfEPFineEntries"]):
                tmp_IsAngleChangePoint_IEndPositionOffset_PTSEPFine_SPNEPFine_H1, = struct.unpack(">H", fObj.read(2))
                tmp_SPNEPFine_L16, = struct.unpack(">H", fObj.read(2))
                tmp4_dic = {}
                tmp4_dic["IsAngleChangePoint"] = (tmp_IsAngleChangePoint_IEndPositionOffset_PTSEPFine_SPNEPFine_H1 >> 15) & 0x1
                tmp4_dic["IEndPositionOffset"] = (tmp_IsAngleChangePoint_IEndPositionOffset_PTSEPFine_SPNEPFine_H1 >> 12) & 0x7
                tmp4_dic["PTSEPFine"] = (tmp_IsAngleChangePoint_IEndPositionOffset_PTSEPFine_SPNEPFine_H1 >> 1) & 0x7FF
                tmp_SPNEPFine_H1 = (tmp_IsAngleChangePoint_IEndPositionOffset_PTSEPFine_SPNEPFine_H1 & 0x1) << 16
                tmp4_dic["SPNEPFine"] = tmp_SPNEPFine_H1 + tmp_SPNEPFine_L16
                tmp2_dic["EPFineTable"].append(tmp4_dic)
        ans["EPMapForOneStreamPID"].append(tmp2_dic)

    # Pad out the read ...
    bytesEnd = fObj.tell()
    bytesPassed = bytesEnd - bytesStart
    if bytesPassed < ans["Length"]:
        l = ans["Length"] - bytesPassed
        fObj.read(l)
    elif bytesPassed > ans["Length"]:
        raise Exception("read more bytes than the length") from None

    # Return answer ...
    return ans
