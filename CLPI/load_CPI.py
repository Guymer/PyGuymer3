def load_CPI(fobj):
    # NOTE: see https://github.com/lerks/BluRay/wiki/CPI
    # and https://patents.google.com/patent/US20090238539

    # Import modules ...
    import os
    import struct

    # Initialize variables ...
    ans = {}

    # Read the binary data ...
    ans["Length"], = struct.unpack(">I", fobj.read(4))
    bytesStart = fobj.tell()

    if ans["Length"] != 0:
        ans["CPIType"], = struct.unpack(">H", fobj.read(2))

        # blkEPMap begins, this position need to record to use later
        blkEPMapStart = fobj.tell()
        ans["NumberOfStreamPIDEntries"], = struct.unpack(">H", fobj.read(2))
        ans["StreamPIDEntries"] = list()
        for i in range(ans["NumberOfStreamPIDEntries"]):
            tmp_dic = dict()
            tmp_dic["StreamPID"], = struct.unpack(">H", fobj.read(2))

            # 10 reserved; 4 EPStreamType; 2+14 NumberOfEPCoarseEntries; 2+16 NumberOfEPFineEntries
            # reserved 10bits, EPStreamType 4bits, NumberOfEPCoarseEntries H2 Bits
            tmp_EPStreamType_NumberOfEPCoarseEntriesH2, = struct.unpack(">H", fobj.read(2))
            # NumberOfEPCoarseEntries L14 Bits, NumberOfEPFineEntries H2 Bits
            tmp_NumberOfEPCoarseEntriesL14_NumberOfEPFineEntriesH2, = struct.unpack(">H", fobj.read(2))
            # NumberOfEPFineEntries L16 Bits
            tmp_NumberOfEPFineEntriesL16, = struct.unpack(">H", fobj.read(2))

            tmp_dic["EPStreamType"] = (tmp_EPStreamType_NumberOfEPCoarseEntriesH2 >> 2) & 0xF

            tmp_NumberOfEPCoarseEntriesH2 = (tmp_EPStreamType_NumberOfEPCoarseEntriesH2 & 0b11) << 14
            tmp_NumberOfEPCoarseEntriesL14 = (tmp_NumberOfEPCoarseEntriesL14_NumberOfEPFineEntriesH2 >> 2) & 0x3FFF
            tmp_dic["NumberOfEPCoarseEntries"] = tmp_NumberOfEPCoarseEntriesH2 + tmp_NumberOfEPCoarseEntriesL14

            tmp_NumberOfEPFineEntriesH2 = (tmp_NumberOfEPCoarseEntriesL14_NumberOfEPFineEntriesH2 & 0b11) << 16
            tmp_dic["NumberOfEPFineEntries"] = tmp_NumberOfEPFineEntriesH2 + tmp_NumberOfEPFineEntriesL16

            # Entry Point Map
            tmp_dic["EPMapForOneStreamPIDStartAddress"], = struct.unpack(">I", fobj.read(4))
            
            ans["StreamPIDEntries"].append(tmp_dic)

        ans["EPMapForOneStreamPID"] = list()
        for i in range(ans["NumberOfStreamPIDEntries"]):
            fobj.seek(blkEPMapStart + ans["StreamPIDEntries"][i]["EPMapForOneStreamPIDStartAddress"], os.SEEK_SET)

            tmp2_dic = dict()
            tmp2_dic["EPFineTableStartAddress"], = struct.unpack(">I", fobj.read(4))

            tmp2_dic['EPCoarseEntries'] = list()
            for ii in range(ans["StreamPIDEntries"][i]["NumberOfEPCoarseEntries"]):
                # RefToEPFineID 18; PTSEPCoarse 14
                # RefToEPFineID_H16 16bits
                tmp_RefToEPFineID_H16, = struct.unpack(">H", fobj.read(2))
                # RefToEPFineID_L2 2 bits; PTSEPCoarse 14 bits
                tmp_RefToEPFineID_L2_PTSEPCoarse_14, = struct.unpack(">H", fobj.read(2))

                tmp_RefToEPFineID_L2 = (tmp_RefToEPFineID_L2_PTSEPCoarse_14 >> 14) & 0b11
                tmp_PTSEPCoarse = tmp_RefToEPFineID_L2_PTSEPCoarse_14 & 0x3FFF

                tmp3_dic = dict()
                tmp3_dic['RefToEPFineID'] = tmp_RefToEPFineID_H16 + tmp_RefToEPFineID_L2
                tmp3_dic['PTSEPCoarse'] = tmp_PTSEPCoarse
                tmp3_dic['SPNEPCoarse'], = struct.unpack(">I", fobj.read(4))

                tmp2_dic['EPCoarseEntries'].append(tmp3_dic)

            tmp2_dic['EPFineTable'] = list()
            # EPFineTableStartAddress is relative to the address of the EPMapForOneStreamPID
            fobj.seek(blkEPMapStart + ans["StreamPIDEntries"][i]["EPMapForOneStreamPIDStartAddress"]
                      + tmp2_dic["EPFineTableStartAddress"], os.SEEK_SET)

            for ii in range(ans["StreamPIDEntries"][i]["NumberOfEPFineEntries"]):
                # 1 ReservedEPFine (also called IsAngleChangePoint); 3 IEndPositionOffset; 11 PTSEPFine; 17 SPNEPFine
                # IsAngleChangePoint 1 bits, IEndPositionOffset 3 bits, PTSEPFine 11 bits, SPNEPFine_H1 1 bits
                tmp_IsAngleChangePoint_IEndPositionOffset_PTSEPFine_SPNEPFine_H1, = struct.unpack(">H", fobj.read(2))
                # SPNEPFine L16 bits
                tmp_SPNEPFine_L16, = struct.unpack(">H", fobj.read(2))

                tmp4_dic = dict()
                tmp4_dic['IsAngleChangePoint'] = (tmp_IsAngleChangePoint_IEndPositionOffset_PTSEPFine_SPNEPFine_H1 >> 15) & 0x1
                tmp4_dic['IEndPositionOffset'] = (tmp_IsAngleChangePoint_IEndPositionOffset_PTSEPFine_SPNEPFine_H1 >> 12) & 0x7
                tmp4_dic['PTSEPFine'] = (tmp_IsAngleChangePoint_IEndPositionOffset_PTSEPFine_SPNEPFine_H1 >> 1) & 0x7FF
                tmp_SPNEPFine_H1 = (tmp_IsAngleChangePoint_IEndPositionOffset_PTSEPFine_SPNEPFine_H1 & 0x1) << 16
                tmp4_dic['SPNEPFine'] = tmp_SPNEPFine_H1 + tmp_SPNEPFine_L16

                tmp2_dic['EPFineTable'].append(tmp4_dic)

        ans["EPMapForOneStreamPID"].append(tmp2_dic)

    # Pad out the read ...
    bytesEnd = fobj.tell()
    bytesPassed = bytesEnd - bytesStart
    if bytesPassed < ans["Length"]:
        l = ans["Length"] - bytesPassed
        fobj.read(l)
        print("load_CPI: skip %d bytes" % l)
    elif bytesPassed > ans["Length"]:
        print("load_CPI: incorrect length")

    # Return answer ...
    return ans
