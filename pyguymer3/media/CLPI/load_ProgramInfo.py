def load_ProgramInfo(fObj):
    # NOTE: see https://github.com/lw/BluRay/wiki/ProgramInfo

    # Import standard modules ...
    import struct

    # Import sub-functions ...
    from .load_StreamCodingInfo import load_StreamCodingInfo

    # Initialize variables ...
    ans = {}

    # Read the binary data ...
    ans["Length"], = struct.unpack(">I", fObj.read(4))
    BytesStart = fObj.tell()

    fObj.read(1)

    ans["NumberOfPrograms"], = struct.unpack(">B", fObj.read(1))
    if ans["NumberOfPrograms"]:
        ans["ProgramInfo"] = list()
        for i in range(ans["NumberOfPrograms"]):
            tmp_dict = dict()
            tmp_dict["SPNProgramSequenceStart"], = struct.unpack(">I", fObj.read(4))
            tmp_dict["ProgramMapPID"], = struct.unpack(">H", fObj.read(2))
            tmp_dict["NumberOfStreamsInPS"], = struct.unpack(">B", fObj.read(1))
            tmp_dict["MayBeNumberOfGroups"], = struct.unpack(">B", fObj.read(1))

            tmp_dict["StreamsInPS"] = list()
            for ii in range(tmp_dict["NumberOfStreamsInPS"]):
                tmp2_dict = dict()
                tmp2_dict['StreamPID'], = struct.unpack(">H", fObj.read(2))
                tmp2_dict["StreamCodingInfo"] = load_StreamCodingInfo(fObj)
                tmp_dict["StreamsInPS"].append(tmp2_dict)
            ans["ProgramInfo"].append(tmp_dict)

    # Pad out the read ...
    BytesEnd = fObj.tell()
    BytesPassed = BytesEnd - BytesStart
    if BytesPassed < ans["Length"]:
        l = ans["Length"] - BytesPassed
        fObj.read(l)
    elif BytesPassed > ans["Length"]:
        print("load_ProgramInfo: incorrect length")

    # Return answer ...
    return ans
