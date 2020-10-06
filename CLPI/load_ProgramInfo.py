def load_ProgramInfo(fobj):
    # NOTE: see https://github.com/lw/BluRay/wiki/ProgramInfo

    # Import standard modules ...
    import struct

    # Load sub-functions ...
    from .load_StreamCodingInfo import load_StreamCodingInfo

    # Initialize variables ...
    ans = {}

    # Read the binary data ...
    ans["Length"], = struct.unpack(">I", fobj.read(4))
    BytesStart = fobj.tell()

    fobj.read(1)

    ans["NumberOfPrograms"], = struct.unpack(">B", fobj.read(1))
    if ans["NumberOfPrograms"]:
        ans["ProgramInfo"] = list()
        for i in range(ans["NumberOfPrograms"]):
            tmp_dict = dict()
            tmp_dict["SPNProgramSequenceStart"], = struct.unpack(">I", fobj.read(4))
            tmp_dict["ProgramMapPID"], = struct.unpack(">H", fobj.read(2))
            tmp_dict["NumberOfStreamsInPS"], = struct.unpack(">B", fobj.read(1))
            tmp_dict["MayBeNumberOfGroups"], = struct.unpack(">B", fobj.read(1))

            tmp_dict["StreamsInPS"] = list()
            for ii in range(tmp_dict["NumberOfStreamsInPS"]):
                tmp2_dict = dict()
                tmp2_dict['StreamPID'], = struct.unpack(">H", fobj.read(2))
                tmp2_dict["StreamCodingInfo"] = load_StreamCodingInfo(fobj)
                tmp_dict["StreamsInPS"].append(tmp2_dict)
            ans["ProgramInfo"].append(tmp_dict)

    # Pad out the read ...
    BytesEnd = fobj.tell()
    BytesPassed = BytesEnd - BytesStart
    if BytesPassed < ans["Length"]:
        l = ans["Length"] - BytesPassed
        fobj.read(l)
    elif BytesPassed > ans["Length"]:
        print("load_ProgramInfo: incorrect length")

    # Return answer ...
    return ans
