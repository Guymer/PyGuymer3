#!/usr/bin/env python3

# Define function ...
def load_ProgramInfo(fObj):
    # NOTE: See https://github.com/lw/BluRay/wiki/ProgramInfo

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

    # Read the binary data ...
    ans["NumberOfPrograms"], = struct.unpack(">B", fObj.read(1))
    if ans["NumberOfPrograms"]:
        ans["ProgramInfo"] = []
        for _ in range(ans["NumberOfPrograms"]):
            tmp_dict = {}
            tmp_dict["SPNProgramSequenceStart"], = struct.unpack(">I", fObj.read(4))
            tmp_dict["ProgramMapPID"], = struct.unpack(">H", fObj.read(2))
            tmp_dict["NumberOfStreamsInPS"], = struct.unpack(">B", fObj.read(1))
            tmp_dict["MayBeNumberOfGroups"], = struct.unpack(">B", fObj.read(1))
            tmp_dict["StreamsInPS"] = []
            for _ in range(tmp_dict["NumberOfStreamsInPS"]):
                tmp2_dict = {}
                tmp2_dict["StreamPID"], = struct.unpack(">H", fObj.read(2))
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
        raise Exception("read more bytes than the length") from None

    # Return answer ...
    return ans
