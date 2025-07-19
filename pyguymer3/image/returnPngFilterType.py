#!/usr/bin/env python3

# Define function ...
def returnPngFilterType(
    pName,
    /,
    *,
    debug = __debug__,
):
    # Import standard modules ...
    import binascii
    import os
    import struct
    import zlib

    # **************************************************************************

    # Create short-hands ...
    chkLen = None                                                               # [B]
    chkSrc = None
    nc = None                                                                   # [B/px]
    nx = None                                                                   # [px]
    ny = None                                                                   # [px]
    pSize = os.path.getsize(pName)                                              # [B]

    # Open file ...
    with open(pName, "rb") as fObj:
        # Read file signature ...
        pngSig = fObj.read(8)
        assert pngSig == binascii.unhexlify("89504E470D0A1A0A"), f"\"{pName}\" is not a PNG file"

        # Loop over chunks ...
        while fObj.tell() < pSize:
            # Read chunk header ...
            chkLen, = struct.unpack(">I", fObj.read(4))                         # [B]
            chkTyp = fObj.read(4).decode("ascii")

            # Populate metadata if this is the IHDR chunk and skip ahead to the
            # next chunk ...
            if chkTyp == "IHDR":
                assert chkLen == 13, f"the \"IHDR\" chunk is {chkLen:,d} bytes long"
                nx, = struct.unpack(">I", fObj.read(4))                         # [px]
                ny, = struct.unpack(">I", fObj.read(4))                         # [px]
                bd, = struct.unpack("B", fObj.read(1))                          # [b]
                ct, = struct.unpack("B", fObj.read(1))
                cm, = struct.unpack("B", fObj.read(1))
                fm, = struct.unpack("B", fObj.read(1))
                im, = struct.unpack("B", fObj.read(1))
                assert bd == 8, f"the bit depth is {bd:,d} bits"
                assert ct in [0, 2, 3,], f"the colour type is {ct:,d}"
                assert cm == 0, f"the compression method is {cm:,d}"
                assert fm == 0, f"the filter method is {fm:,d}"
                assert im == 0, f"the interlace method is {im:,d}"
                if ct in [0, 3,]:
                    nc = 1                                                      # [B/px]
                else:
                    nc = 3                                                      # [B/px]
                fObj.seek(4, os.SEEK_CUR)
                continue

            # Decompress image data if this is the IDAT chunk and stop looping
            # over chunks ...
            if chkTyp == "IDAT":
                chkSrc = zlib.decompress(fObj.read(chkLen))
                fObj.seek(4, os.SEEK_CUR)
                break

            # Skip ahead to the next chunk ...
            fObj.seek(chkLen, os.SEEK_CUR)
            fObj.seek(4, os.SEEK_CUR)
    assert chkLen is not None, "\"chkLen\" has not been determined"
    assert chkSrc is not None, "\"chkSrc\" has not been determined"
    assert nc is not None, "\"nc\" has not been determined"
    assert nx is not None, "\"nx\" has not been determined"
    assert ny is not None, "\"ny\" has not been determined"
    assert len(chkSrc) == ny * (nx * nc + 1), f"the decompressed image data is {len(chkSrc):,d} bytes"

    if debug:
        print(f"DEBUG: \"{pName}\" is {nx:,d} px wide.")
        print(f"DEBUG: \"{pName}\" is {ny:,d} px high.")
        print(f"DEBUG: \"{pName}\" has {nc:,d} colour channels.")
        print(f"DEBUG: \"{pName}\" has a compression ratio of {float(chkLen) / float(len(chkSrc)):.3f}×.")

    # Initialize histogram ...
    hist = {
        0 : {
               "n" : 0,                                                         # [#]
            "name" : "none",
        },
        1 : {
               "n" : 0,                                                         # [#]
            "name" : "sub",
        },
        2 : {
               "n" : 0,                                                         # [#]
            "name" : "up",
        },
        3 : {
               "n" : 0,                                                         # [#]
            "name" : "average",
        },
        4 : {
               "n" : 0,                                                         # [#]
            "name" : "paeth",
        },
    }

    # Loop over scanlines ...
    for iy in range(ny):
        # Populate histogram ...
        ft = chkSrc[iy * (nx * nc + 1)]
        hist[ft]["n"] += 1                                                      # [#]

    # Return answer ...
    for ft, info in hist.items():
        if info["n"] == ny:
            return info["name"]
    return "adaptive"
