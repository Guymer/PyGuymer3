#!/usr/bin/env python3

# Define function ...
def returnPngInfo(
    pName,
    /,
    *,
    debug = __debug__,
):
    """Return information about a PNG

    This function returns information, such as filter type and compression
    method, about a PNG.

    Parameters
    ----------
    pName : str
        The input PNG.
    debug : bool, optional
        Print debug messages.

    Returns
    -------
    nx : int
        The width of the input PNG.
    ny : int
        The height of the input PNG.
    ct : str
        The colour type of the input PNG (as defined in the PNG specification
        [2]_).
    ft : str
        The filter type of the input PNG (as defined in the PNG specification
        [2]_)
    wbits : int
        The compression window size of the input PNG (as defined in the ZLIB
        compressed data format specification [3]_).
    fdict : bool
        Whether the compressor was preconditioned with data.
    flevel : str
        The compression level of the input PNG (as defined in the ZLIB
        compressed data format specification [3]_).

    Notes
    -----
    This function only supports 8-bit images (either greyscale, paletted or
    truecolour), without interlacing.

    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    .. [2] PNG Specification (Third Edition), https://www.w3.org/TR/png-3/
    .. [3] ZLIB Compressed Data Format Specification (Version 3.3), https://datatracker.ietf.org/doc/html/rfc1950
    """

    # Import standard modules ...
    import binascii
    import os
    import struct
    import zlib

    # **************************************************************************

    # Create short-hands ...
    chkLen = None                                                               # [B]
    chkSrc = bytearray()
    cts = {
        0 : "greyscale",
        2 : "truecolor",
        3 : "indexed-color",
        4 : "greyscale with alpha",
        6 : "truecolor with alpha",
    }
    fts = {
        0 : "none",
        1 : "sub",
        2 : "up",
        3 : "average",
        4 : "paeth",
    }
    nc = None                                                                   # [B/px]
    nx = None                                                                   # [px]
    ny = None                                                                   # [px]
    pSize = os.path.getsize(pName)                                              # [B]
    zlib_flevels = {
        0 : "compressor used fastest algorithm",
        1 : "compressor used fast algorithm",
        2 : "compressor used default algorithm",
        3 : "compressor used maximum compression, slowest algorithm",
    }

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

            # Populate PNG metadata if this is the IHDR chunk and skip ahead to
            # the next chunk ...
            if chkTyp == "IHDR":
                assert chkLen == 13, f"the \"IHDR\" chunk is {chkLen:,d} bytes long"
                nx, = struct.unpack(">I", fObj.read(4))                         # [px]
                ny, = struct.unpack(">I", fObj.read(4))                         # [px]
                bd, = struct.unpack("B", fObj.read(1))                          # [b]
                ct, = struct.unpack("B", fObj.read(1))
                cm, = struct.unpack("B", fObj.read(1))
                fm, = struct.unpack("B", fObj.read(1))
                im, = struct.unpack("B", fObj.read(1))
                if bd != 8:
                    return f"un-supported bit depth ({bd:,d} bits)"
                assert ct in cts, f"the colour type is {ct:,d}"
                if ct not in [0, 2, 3,]:
                    return f"un-supported colour type ({ct:,d}; {cts[ct]})"
                assert cm == 0, f"the compression method is {cm:,d}"
                assert fm == 0, f"the filter method is {fm:,d}"
                if im != 0:
                    return f"un-supported interlace method ({im:,d})"
                if ct in [0, 3,]:
                    nc = 1                                                      # [B/px]
                else:
                    nc = 3                                                      # [B/px]
                fObj.seek(4, os.SEEK_CUR)
                continue

            # Concatenate image data if this is a IDAT chunk and skip ahead to
            # the next chunk ...
            if chkTyp == "IDAT":
                chkSrc += fObj.read(chkLen)
                fObj.seek(4, os.SEEK_CUR)
                continue

            # Skip ahead to the next chunk ...
            fObj.seek(chkLen, os.SEEK_CUR)
            fObj.seek(4, os.SEEK_CUR)
    assert chkLen is not None, "\"chkLen\" has not been determined"
    assert nc is not None, "\"nc\" has not been determined"
    assert nx is not None, "\"nx\" has not been determined"
    assert ny is not None, "\"ny\" has not been determined"

    # Populate ZLIB metadata ...
    zlib_cmf = chkSrc[0]
    zlib_cm = zlib_cmf % 16
    zlib_cinfo = zlib_cmf // 16
    assert zlib_cm == 8, f"the ZLIB compression method is {zlib_cm:,d}"
    assert zlib_cinfo <= 7, f"the ZLIB window size minus 8 is {zlib_cinfo:,d}"
    wbits = zlib_cinfo + 8

    # Populate more ZLIB metadata ...
    zlib_flg = chkSrc[1]
    zlib_fcheck = zlib_flg % 32
    zlib_fdict = (zlib_flg % 64) // 32
    zlib_flevel = zlib_flg // 64
    assert (zlib_cmf * 256 + zlib_flg) % 31 == 0, f"the ZLIB flag check is {zlib_fcheck:,d}"
    assert zlib_fdict in [0, 1,], f"the ZLIB preset dictionary is {zlib_fdict:,d}"
    assert zlib_flevel in zlib_flevels, f"the ZLIB compression level is {zlib_flevel:,d}"

    # Decompress the image data ...
    chkSrc = zlib.decompress(chkSrc)
    assert len(chkSrc) == ny * (nx * nc + 1), f"the decompressed image data is {len(chkSrc):,d} bytes"

    if debug:
        print(f"DEBUG: \"{pName}\" has a compression ratio of {float(chkLen) / float(len(chkSrc)):.3f}×.")

    # Initialize histogram ...
    hist = {}
    for ft, name in fts.items():
        hist[name] = 0                                                          # [#]

    # Populate histogram ...
    for iy in range(ny):
        ft = chkSrc[iy * (nx * nc + 1)]
        hist[fts[ft]] += 1                                                      # [#]

    # Return answer ...
    for name, n in hist.items():
        if n == ny:
            return nx, ny, cts[ct], name, wbits, bool(zlib_fdict), zlib_flevels[zlib_flevel], None
    return nx, ny, cts[ct], "adaptive", wbits, bool(zlib_fdict), zlib_flevels[zlib_flevel], hist
