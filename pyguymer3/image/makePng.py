#!/usr/bin/env python3

# Define function ...
def makePng(
    arrUint8,
    /,
    *,
    calcAdaptive: bool = True,
     calcAverage: bool = True,
        calcNone: bool = True,
       calcPaeth: bool = True,
         calcSub: bool = True,
          calcUp: bool = True,
         choices: str = "fastest",
           debug: bool = __debug__,
             dpi: None | int = None,
          levels: None | list[int] = None,
       memLevels: None | list[int] = None,
         modTime = None,
        palUint8 = None,
      strategies: None | list[int] = None,
          wbitss: None | list[int] = None,
) -> bytearray:
    """Make a PNG

    This function reads in a "height * width * colour" unsigned 8-bit integer
    NumPy array and returns the Python bytearray which is the binary source of
    the PNG file of the input.

    By default, this function calculates the PNG image data stream using all
    five filters (as defined in the PNG specification [2]_), as well as adaptive
    filtering, and this function then uses the one filter which ends up having
    the smallest compressed size.

    This function also allows the user to declare sets of settings which are
    tried when compressing the PNG image data stream and this function then uses
    the one set of settings which ends up having the smallest compressed size.
    This allows the user to: either choose to spend CPU time trying different
    sets of settings in an attempt to find the one which produces the smallest
    compressed size for the supplied input; or try a stab in the dark at knowing
    the fastest (or best) set of settings.

    Parameters
    ----------
    arrUint8 : numpy.ndarray
        A "height * width * colour" unsigned 8-bit integer NumPy array.
    calcAdaptive : bool, optional
        Calculate the compressed PNG image data stream using an adaptive filter
        type. Each of the five named filters is applied to a scanline and a
        prediction is made as to which one will produce the smallest compressed
        scanline. The chosen filtered uncompressed scanline is concatenated with
        all of the other filtered uncompressed scanlines and a single
        compression operation is performed once the whole image has been
        processed.
    calcAverage : bool, optional
        Calculate the compressed PNG image data stream using the "average"
        filter type, as defined in the PNG specification [2]_.
    calcNone : bool, optional
        Calculate the compressed PNG image data stream using the "none" filter
        type, as defined in the PNG specification [2]_.
    calcPaeth : bool, optional
        Calculate the compressed PNG image data stream using the "Paeth" filter
        type, as defined in the PNG specification [2]_.
    calcSub : bool, optional
        Calculate the compressed PNG image data stream using the "sub" filter
        type, as defined in the PNG specification [2]_.
    calcUp : bool, optional
        Calculate the compressed PNG image data stream using the "up" filter
        type, as defined in the PNG specification [2]_.
    choices : str, optional
        If any of the settings are not passed (or passed as ``None``) then this
        string is used to set them. The accepted values are ``"fastest"``,
        ``"best"`` and ``"all"``.
    debug : bool, optional
        Print debug messages.
    dpi : None or float or int, optional
        If a number is passed then the ancillary "pHYs" chunk will get created
        and the resolution will be specified.
    levels : None or list of int, optional
        The list of compression levels to loop over when trying to find the
        smallest compressed size. If not supplied, or ``None``, then the
        value of ``choices`` will determine the value of ``levels``.

        If ``levels is None and choices == "fastest"`` then ``levels = [0,]``.

        If ``levels is None and choices == "best"`` then ``levels = [9,]``.

        If ``levels is None and choices == "all"`` then ``levels = [0, 1, 2, 3,
        4, 5, 6, 7, 8, 9,]``.

        See :py:func:`zlib.compressobj` for what the valid compression levels
        are.
    memLevels : None or list of int, optional
        The list of memory levels to loop over when trying to find the smallest
        compressed size. If not supplied, or ``None``, then the value of
        ``choices`` will determine the value of ``memLevels``.

        If ``memLevels is None and choices == "fastest"`` then ``memLevels = [9,
        ]``.

        If ``memLevels is None and choices == "best"`` then ``memLevels = [9,]``
        .

        If ``memLevels is None and choices == "all"`` then ``memLevels = [1, 2,
        3, 4, 5, 6, 7, 8, 9,]``.

        See :py:func:`zlib.compressobj` for what the valid memory levels are.
    modTime : None or datetime.datetime, optional
        If a time is passed then the ancillary "tIME" chunk will get created and
        the image last-modification time will be specified.
    palUint8 : None or numpy.ndarray, optional
        A "level * colour" unsigned 8-bit integer NumPy array. If the size of
        the "colours" axis in ``arrUint8`` is ``1`` then ``arrUint8`` is assumed
        to be either greyscale (if ``palUint8`` is ``None``) or paletted and
        ``palUint8`` is the palette.
    strategies : None or list of int, optional
        The list of strategies to loop over when trying to find the smallest
        compressed size. If not supplied, or ``None``, then the value of
        ``choices`` will determine the value of ``strategies``.

        If ``strategies is None and choices == "fastest"`` then ``strategies = [
        zlib.Z_DEFAULT_STRATEGY,]``.

        If ``strategies is None and choices == "best"`` then ``strategies = [
        zlib.Z_DEFAULT_STRATEGY,]``.

        If ``strategies is None and choices == "all"`` then ``strategies = [
        zlib.Z_DEFAULT_STRATEGY, zlib.Z_FILTERED, zlib.Z_HUFFMAN_ONLY,
        zlib.Z_RLE, zlib.Z_FIXED,]``.

        See :py:func:`zlib.compressobj` for what the valid strategies are.
    wbitss : None or list of int, optional
        The list of window sizes to loop over when trying to find the smallest
        compressed size. If not supplied, or ``None``, then the value of
        ``choices`` will determine the value of ``wbitss``.

        If ``wbitss is None and choices == "fastest"`` then ``wbitss = [15,]``.

        If ``wbitss is None and choices == "best"`` then ``wbitss = [15,]``.

        If ``wbitss is None and choices == "all"`` then ``wbitss = [9, 10, 11,
        12, 13, 14, 15,]``.

        See :py:func:`zlib.compressobj` for what the valid window sizes are.

    Returns
    -------
    src : bytearray
        The binary source of the PNG file of the input.

    Notes
    -----
    This function only creates 8-bit images (either greyscale, paletted or
    truecolour), without interlacing. It stores the entire image in a single
    "IDAT" chunk.

    This function always writes out three of the four critical chunks: "IHDR",
    "IDAT" and "IEND". Depending on optional keyword arguments which may be
    provided then this function may also write out the critical chunk "PLTE" as
    well as the ancillary chunks "iTIM" and "pHYs".

    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    .. [2] PNG Specification (Third Edition), https://www.w3.org/TR/png-3/
    """

    # Import standard modules ...
    import binascii
    import sys
    import zlib

    # Import special modules ...
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None

    # Import sub-functions ...
    from .makePngSrc import createStream

    # **************************************************************************

    # Populate compression levels if the user has not ...
    if levels is None:
        match choices:
            case "fastest":
                levels = [0,]
            case "best":
                levels = [9,]
            case "all":
                levels = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9,]
            case _:
                raise ValueError(f"\"choices\" was an unexpected value (\"{choices}\")") from None

    # Populate memory levels if the user has not ...
    if memLevels is None:
        match choices:
            case "fastest":
                memLevels = [9,]
            case "best":
                memLevels = [9,]
            case "all":
                memLevels = [1, 2, 3, 4, 5, 6, 7, 8, 9,]
            case _:
                raise ValueError(f"\"choices\" was an unexpected value (\"{choices}\")") from None

    # Populate strategies if the user has not ...
    if strategies is None:
        match choices:
            case "fastest":
                strategies = [zlib.Z_DEFAULT_STRATEGY,]
            case "best":
                strategies = [zlib.Z_DEFAULT_STRATEGY,]
            case "all":
                strategies = [zlib.Z_DEFAULT_STRATEGY, zlib.Z_FILTERED, zlib.Z_HUFFMAN_ONLY, zlib.Z_RLE, zlib.Z_FIXED,]
            case _:
                raise ValueError(f"\"choices\" was an unexpected value (\"{choices}\")") from None

    # Populate window sizes if the user has not ...
    if wbitss is None:
        match choices:
            case "fastest":
                wbitss = [15,]
            case "best":
                wbitss = [15,]
            case "all":
                wbitss = [9, 10, 11, 12, 13, 14, 15,]
            case _:
                raise ValueError(f"\"choices\" was an unexpected value (\"{choices}\")") from None

    # Check system ...
    assert sys.byteorder == "little", "the system is not little-endian"

    # Check input ...
    assert arrUint8.dtype == "uint8", f"the NumPy array is not 8-bit (\"{arrUint8.dtype}\")"
    assert arrUint8.ndim == 3, f"the NumPy array is not 3D (\"{arrUint8.ndim:d}\")"
    match arrUint8.shape[2]:
        case 1:
            if palUint8 is None:
                colourType = 0
            else:
                assert palUint8.dtype == "uint8", f"the NumPy palette is not 8-bit (\"{palUint8.dtype}\")"
                assert palUint8.ndim == 2, f"the NumPy palette is not 2D (\"{palUint8.ndim:d}\")"
                assert palUint8.shape[0] <= 256, f"the NumPy palette has more than 256 colours (\"{palUint8.shape[0]:,d}\")"
                assert palUint8.shape[1] == 3, "the NumPy palette does not have 3 colour channels"
                assert arrUint8.max() < palUint8.shape[0], f"the NumPy array references more colours than are in the NumPy palette (\"{arrUint8.max():d}\" -vs- \"{palUint8.shape[0]:d}\")"
                colourType = 3
        case 3:
            colourType = 2
        case _:
            raise ValueError(f"the NumPy array does not have either 1 or 3 colour channels (\"{arrUint8.shape[2]:d}\")") from None

    # **************************************************************************

    # Create short-hand ...
    arrInt16 = arrUint8.astype(numpy.int16)

    # Make the file signature ...
    pngSig = bytearray()
    pngSig += binascii.unhexlify("89504E470D0A1A0A")

    # Make the IHDR chunk ...
    hdrChk = bytearray()
    hdrChk += numpy.uint32(13).byteswap().tobytes()                             # Length
    hdrChk += bytearray("IHDR", encoding = "ascii")                             # Chunk type
    hdrChk += numpy.uint32(arrUint8.shape[1]).byteswap().tobytes()              # IHDR : Width
    hdrChk += numpy.uint32(arrUint8.shape[0]).byteswap().tobytes()              # IHDR : Height
    hdrChk += numpy.uint8(8).tobytes()                                          # IHDR : Bit depth
    hdrChk += numpy.uint8(colourType).tobytes()                                 # IHDR : Colour type
    hdrChk += numpy.uint8(0).tobytes()                                          # IHDR : Compression method
    hdrChk += numpy.uint8(0).tobytes()                                          # IHDR : Filter method
    hdrChk += numpy.uint8(0).tobytes()                                          # IHDR : Interlace method
    hdrChk += numpy.uint32(binascii.crc32(hdrChk[4:])).byteswap().tobytes()     # CRC-32

    # Make the IDAT chunk ...
    datChk = bytearray()
    datChk += numpy.uint32(0).byteswap().tobytes()                              # Length
    datChk += bytearray("IDAT", encoding = "ascii")                             # Chunk type
    datChk += createStream(
        arrUint8,
        arrInt16,
        calcAdaptive = calcAdaptive,
         calcAverage = calcAverage,
            calcNone = calcNone,
           calcPaeth = calcPaeth,
             calcSub = calcSub,
              calcUp = calcUp,
             choices = choices,
               debug = debug,
              levels = levels,
           memLevels = memLevels,
          strategies = strategies,
              wbitss = wbitss,
    )                                                                           # IDAT : Data
    datChk[:4] = numpy.uint32(len(datChk[8:])).byteswap().tobytes()             # Length
    datChk += numpy.uint32(binascii.crc32(datChk[4:])).byteswap().tobytes()     # CRC-32

    # Check if it is a paletted image ...
    if colourType == 3:
        # Make the PLTE chunk ...
        palChk = bytearray()
        palChk += numpy.uint32(palUint8.size).byteswap().tobytes()              # Length
        palChk += bytearray("PLTE", encoding = "ascii")                         # Chunk type
        for lvl in range(palUint8.shape[0]):
            palChk += palUint8[lvl, :].tobytes()                                # PLTE : Data
        palChk += numpy.uint32(binascii.crc32(palChk[4:])).byteswap().tobytes() # CRC-32

        # Prepend the PLTE chunk to the IDAT chunk (so that it is included in
        # the result) ...
        datChk = palChk + datChk

    # Check if the user has supplied a DPI ...
    if dpi is not None:
        # Convert the dots-per-inch to dots-per-metre ...
        dpm = round(dpi * 100.0 / 2.54)                                         # [#/m]

        # Make the pHYs chunk ...
        phyChk = bytearray()
        phyChk += numpy.uint32(9).byteswap().tobytes()                          # Length
        phyChk += bytearray("pHYs", encoding = "ascii")                         # Chunk type
        phyChk += numpy.uint32(dpm).byteswap().tobytes()                        # pHYs : Pixels per unit, x axis
        phyChk += numpy.uint32(dpm).byteswap().tobytes()                        # pHYs : Pixels per unit, y axis
        phyChk += numpy.uint8(1).tobytes()                                      # pHYs : Unit specifier
        phyChk += numpy.uint32(binascii.crc32(phyChk[4:])).byteswap().tobytes() # CRC-32

        # Prepend the pHYs chunk to the IDAT chunk (so that it is included in
        # the result) ...
        datChk = phyChk + datChk

    # Check if the user has supplied a last-modification time ...
    if modTime is not None:
        # Make the tIME chunk ...
        timChk = bytearray()
        timChk += numpy.uint32(7).byteswap().tobytes()                          # Length
        timChk += bytearray("tIME", encoding = "ascii")                         # Chunk type
        timChk += numpy.uint16(modTime.year).byteswap().tobytes()               # tIME : Year
        timChk += numpy.uint8(modTime.month).tobytes()                          # tIME : Month
        timChk += numpy.uint8(modTime.day).tobytes()                            # tIME : Day
        timChk += numpy.uint8(modTime.hour).tobytes()                           # tIME : Hour
        timChk += numpy.uint8(modTime.minute).tobytes()                         # tIME : Minute
        timChk += numpy.uint8(modTime.second).tobytes()                         # tIME : Second
        timChk += numpy.uint32(binascii.crc32(timChk[4:])).byteswap().tobytes() # CRC-32

        # Prepend the tIME chunk to the IDAT chunk (so that it is included in
        # the result) ...
        datChk = timChk + datChk

    # Make the IEND chunk ...
    endChk = bytearray()
    endChk += numpy.uint32(0).byteswap().tobytes()                              # Length
    endChk += bytearray("IEND", encoding = "ascii")                             # Chunk type
    endChk += numpy.uint32(binascii.crc32(endChk[4:])).byteswap().tobytes()     # CRC-32

    # Return answer ...
    return pngSig + hdrChk + datChk + endChk
