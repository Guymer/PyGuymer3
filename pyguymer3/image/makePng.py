#!/usr/bin/env python3

# Define function ...
def makePng(
    arrUint8,
    /,
    *,
       choices = "fastest",
         debug = __debug__,
           dpi = None,
        levels = None,
     memLevels = None,
       modTime = None,
    strategies = None,
        wbitss = None,
):
    """Make a PNG

    This function reads in a "height * width * colour" unsigned 8-bit integer
    NumPy array and returns the Python bytearray which is the binary source of
    the PNG file of the input.

    This function always calculates the PNG image data stream using all five
    filters (as defined in the PNG specification [2]_), as well as adaptive
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
    This function only creates 8-bit RGB images, without interlacing. It stores
    the entire image in a single "IDAT" chunk.

    According to the PNG specification [2]_:

        "The PNG datastream consists of a PNG signature followed by a sequence
        of chunks. Each chunk has a chunk type which specifies its function."

    This function writes out three of the four critical chunks: "IHDR", "IDAT"
    and "IEND". This function does not currently write out a "PLTE" chunk as it
    does not currently support writing paletted images. This function can
    optionally include an ancillary "pHYs" chunk and an ancillary "tIME" chunk
    if the user passes optional keyword arguments to populate them.

    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    .. [2] PNG Specification (Third Edition), https://www.w3.org/TR/png-3/
    """

    # Import standard modules ...
    import binascii
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

    # Check input ...
    assert arrUint8.dtype == "uint8", "the NumPy array is not 8-bit"
    assert arrUint8.ndim == 3, "the NumPy array does not have a colour dimension"
    assert arrUint8.shape[2] == 3, "the NumPy array does not have 3 colour channels"

    # **************************************************************************

    # Create short-hands ...
    ny, nx, _ = arrUint8.shape
    arrInt16 = arrUint8.astype(numpy.int16)

    # Make the file signature ...
    pngSig = bytearray()
    pngSig += binascii.unhexlify("89504E470D0A1A0A")

    # Make the IHDR chunk ...
    hdrChk = bytearray()
    hdrChk += numpy.uint32(13).byteswap().tobytes()                             # Length
    hdrChk += bytearray("IHDR", encoding = "ascii")                             # Chunk type
    hdrChk += numpy.uint32(nx).byteswap().tobytes()                             # IHDR : Width
    hdrChk += numpy.uint32(ny).byteswap().tobytes()                             # IHDR : Height
    hdrChk += numpy.uint8(8).tobytes()                                          # IHDR : Bit depth
    hdrChk += numpy.uint8(2).tobytes()                                          # IHDR : Colour type
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
           choices = choices,
             debug = debug,
            levels = levels,
         memLevels = memLevels,
        strategies = strategies,
            wbitss = wbitss,
    )                                                                           # IDAT : Data
    datChk[:4] = numpy.uint32(len(datChk[8:])).byteswap().tobytes()             # Length
    datChk += numpy.uint32(binascii.crc32(datChk[4:])).byteswap().tobytes()     # CRC-32

    # Check if the user has supplied a DPI ...
    if dpi is not None:
        # Convert the dots-per-inch to dots-per-metre ...
        dpm = round(dpi * 100.0 / 2.54)                                         # [#/m]

        # Make the PHYS chunk ...
        phyChk = bytearray()
        phyChk += numpy.uint32(9).byteswap().tobytes()                          # Length
        phyChk += bytearray("pHYs", encoding = "ascii")                         # Chunk type
        phyChk += numpy.uint32(dpm).byteswap().tobytes()                        # pHYs : Pixels per unit, x axis
        phyChk += numpy.uint32(dpm).byteswap().tobytes()                        # pHYs : Pixels per unit, y axis
        phyChk += numpy.uint8(1).tobytes()                                      # pHYs : Unit specifier
        phyChk += numpy.uint32(binascii.crc32(phyChk[4:])).byteswap().tobytes() # CRC-32

        # Prepend the PHYS chunk to the IDAT chunk (so that it is included in
        # the result) ...
        datChk = phyChk + datChk

    # Check if the user has supplied a last-modification time ...
    if modTime is not None:
        # Make the TIME chunk ...
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

        # Prepend the TIME chunk to the IDAT chunk (so that it is included in
        # the result) ...
        datChk = timChk + datChk

    # Make the IEND chunk ...
    endChk = bytearray()
    endChk += numpy.uint32(0).byteswap().tobytes()                              # Length
    endChk += bytearray("IEND", encoding = "ascii")                             # Chunk type
    endChk += numpy.uint32(binascii.crc32(endChk[4:])).byteswap().tobytes()     # CRC-32

    # Return answer ...
    return pngSig + hdrChk + datChk + endChk
