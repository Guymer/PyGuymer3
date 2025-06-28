#!/usr/bin/env python3

# Define function ...
def makePng(
    inputArrUint8,
    /,
    *,
       choices = "fastest",
         debug = __debug__,
        levels = None,
     memLevels = None,
    strategies = None,
        wbitss = None,
):
    """Make a PNG

    This function reads in a "height * width * colour" unsigned 8-bit integer
    NumPy array and returns the Python bytearray which is the binary source of
    the PNG file of the input.

    This function always calculates the PNG image data stream using all five
    filters (as defined in the PNG specification [2]_) and this function then
    uses the one filter which ends up having the smallest compressed size.

    This function also allows the user to declare sets of settings which are
    tried when compressing the PNG image data stream and this function then uses
    the one set of settings which ends up having the smallest compressed size.
    This allows the user to: either choose to spend CPU time trying different
    sets of settings in an attempt to find the one which produces the smallest
    compressed size for the supplied input; or try a stab in the dark at knowing
    the fastest (or best) set of settings.

    Parameters
    ----------
    inputArrUint8 : numpy.ndarray
        A "height * width * colour" unsigned 8-bit integer NumPy array.
    choices : str, optional
        If any of the settings are not passed (or passed as ``None``) then this
        string is used to set them. The accepted values are ``"fastest"``,
        ``"best"`` and ``"all"``.
    debug : bool, optional
        Print debug messages.
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
    does not currently support writing paletted images.

    The PNG specification [2]_ also says:

        "The encoder shall use only a single filter method for an interlaced PNG
        image, but may use different filter types for each scanline in a reduced
        image. An intelligent encoder can switch filters from one scanline to
        the next. The method for choosing which filter to employ is left to the
        encoder."

    By that definition, this encoder is not intelligent as it applies a single
    filter to the whole image. Future work may add a brute forcing capability,
    but at time of writing that is currently not implemented.

    The PNG specification [2]_ also says:

        "For best compression of truecolor and greyscale images, and if
        compression efficiency is valued over speed of compression, the
        recommended approach is adaptive filtering in which a filter type is
        chosen for each scanline. Each unique image will have a different set of
        filters which perform best for it. An encoder could try every
        combination of filters to find what compresses best for a given image.
        However, when an exhaustive search is unacceptable, here are some
        general heuristics which may perform well enough: compute the output
        scanline using all five filters, and select the filter that gives the
        smallest sum of absolute values of outputs. (Consider the output bytes
        as signed differences for this test.) This method usually outperforms
        any single fixed filter type choice."

    ... which is good to know if I ever implement it.

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
    assert inputArrUint8.dtype == "uint8", "the NumPy array is not 8-bit"
    assert inputArrUint8.ndim == 3, "the NumPy array does not have a colour dimension"
    assert inputArrUint8.shape[2] == 3, "the NumPy array does not have 3 colour channels"

    # **************************************************************************

    # Create short-hands ...
    ny, nx, _ = inputArrUint8.shape
    inputArrInt16 = inputArrUint8.astype(numpy.int16)

    # Make the file signature ...
    imageSig = bytearray()
    imageSig += binascii.unhexlify("89504E470D0A1A0A")

    # Make the IHDR chunk ...
    imageHdr = bytearray()
    imageHdr += numpy.uint32(13).byteswap().tobytes()                           # Length
    imageHdr += bytearray("IHDR", encoding = "ascii")                           # Chunk type
    imageHdr += numpy.uint32(nx).byteswap().tobytes()                           # IHDR : Width
    imageHdr += numpy.uint32(ny).byteswap().tobytes()                           # IHDR : Height
    imageHdr += numpy.uint8(8).tobytes()                                        # IHDR : Bit depth
    imageHdr += numpy.uint8(2).tobytes()                                        # IHDR : Colour type
    imageHdr += numpy.uint8(0).tobytes()                                        # IHDR : Compression method
    imageHdr += numpy.uint8(0).tobytes()                                        # IHDR : Filter method
    imageHdr += numpy.uint8(0).tobytes()                                        # IHDR : Interlace method
    imageHdr += numpy.uint32(binascii.crc32(imageHdr[4:])).byteswap().tobytes() # CRC-32

    # Make the IDAT chunk ...
    imageDat = bytearray()
    imageDat += numpy.uint32(0).byteswap().tobytes()                            # Length
    imageDat += bytearray("IDAT", encoding = "ascii")                           # Chunk type
    imageDat += createStream(
        inputArrUint8,
        inputArrInt16,
           choices = choices,
             debug = debug,
            levels = levels,
         memLevels = memLevels,
        strategies = strategies,
            wbitss = wbitss,
    )                                                                           # IDAT : Data
    imageDat[:4] = numpy.uint32(len(imageDat[8:])).byteswap().tobytes()         # Length
    imageDat += numpy.uint32(binascii.crc32(imageDat[4:])).byteswap().tobytes() # CRC-32

    # Make the IEND chunk ...
    imageEnd = bytearray()
    imageEnd += numpy.uint32(0).byteswap().tobytes()                            # Length
    imageEnd += bytearray("IEND", encoding = "ascii")                           # Chunk type
    imageEnd += numpy.uint32(binascii.crc32(imageEnd[4:])).byteswap().tobytes() # CRC-32

    # Return answer ...
    return imageSig + imageHdr + imageDat + imageEnd
