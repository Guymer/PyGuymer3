#!/usr/bin/env python3

# Define function ...
def _makePng(
    inputArrUint8,
    /,
    *,
         debug = __debug__,
        levels = None,
     memLevels = None,
          mode = "fastest",
    strategies = None,
        wbitss = None,
):
    # Import standard modules ...
    import binascii
    import zlib

    # Import special modules ...
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None

    # Import sub-functions ...
    from ._makePngSrc import createStream

    # **************************************************************************

    # Populate compression levels if the user has not ...
    if levels is None:
        match mode:
            case "fastest":
                levels = [0,]
            case "best":
                levels = [9,]
            case "all":
                levels = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9,]
            case _:
                raise ValueError(f"\"mode\" was an unexpected value (\"{mode}\")") from None

    # Populate memory levels if the user has not ...
    if memLevels is None:
        match mode:
            case "fastest":
                memLevels = [1,]
            case "best":
                memLevels = [9,]
            case "all":
                memLevels = [1, 2, 3, 4, 5, 6, 7, 8, 9,]
            case _:
                raise ValueError(f"\"mode\" was an unexpected value (\"{mode}\")") from None

    # Populate strategies if the user has not ...
    if strategies is None:
        match mode:
            case "fastest":
                strategies = [zlib.Z_DEFAULT_STRATEGY,]
            case "best":
                strategies = [zlib.Z_DEFAULT_STRATEGY,]
            case "all":
                strategies = [zlib.Z_DEFAULT_STRATEGY, zlib.Z_FILTERED, zlib.Z_HUFFMAN_ONLY, zlib.Z_RLE, zlib.Z_FIXED,]
            case _:
                raise ValueError(f"\"mode\" was an unexpected value (\"{mode}\")") from None

    # Populate widths if the user has not ...
    if wbitss is None:
        match mode:
            case "fastest":
                wbitss = [9,]
            case "best":
                wbitss = [15,]
            case "all":
                wbitss = [9, 10, 11, 12, 13, 14, 15,]
            case _:
                raise ValueError(f"\"mode\" was an unexpected value (\"{mode}\")") from None

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
             debug = debug,
            levels = levels,
         memLevels = memLevels,
              mode = mode,
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
