#!/usr/bin/env python3

# Define function ...
def createStream(
    arrUint8,
    arrInt16,
    /,
    *,
    calcAdaptive = True,
     calcAverage = True,
        calcNone = True,
       calcPaeth = True,
         calcSub = True,
          calcUp = True,
         choices = "fastest",
           debug = __debug__,
          levels = None,
       memLevels = None,
      strategies = None,
          wbitss = None,
):
    """Compress the PNG image data stream

    This function loops over sets of settings and returns the smallest
    compressed PNG image data stream. See :py:func:`pyguymer3.image.makePng` for
    a discussion on how it does that.

    Parameters
    ----------
    arrUint8 : numpy.ndarray
        A "height * width * colour" unsigned 8-bit integer NumPy array.
    arrInt16 : numpy.ndarray
        A "height * width * colour" signed 16-bit integer NumPy array.
    calcAdaptive : bool, optional
        Calculate the compressed PNG image data stream using an adaptive filter
        type. Each of the five named filters is applied to a scanline and a
        predictiion is made as to which one will produce the smallest compressed
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
        See :py:func:`pyguymer3.image.makePng` for the documentation.
    debug : bool, optional
        Print debug messages.
    levels : None or list of int, optional
        See :py:func:`pyguymer3.image.makePng` for the documentation.
    memLevels : None or list of int, optional
        See :py:func:`pyguymer3.image.makePng` for the documentation.
    strategies : None or list of int, optional
        See :py:func:`pyguymer3.image.makePng` for the documentation.
    wbitss : None or list of int, optional
        See :py:func:`pyguymer3.image.makePng` for the documentation.

    Returns
    -------
    stream : bytearray
        The compressed PNG image data stream.

    Notes
    -----

    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    .. [2] PNG Specification (Third Edition), https://www.w3.org/TR/png-3/
    """

    # Import standard modules ...
    import zlib

    # Import special modules ...
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None

    # Import sub-functions ...
    from .createStreamAdaptive import createStreamAdaptive
    from .createStreamAverage import createStreamAverage
    from .createStreamNone import createStreamNone
    from .createStreamPaeth import createStreamPaeth
    from .createStreamSub import createStreamSub
    from .createStreamUp import createStreamUp

    # **************************************************************************

    # Check input ...
    assert arrUint8.dtype == "uint8", f"the NumPy array is not 8-bit (\"{arrUint8.dtype}\")"
    assert arrInt16.dtype == "int16", f"the NumPy array is not 16-bit (\"{arrInt16.dtype}\")"
    assert arrUint8.ndim == 3, f"the NumPy array is not 3D (\"{arrUint8.ndim:d}\")"
    assert arrUint8.shape[2] == 3, "the NumPy array does not have 3 colour channels"
    assert arrUint8.shape == arrInt16.shape, "the NumPy arrays do not have the same shape"

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

    # **************************************************************************

    # Initialize best answer and figure-of-merit ...
    bestStream = bytearray()
    minSize = numpy.iinfo("uint64").max                                         # [B]

    # Calculate streams ...
    streams = []
    if calcNone:
        streams.append(
            (
                0,
                createStreamNone(
                    arrUint8,
                    arrInt16,
                ),
            )
        )
    if calcSub:
        streams.append(
            (
                1,
                createStreamSub(
                    arrUint8,
                    arrInt16,
                ),
            )
        )
    if calcUp:
        streams.append(
            (
                2,
                createStreamUp(
                    arrUint8,
                    arrInt16,
                ),
            )
        )
    if calcAverage:
        streams.append(
            (
                3,
                createStreamAverage(
                    arrUint8,
                    arrInt16,
                ),
            )
        )
    if calcPaeth:
        streams.append(
            (
                4,
                createStreamPaeth(
                    arrUint8,
                    arrInt16,
                ),
            )
        )
    if calcAdaptive:
        streams.append(
            (
                5,
                createStreamAdaptive(
                    arrUint8,
                    arrInt16,
                    debug = debug,
                ),
            )
        )

    # Loop over streams ...
    for (filtType, stream,) in streams:
        # Loop over compression levels ...
        for level in levels:
            # Loop over window sizes ...
            for wbits in wbitss:
                # Check window size ...
                assert pow(2, wbits) <= 32768, f"the PNG specification only allows window sizes up to 32,768; you have asked for 2 ** {wbits:d}"

                # Loop over memory levels ...
                for memLevel in memLevels:
                    # Loop over strategies ...
                    for strategy in strategies:
                        # Make a compression object and compress the stream ...
                        # NOTE: On 28/Jun/2025, I replaced zlib.compressobj(...)
                        #       with zlib.compress(..., level = 9) and confirmed
                        #       that all five filters, and adaptive filtering,
                        #       produced binary identical PNG files to the (soon
                        #       to be legacy) function save_array_as_PNG().
                        zObj = zlib.compressobj(
                               level = level,
                            memLevel = memLevel,
                              method = zlib.DEFLATED,
                            strategy = strategy,
                               wbits = wbits,
                        )
                        possibleStream = bytearray()
                        possibleStream += zObj.compress(stream)
                        possibleStream += zObj.flush(zlib.Z_FINISH)

                        # Check if this compressed stream is the best ...
                        if len(possibleStream) < minSize:
                            if debug:
                                print(f"DEBUG: filter = {filtType:d}; compression level = {level:d}; window size = {wbits:2d}; memory level = {memLevel:d}; strategy = {strategy:d} --> {len(possibleStream):,d} bytes.")

                            # Overwrite the best ...
                            bestStream = bytearray()
                            bestStream += possibleStream
                            minSize = len(bestStream)                           # [B]

    # Check that a best stream was found ...
    assert len(bestStream) > 0, f"no best stream was found"

    # Return answer ...
    return bestStream
