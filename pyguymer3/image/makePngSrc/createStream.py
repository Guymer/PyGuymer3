#!/usr/bin/env python3

# Define function ...
def createStream(
    inputArrUint8,
    inputArrInt16,
    /,
    *,
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
    inputArrUint8 : numpy.ndarray
        A "height * width * colour" unsigned 8-bit integer NumPy array.
    inputArrInt16 : numpy.ndarray
        A "height * width * colour" signed 16-bit integer NumPy array.
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
    assert inputArrUint8.dtype == "uint8", "the NumPy array is not 8-bit"
    assert inputArrInt16.dtype == "int16", "the NumPy array is not 16-bit"
    assert inputArrUint8.ndim == 3, "the NumPy array does not have a colour dimension"
    assert inputArrUint8.shape[2] == 3, "the NumPy array does not have 3 colour channels"
    assert inputArrUint8.shape == inputArrInt16.shape, "the NumPy arrays do not have the same shape"

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

    # Loop over streams ...
    for iFilter, stream in enumerate(
        [
            createStreamNone(
                inputArrUint8,
                inputArrInt16,
            ),
            createStreamSub(
                inputArrUint8,
                inputArrInt16,
            ),
            createStreamUp(
                inputArrUint8,
                inputArrInt16,
            ),
            createStreamAverage(
                inputArrUint8,
                inputArrInt16,
            ),
            createStreamPaeth(
                inputArrUint8,
                inputArrInt16,
            ),
            createStreamAdaptive(
                inputArrUint8,
                inputArrInt16,
                debug = debug,
            ),
        ]
    ):
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
                                print(f"DEBUG: filter = {iFilter:d}; compression level = {level:d}; window size = {wbits:2d}; memory level = {memLevel:d}; strategy = {strategy:d} --> {len(possibleStream):,d} bytes")

                            # Overwrite the best ...
                            bestStream = bytearray()
                            bestStream += possibleStream
                            minSize = len(bestStream)                           # [B]

    # Check that a best stream was found ...
    assert len(bestStream) > 0, f"no best stream was found"

    # Return answer ...
    return bestStream
