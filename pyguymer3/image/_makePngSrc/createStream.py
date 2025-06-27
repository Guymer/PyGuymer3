#!/usr/bin/env python3

# Define function ...
def createStream(
    inputArrUint8,
    inputArrInt16,
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
    import copy
    import zlib

    # Import sub-functions ...
    from .createStreamAverage import createStreamAverage
    from .createStreamNone import createStreamNone
    from .createStreamPaeth import createStreamPaeth
    from .createStreamSub import createStreamSub
    from .createStreamUp import createStreamUp

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

    # Initialize best answer and figure-of-merit ...
    bestStream = bytearray()
    minSize = 999999999999                                                      # [B]

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
        ]
    ):
        # Loop over compression levels ...
        for level in levels:
            # Loop over widths ...
            for wbits in wbitss:
                # Loop over memory levels ...
                for memLevel in memLevels:
                    # Loop over strategies ...
                    for strategy in strategies:
                        # Make a compression object and compress the stream ...
                        zObj = zlib.compressobj(
                               level = level,
                              method = zlib.DEFLATED,
                               wbits = wbits,
                            memLevel = memLevel,
                            strategy = strategy,
                        )
                        possibleStream = zObj.compress(stream)
                        possibleStream += zObj.flush(zlib.Z_FINISH)

                        # Check if this compressed stream is the best ...
                        if len(possibleStream) < minSize:
                            if debug:
                                print(f"filter = {iFilter:d}; level = {level:d}; wbits = {wbits:2d}; memLevel = {memLevel:d}; strategy = {strategy:d} --> {len(possibleStream):,d} bytes")

                            # Overwrite the best ...
                            bestStream = copy.copy(possibleStream)
                            minSize = len(bestStream)                           # [B]

    # Return answer ...
    return bestStream
