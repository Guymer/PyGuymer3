#!/usr/bin/env python3

# Define function ...
def createStreamNone(
    inputArrUint8,
    inputArrInt16,
    /,
):
    # Import special modules ...
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None

    # **************************************************************************

    # Create short-hands ...
    ny, nx, nc = inputArrUint8.shape

    # Initialize array and bytearray ...
    row = numpy.zeros(
        (nc, nx),
        dtype = numpy.uint8,
    )
    stream = bytearray()

    # Loop over rows ...
    for iy in range(ny):
        # Calculate stream for "none" filter ...
        stream += numpy.uint8(0).tobytes()
        for ix in range(nx):
            row[:, ix] = inputArrUint8[iy, ix, :]
            stream += row[:, ix].tobytes()

    # Return answer ...
    return stream
