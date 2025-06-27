#!/usr/bin/env python3

# Define function ...
def createStreamSub(
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
        # Calculate stream for "sub" filter ...
        stream += numpy.uint8(1).tobytes()
        for ix in range(nx):
            for ic in range(nc):
                if ix == 0:
                    p1 = numpy.int16(0)
                else:
                    p1 = inputArrInt16[iy, ix - 1, ic]
                diff = inputArrInt16[iy, ix, ic] - p1
                diff = numpy.mod(diff, 256)
                row[ic, ix] = diff.astype(numpy.uint8)
            stream += row[:, ix].tobytes()

    # Return answer ...
    return stream
