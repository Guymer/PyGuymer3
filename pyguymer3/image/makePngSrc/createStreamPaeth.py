#!/usr/bin/env python3

# Define function ...
def createStreamPaeth(
    inputArrUint8,
    inputArrInt16,
    /,
):
    # Import special modules ...
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None

    # Import sub-functions ...
    from .paethFilter import paethFilter

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
        # Calculate stream for "Paeth" filter ...
        stream += numpy.uint8(4).tobytes()
        for ix in range(nx):
            for ic in range(nc):
                if ix == 0:
                    p1 = numpy.int16(0)
                else:
                    p1 = inputArrInt16[iy, ix - 1, ic]
                if iy == 0:
                    p2 = numpy.int16(0)
                else:
                    p2 = inputArrInt16[iy - 1, ix, ic]
                if ix == 0 or iy == 0:
                    p3 = numpy.int16(0)
                else:
                    p3 = inputArrInt16[iy - 1, ix - 1, ic]
                diff = inputArrInt16[iy, ix, ic] - paethFilter(p1, p2, p3)
                diff = numpy.mod(diff, 256)
                row[ic, ix] = diff.astype(numpy.uint8)
            stream += row[:, ix].tobytes()

    # Return answer ...
    return stream
