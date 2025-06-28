#!/usr/bin/env python3

# Define function ...
def createStreamAverage(
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

    # Check input ...
    assert inputArrUint8.dtype == "uint8", "the NumPy array is not 8-bit"
    assert inputArrInt16.dtype == "int16", "the NumPy array is not 16-bit"
    assert inputArrUint8.ndim == 3, "the NumPy array does not have a colour dimension"
    assert inputArrUint8.shape[2] == 3, "the NumPy array does not have 3 colour channels"
    assert inputArrUint8.shape == inputArrInt16.shape, "the NumPy arrays do not have the same shape"

    # **************************************************************************

    # Create short-hands ...
    ny, nx, nc = inputArrUint8.shape

    # Initialize array and bytearray ...
    scanline = numpy.zeros(
        (nc, nx),
        dtype = numpy.uint8,
    )
    stream = bytearray()

    # Loop over scanlines ...
    for iy in range(ny):
        # Calculate stream for "average" filter ...
        stream += numpy.uint8(3).tobytes()
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
                diff = inputArrInt16[iy, ix, ic] - ((p1 + p2) // numpy.int16(2))
                diff = numpy.mod(diff, 256)
                scanline[ic, ix] = diff.astype(numpy.uint8)
            stream += scanline[:, ix].tobytes()

    # Return answer ...
    return stream
