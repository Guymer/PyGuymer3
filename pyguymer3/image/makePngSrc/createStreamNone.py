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
        # Calculate stream for "none" filter ...
        stream += numpy.uint8(0).tobytes()
        for ix in range(nx):
            scanline[:, ix] = inputArrUint8[iy, ix, :]
            stream += scanline[:, ix].tobytes()

    # Return answer ...
    return stream
