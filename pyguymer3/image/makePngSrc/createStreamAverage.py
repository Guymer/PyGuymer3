#!/usr/bin/env python3

# Define function ...
def createStreamAverage(
    arrUint8,
    arrInt16,
    /,
):
    """Create a PNG image data stream of an image using the "average" filter (as
    defined in the PNG specification [2]_).

    Parameters
    ----------
    arrUint8 : numpy.ndarray
        A "height * width * colour" unsigned 8-bit integer NumPy array.
    arrInt16 : numpy.ndarray
        A "height * width * colour" signed 16-bit integer NumPy array.

    Returns
    -------
    stream : bytearray
        The PNG image data stream.

    Notes
    -----

    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    .. [2] PNG Specification (Third Edition), https://www.w3.org/TR/png-3/
    """

    # Import special modules ...
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None

    # **************************************************************************

    # Check input ...
    assert arrUint8.dtype == "uint8", f"the NumPy array is not 8-bit (\"{arrUint8.dtype}\")"
    assert arrInt16.dtype == "int16", f"the NumPy array is not 16-bit (\"{arrInt16.dtype}\")"
    assert arrUint8.ndim == 3, f"the NumPy array is not 3D (\"{arrUint8.ndim:d}\")"
    match arrUint8.shape[2]:
        case 1:
            pass
        case 3:
            pass
        case _:
            raise ValueError(f"the NumPy array does not have either 1 or 3 colour channels (\"{arrUint8.shape[2]:d}\")") from None
    assert arrUint8.shape == arrInt16.shape, "the NumPy arrays do not have the same shape"

    # **************************************************************************

    # Create short-hands ...
    ny, nx, nc = arrUint8.shape

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
                    p1 = arrInt16[iy, ix - 1, ic]
                if iy == 0:
                    p2 = numpy.int16(0)
                else:
                    p2 = arrInt16[iy - 1, ix, ic]
                diff = arrInt16[iy, ix, ic] - ((p1 + p2) // numpy.int16(2))
                diff = numpy.mod(diff, 256)
                scanline[ic, ix] = diff.astype(numpy.uint8)
            stream += scanline[:, ix].tobytes()

    # Return answer ...
    return stream
