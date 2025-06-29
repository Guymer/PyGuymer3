#!/usr/bin/env python3

# Define function ...
def createStreamAdaptive(
    arrUint8,
    arrInt16,
    /,
    *,
    debug = __debug__,
):
    """Create a PNG image data stream of an image using "adaptive" filtering (as
    defined in the PNG specification [2]_).

    Parameters
    ----------
    arrUint8 : numpy.ndarray
        A "height * width * colour" unsigned 8-bit integer NumPy array.
    arrInt16 : numpy.ndarray
        A "height * width * colour" signed 16-bit integer NumPy array.
    debug : bool, optional
        Print debug messages.

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

    # Import sub-functions ...
    from .paethFilter import paethFilter

    # **************************************************************************

    # Check input ...
    assert arrUint8.dtype == "uint8", f"the NumPy array is not 8-bit (\"{arrUint8.dtype}\")"
    assert arrInt16.dtype == "int16", f"the NumPy array is not 16-bit (\"{arrInt16.dtype}\")"
    assert arrUint8.ndim == 3, f"the NumPy array is not 3D (\"{arrUint8.ndim:d}\")"
    assert arrUint8.shape[2] == 3, "the NumPy array does not have 3 colour channels"
    assert arrUint8.shape == arrInt16.shape, "the NumPy arrays do not have the same shape"

    # **************************************************************************

    # Create short-hands ...
    ny, nx, nc = arrUint8.shape

    # Initialize arrays and bytearray ...
    scanline0 = numpy.zeros(
        (nc, nx),
        dtype = numpy.uint8,
    )
    scanline1 = numpy.zeros(
        (nc, nx),
        dtype = numpy.uint8,
    )
    scanline2 = numpy.zeros(
        (nc, nx),
        dtype = numpy.uint8,
    )
    scanline3 = numpy.zeros(
        (nc, nx),
        dtype = numpy.uint8,
    )
    scanline4 = numpy.zeros(
        (nc, nx),
        dtype = numpy.uint8,
    )
    stream = bytearray()

    # Loop over scanlines ...
    for iy in range(ny):
        # Initialize best answer and figure-of-merit ...
        bestStream = bytearray()
        minTotal = numpy.iinfo("uint64").max

        # Calculate scanline for "none" filter ...
        for ix in range(nx):
            scanline0[:, ix] = arrUint8[iy, ix, :]

        # Calculate scanline for "sub" filter ...
        for ix in range(nx):
            for ic in range(nc):
                if ix == 0:
                    p1 = numpy.int16(0)
                else:
                    p1 = arrInt16[iy, ix - 1, ic]
                diff = arrInt16[iy, ix, ic] - p1
                diff = numpy.mod(diff, 256)
                scanline1[ic, ix] = diff.astype(numpy.uint8)

        # Calculate scanline for "up" filter ...
        for ix in range(nx):
            for ic in range(nc):
                if iy == 0:
                    p1 = numpy.int16(0)
                else:
                    p1 = arrInt16[iy - 1, ix, ic]
                diff = arrInt16[iy, ix, ic] - p1
                diff = numpy.mod(diff, 256)
                scanline2[ic, ix] = diff.astype(numpy.uint8)

        # Calculate scanline for "average" filter ...
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
                scanline3[ic, ix] = diff.astype(numpy.uint8)

        # Calculate scanline for "Paeth" filter ...
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
                if ix == 0 or iy == 0:
                    p3 = numpy.int16(0)
                else:
                    p3 = arrInt16[iy - 1, ix - 1, ic]
                diff = arrInt16[iy, ix, ic] - paethFilter(p1, p2, p3)
                diff = numpy.mod(diff, 256)
                scanline4[ic, ix] = diff.astype(numpy.uint8)

        # Check if the "none" filter is likely to be the best stream ...
        if scanline0.sum() < minTotal:
            if debug:
                print(f"DEBUG: scanline = {iy:,d}; filter = 0 --> {scanline0.sum():,d} counts")
            bestStream = bytearray()
            bestStream += numpy.uint8(0).tobytes()
            for ix in range(nx):
                bestStream += scanline0[:, ix].tobytes()
            minTotal = scanline0.sum()

        # Check if the "sub" filter is likely to be the best stream ...
        if scanline1.sum() < minTotal:
            if debug:
                print(f"DEBUG: scanline = {iy:,d}; filter = 1 --> {scanline1.sum():,d} counts")
            bestStream = bytearray()
            bestStream += numpy.uint8(1).tobytes()
            for ix in range(nx):
                bestStream += scanline1[:, ix].tobytes()
            minTotal = scanline1.sum()

        # Check if the "up" filter is likely to be the best stream ...
        if scanline2.sum() < minTotal:
            if debug:
                print(f"DEBUG: scanline = {iy:,d}; filter = 2 --> {scanline2.sum():,d} counts")
            bestStream = bytearray()
            bestStream += numpy.uint8(2).tobytes()
            for ix in range(nx):
                bestStream += scanline2[:, ix].tobytes()
            minTotal = scanline2.sum()

        # Check if the "average" filter is likely to be the best stream ...
        if scanline3.sum() < minTotal:
            if debug:
                print(f"DEBUG: scanline = {iy:,d}; filter = 3 --> {scanline3.sum():,d} counts")
            bestStream = bytearray()
            bestStream += numpy.uint8(3).tobytes()
            for ix in range(nx):
                bestStream += scanline3[:, ix].tobytes()
            minTotal = scanline3.sum()

        # Check if the "Paeth" filter is likely to be the best stream ...
        if scanline4.sum() < minTotal:
            if debug:
                print(f"DEBUG: scanline = {iy:,d}; filter = 4 --> {scanline4.sum():,d} counts")
            bestStream = bytearray()
            bestStream += numpy.uint8(4).tobytes()
            for ix in range(nx):
                bestStream += scanline4[:, ix].tobytes()
            minTotal = scanline4.sum()

        # Check that a best stream was found ...
        assert len(bestStream) > 0, f"no best stream was found for scanline {iy:,d}"

        # Add the best stream for this scanline to the total stream ...
        stream += bestStream

    # Return answer ...
    return stream
