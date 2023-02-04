#!/usr/bin/env python3

# Define function ...
def save_array_as_PNG(img, fname, /, *, ftype_req = -1):
    """
    Save an array as a PNG image.

    img -- a 3D NumPy array of type uint8 with shape (ny,nx,3)
    fname -- output file name
    ftype_req -- filter type to be used

    The PNG specification defines 5 different possible filters which are
    numbered 0 to 4 (inclusively). Filter #0 is "no filtering". If the user
    defines "ftype_req" as one of the identifying integers then that filter will
    be used for the entire PNG file. If the user defines "ftype_req" as "-1" (or
    does not define "ftype_req" at all) then adaptive filtering will be used
    whereby an attempt is made to predict which filtering method will yield the
    smallest compressed stream.
    """

    # Import standard modules ...
    import binascii
    import zlib

    # Import special modules ...
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None

    # Import sub-functions ...
    from .paeth_filter import paeth_filter

    # Find image size ...
    ny, nx, nc = img.shape

    # Check image ...
    if not img.dtype == "uint8":
        raise TypeError("\"img\" must be a \"uint8\" array") from None
    if nc != 3:
        raise Exception("\"img\" must be a 3-channel array") from None

    # Try opening the PNG ...
    with open(fname, "wb") as fObj:
        # **********************************************************************
        # *                        WRITE THE SIGNATURE                         *
        # **********************************************************************

        fObj.write(binascii.unhexlify("89504E470D0A1A0A"))

        # **********************************************************************
        # *                  CREATE "IHDR" CHUNK AND WRITE IT                  *
        # **********************************************************************

        ihdr = bytearray()
        ihdr += numpy.uint32(13).byteswap().tobytes()                           # Length
        ihdr += bytearray("IHDR", encoding = "ascii")                           # Chunk type
        ihdr += numpy.uint32(nx).byteswap().tobytes()                           # IHDR : Width
        ihdr += numpy.uint32(ny).byteswap().tobytes()                           # IHDR : Height
        ihdr += numpy.uint8(8).byteswap().tobytes()                             # IHDR : Bit depth
        ihdr += numpy.uint8(2).byteswap().tobytes()                             # IHDR : Colour type
        ihdr += numpy.uint8(0).byteswap().tobytes()                             # IHDR : Compression method
        ihdr += numpy.uint8(0).byteswap().tobytes()                             # IHDR : Filter method
        ihdr += numpy.uint8(0).byteswap().tobytes()                             # IHDR : Interlace method
        ihdr += numpy.uint32(binascii.crc32(ihdr[4:])).byteswap().tobytes()     # CRC-32
        fObj.write(ihdr)
        del ihdr

        # **********************************************************************
        # *                  CREATE "IDAT" CHUNK AND WRITE IT                  *
        # **********************************************************************

        idat = bytearray()
        idat += numpy.uint32(0).byteswap().tobytes()                            # Length
        idat += bytearray("IDAT", encoding = "ascii")                           # Chunk type
        stream = bytearray()

        # Loop over rows ...
        for iy in range(ny):
            row = numpy.zeros((5, nc, nx), dtype = numpy.uint8)

            # Calculate stream for "none" filter (if required) ...
            if ftype_req in [-1, 0]:
                ftype = 0
                for ix in range(nx):
                    row[ftype, :, ix] = img[iy, ix, :]

            # Calculate stream for "sub" filter (if required) ...
            if ftype_req in [-1, 1]:
                ftype = 1
                for ix in range(nx):
                    for ic in range(nc):
                        if ix == 0:
                            p1 = numpy.int16(0)
                        else:
                            p1 = img[iy, ix - 1, ic].astype(numpy.int16)
                        diff = img[iy, ix, ic].astype(numpy.int16) - p1
                        diff = numpy.mod(diff, 256)
                        row[ftype, ic, ix] = diff.astype(numpy.uint8)

            # Calculate stream for "up" filter (if required) ...
            if ftype_req in [-1, 2]:
                ftype = 2
                for ix in range(nx):
                    for ic in range(nc):
                        if iy == 0:
                            p1 = numpy.int16(0)
                        else:
                            p1 = img[iy - 1, ix, ic].astype(numpy.int16)
                        diff = img[iy, ix, ic].astype(numpy.int16) - p1
                        diff = numpy.mod(diff, 256)
                        row[ftype, ic, ix] = diff.astype(numpy.uint8)

            # Calculate stream for "average" filter (if required) ...
            if ftype_req in [-1, 3]:
                ftype = 3
                for ix in range(nx):
                    for ic in range(nc):
                        if ix == 0:
                            p1 = numpy.int16(0)
                        else:
                            p1 = img[iy, ix - 1, ic].astype(numpy.int16)
                        if iy == 0:
                            p2 = numpy.int16(0)
                        else:
                            p2 = img[iy - 1, ix, ic].astype(numpy.int16)
                        diff = img[iy, ix, ic].astype(numpy.int16) - ((p1 + p2) // numpy.int16(2))
                        diff = numpy.mod(diff, 256)
                        row[ftype, ic, ix] = diff.astype(numpy.uint8)

            # Calculate stream for "Paeth" filter (if required) ...
            if ftype_req in [-1, 4]:
                ftype = 4
                for ix in range(nx):
                    for ic in range(nc):
                        if ix == 0:
                            p1 = numpy.int16(0)
                        else:
                            p1 = img[iy, ix - 1, ic].astype(numpy.int16)
                        if iy == 0:
                            p2 = numpy.int16(0)
                        else:
                            p2 = img[iy - 1, ix, ic].astype(numpy.int16)
                        if ix == 0 or iy == 0:
                            p3 = numpy.int16(0)
                        else:
                            p3 = img[iy - 1, ix - 1, ic].astype(numpy.int16)
                        diff = img[iy, ix, ic].astype(numpy.int16) - paeth_filter(p1, p2, p3).astype(numpy.int16)
                        diff = numpy.mod(diff, 256)
                        row[ftype, ic, ix] = diff.astype(numpy.uint8)

            # Figure out which stream to use ...
            if ftype_req == -1:
                tmp1 = numpy.uint64(255 * nx)
                for ftype in range(5):
                    tmp2 = row[ftype, :, :].astype(numpy.uint64).sum()
                    if tmp2 < tmp1:
                        tmp1 = tmp2
                        ftype_best = ftype
            else:
                ftype_best = ftype_req

            # Use the best/requested stream for this row ...
            stream += numpy.uint8(ftype_best).byteswap().tobytes()
            for ix in range(nx):
                stream += row[ftype_best, :, ix].byteswap().tobytes()

            # Clean up ...
            del row

        idat += zlib.compress(stream, level = 9)                                # IDAT : Data
        idat[0:4] = numpy.uint32(len(idat[8:])).byteswap().tobytes()            # Length
        idat += numpy.uint32(binascii.crc32(idat[4:])).byteswap().tobytes()     # CRC-32
        fObj.write(idat)
        del idat

        # **********************************************************************
        # *                  CREATE "IEND" CHUNK AND WRITE IT                  *
        # **********************************************************************

        iend = bytearray()
        iend += numpy.uint32(0).byteswap().tobytes()                            # Length
        iend += bytearray("IEND", encoding = "ascii")                           # Chunk type
        iend += numpy.uint32(binascii.crc32(iend[4:])).byteswap().tobytes()     # CRC-32
        fObj.write(iend)
        del iend
