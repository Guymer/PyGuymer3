def save_array_as_PPM(img, fname):
    """
    Save an array as a PPM image.

    img -- a 3D NumPy array of type uint8 with shape (ny,nx,3)
    fname -- output file name
    """

    # Find image size ...
    ny, nx, nc = img.shape

    # Check image ...
    if not img.dtype == "uint8":
        raise TypeError("\"img\" must be a \"uint8\" array") from None
    if nc != 3:
        raise Exception("\"img\" must be a 3-channel array") from None

    # Write out PPM ...
    with open(fname, "wb") as fobj:
        fobj.write("P6 {0:d} {1:d} 255 ".format(nx, ny))
        img.tofile(fobj)
