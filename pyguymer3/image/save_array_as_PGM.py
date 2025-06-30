#!/usr/bin/env python3

# Define function ...
def save_array_as_PGM(
    img,
    fname,
    /,
):
    """
    Save an array as a PGM image.

    img -- a 2D NumPy array of type uint8 with shape (ny,nx)
    fname -- output file name
    """

    # Find image size ...
    ny, nx = img.shape

    # Check image ...
    if not img.dtype == "uint8":
        raise TypeError("\"img\" must be a \"uint8\" array") from None

    # Write out PGM ...
    with open(fname, "wb") as fObj:
        fObj.write(f"P5 {nx:d} {ny:d} 255 ".encode("utf-8"))
        img.tofile(fObj)
