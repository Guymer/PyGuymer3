#!/usr/bin/env python3

# Define function ...
def findExtent(
    fname0,
    /,
):
    """Find the extent of the dataset

    Parameters
    ----------
    fname0 : str
        the path to the dataset

    Returns
    -------
    minX : int
        the left edge
    maxX : int
        the right edge
    minY : int
        the bottom edge
    maxY : int
        the top edge

    Notes
    -----

    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import standard modules ...
    import io
    import re
    import zipfile

    # Import sub-functions ...
    from .loadASCIIheader import loadASCIIheader

    # **************************************************************************

    # Initialize limits ...
    maxX = -2**31
    maxY = -2**31
    minX =  2**31
    minY =  2**31

    # Compile regex to save time ...
    pattern = re.compile(r"data/[a-z]+/[a-z]+[0-9]+_OST50GRID_[0-9]+.zip")

    # Load dataset ...
    with zipfile.ZipFile(fname0, "r") as fObj0:
        # Loop over members ...
        for fname1 in fObj0.namelist():
            # Skip this member if it is not a sub-dataset ...
            if pattern.match(fname1) is None:
                continue

            # Determine sub-dataset key ...
            key = fname1.split("/")[-1].split("_")[0].upper()

            # Read sub-dataset into RAM so that it becomes seekable ...
            # NOTE: https://stackoverflow.com/a/12025492
            zipObj = io.BytesIO(fObj0.read(fname1))

            # Load sub-dataset ...
            with zipfile.ZipFile(zipObj, "r") as fObj1:
                # Read ASCII dataset into RAM so that it becomes seekable ...
                # NOTE: https://stackoverflow.com/a/12025492
                ascObj = io.BytesIO(fObj1.read(f"{key}.asc"))

                # Load header of ASCII dataset ...
                hdr = loadASCIIheader(ascObj)

                # Increment limits ...
                maxX = max(maxX, hdr["xllcorner"] // hdr["cellsize"] + hdr["ncols"])
                maxY = max(maxY, hdr["yllcorner"] // hdr["cellsize"] + hdr["nrows"])
                minX = min(minX, hdr["xllcorner"] // hdr["cellsize"])
                minY = min(minY, hdr["yllcorner"] // hdr["cellsize"])

    # Return answers ...
    return minX, maxX, minY, maxY
