#!/usr/bin/env python3

# Define function ...
def save_array_as_image(
    img0,
    fname,
    /,
    *,
       chunksize = 1048576,
              ct = "grey",
           debug = __debug__,
    exiftoolPath = None,
            form = "png",
    gifsiclePath = None,
    jpegtranPath = None,
     optipngPath = None,
          pc_bot = 0.0,
          pc_top = 0.0,
           scale = False,
         timeout = 60.0,
):
    """Save an array as an image

    This function accepts a NumPy array, with optional scaling and/or colour
    mapping, and saves it as an image.Currently only "png" and "ppm" formats are
    available.

    Parameters
    ----------
    img0 : numpy.ndarray
        a 2D NumPy array of any type with shape (ny,nx)
    fname : str
        output file name
    ct : str, optional
        the colour table to apply (the default is no colour mapping, i.e.,
        greyscale)
    exiftoolPath : str, optional
        the path to the "exiftool" binary (if not provided then Python will attempt to
        find the binary itself)
    form : str, optional
        output image format (default "png")
    gifsiclePath : str, optional
        the path to the "gifsicle" binary (if not provided then Python will attempt to
        find the binary itself)
    jpegtranPath : str, optional
        the path to the "jpegtran" binary (if not provided then Python will attempt to
        find the binary itself)
    optipngPath : str, optional
        the path to the "optipng" binary (if not provided then Python will attempt to
        find the binary itself)
    pc_bot : float, optional
        the percentage to clip off the bottom of the histogram, if scaling is
        requested (default 0.0)
    pc_top : float, optional
        the percentage to clip off the top of the histogram, if scaling is
        requested (default 0.0)
    scale : bool, optional
        Does the input need scaling? If not, then the input array must be ≥ 0
        and ≤ 255. (default False)
    timeout : float, optional
        the timeout for any requests/subprocess calls

    Notes
    -----
    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import modules ...
    import json
    import os

    # Import special modules ...
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None

    # Import sub-functions ...
    from .optimize_image import optimize_image
    from .save_array_as_PPM import save_array_as_PPM
    from .save_array_as_PNG import save_array_as_PNG

    # Find image size ...
    ny, nx = img0.shape                                                         # [px], [px]

    # Load colour tables ...
    with open(f"{os.path.dirname(__file__)}/../data/json/colourTables.json", "rt", encoding = "utf-8") as fObj:
        cts = json.load(fObj)

    # Create uint8 image that will be passed to the external function ...
    img2 = numpy.empty((ny, nx, 3), dtype = numpy.uint8)

    # Check if scaling is required ...
    if scale:
        # Check input values ...
        if pc_bot < 0.0:
            raise Exception("pc_bot < 0.0") from None
        if pc_top < 0.0:
            raise Exception("pc_top < 0.0") from None
        if (pc_bot + pc_top) > 100.0:
            raise Exception("(pc_bot + pc_top) > 100.0") from None

        # Create copy and find the percentiles ...
        img1 = img0.astype(numpy.float64)
        p_lo = numpy.percentile(img1, pc_bot)
        p_hi = numpy.percentile(img1, 100.0 - pc_top)

        # Scale the image, clip scale image, convert scaled image to correct
        # type, clean up ...
        img1 = 255.0 * (img1 - p_lo) / (p_hi - p_lo)
        numpy.place(img1, img1 > 255.0, 255.0)
        numpy.place(img1, img1 <   0.0,   0.0)
        for ix in range(nx):
            for iy in range(ny):
                img2[iy, ix, :] = cts[ct][img1[iy, ix].astype(numpy.uint8)][:]
    else:
        # Convert image to correct type ...
        for ix in range(nx):
            for iy in range(ny):
                img2[iy, ix, :] = cts[ct][img0[iy, ix].astype(numpy.uint8)][:]

    # Save image ...
    match form:
        case "png":
            save_array_as_PNG(
                img2,
                fname,
                ftype_req = 4,
            )
            optimize_image(
                fname,
                   chunksize = chunksize,
                       debug = debug,
                exiftoolPath = exiftoolPath,
                gifsiclePath = gifsiclePath,
                jpegtranPath = jpegtranPath,
                 optipngPath = optipngPath,
                        pool = None,
                       strip = True,
                     timeout = timeout,
            )
        case "ppm":
            save_array_as_PPM(
                img2,
                fname,
            )
        case _:
            # Crash ...
            raise ValueError(f"\"form\" is an unexpected value ({repr(form)})") from None
