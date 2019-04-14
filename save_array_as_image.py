def save_array_as_image(img0, fname, form = "png", scale = False, pc_bot = 0.0, pc_top = 0.0, ct = "fire"):
    """
    Saves an array as an image with optional scaling and/or colour mapping.
    Currently only "png" and "ppm" formats are available.

    img0: a 2D NumPy array of any type with shape (ny,nx)
    fname: output file name
    form: output file format
    scale: does the input need scaling?
    pc_bot: the percentage to clip off the bottom of the histogram (if scaling
        is requested)
    pc_top: the percentage to clip off the top of the histogram (if scaling is
        requested)
    ct: the colour table to apply (if 3-channel output is requested)
    """

    # Load modules ...
    import json
    import numpy
    import os

    # Load sub-functions ...
    from .save_array_as_PPM import save_array_as_PPM
    from .save_array_as_PNG import save_array_as_PNG

    # Load colour tables ...
    colour_tables = json.load(
        open(
            os.path.join(
                os.path.dirname(__file__),
                "data",
                "json",
                "colour_tables.json"
            ),
            "rt"
        )
    )

    # Create uint8 image that will be passed to the external function ...
    img2 = numpy.empty((img0.shape[0], img0.shape[1], 3), dtype = numpy.uint8)

    # Check if scaling is required ...
    if scale:
        # Check input values ...
        if pc_bot < 0.0:
            raise Exception("pc_bot < 0.0")
        if pc_top < 0.0:
            raise Exception("pc_top < 0.0")
        if (pc_bot + pc_top) > 100.0:
            raise Exception("(pc_bot + pc_top) > 100.0")

        # Create copy and find the percentiles ...
        img1 = img0.astype(numpy.float64)
        p_lo = numpy.percentile(img1, pc_bot)
        p_hi = numpy.percentile(img1, 100.0 - pc_top)

        # Scale the image, clip scale image, convert scaled image to correct type, clean up ...
        img1 = 255.0 * (img1 - p_lo) / (p_hi - p_lo)
        numpy.place(img1, img1 > 255.0, 255.0)
        numpy.place(img1, img1 < 0.0, 0.0)
        for ix in range(img0.shape[1]):
            for iy in range(img0.shape[0]):
                img2[iy, ix, :] = colour_tables[ct][img1[iy, ix].astype(numpy.uint8)][:]
    else:
        # Convert image to correct type ...
        for ix in range(img0.shape[1]):
            for iy in range(img0.shape[0]):
                img2[iy, ix, :] = colour_tables[ct][img0[iy, ix].astype(numpy.uint8)][:]

    # Save image ...
    if form == "png":
        save_array_as_PNG(img2, fname, ftype_req = 4)
    elif form == "ppm":
        save_array_as_PPM(img2, fname)
    else:
        raise Exception("unknown format was requested")
