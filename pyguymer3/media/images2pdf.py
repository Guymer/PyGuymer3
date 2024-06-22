#!/usr/bin/env python3

# Define function ...
def images2pdf(imgs, pdf, /, *, author = "My Author", keywords = "My Keywords", mode = "RGB", optimize = True, progressive = False, quality = 95, resolution = 300, screenHeight = -1, screenWidth = -1, subject = "My Subject", title = "My Title"):
    """Convert a sequence of images to a PDF slide pack.

    This function makes a PDF slide pack from either a list of PIL Images or a
    list of file paths.

    Parameters
    ----------
    imgs : list of PIL.Image.Image or list of str
        the list of input PIL Images or list of paths to the input images
    pdf : str
        the path to the output PDF
    author : str, optional
        The author field in the output PDF metadata
    keywords : str, optional
        The keywords field in the output PDF metadata
    mode : str, optional
        the mode of the JPGs in the outout PDF
    optimize : bool, optional
        optimize the JPGs in the output PDF
    progressive : bool, optional
        save progressive JPGs in the output PDF
    quality : int, optional
        the quality of the JPGs in the output PDF
    resolution : int, optional
        the resolution of the JPGs in the output PDF (has no effect on the JPGs,
        but the physical size of the PDF is derived from the pixel size of the
        JPGs)
    screenHeight : int, optional
        the height of the screen to downscale the input images to fit within,
        currently only implemented if "imgs" is a list of str (integers less
        than 100 imply no downscaling)
    screenWidth : int, optional
        the width of the screen to downscale the input images to fit within,
        currently only implemented if "imgs" is a list of str (integers less
        than 100 imply no downscaling)
    subject : str, optional
        The subject field in the output PDF metadata
    title : str, optional
        The title field in the output PDF metadata

    Notes
    -----
    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import special modules ...
    try:
        import PIL
        import PIL.Image
        PIL.Image.MAX_IMAGE_PIXELS = 1024 * 1024 * 1024                         # [px]
    except:
        raise Exception("\"PIL\" is not installed; run \"pip install --user Pillow\"") from None

    # **************************************************************************

    # Initialize list ...
    tmpImgs = []

    # Loop over input ...
    for img in imgs:
        # Find out what the user supplied ...
        match img:
            case str():
                # Open image as RGB (even if it is paletted) ...
                with PIL.Image.open(img) as iObj:
                    tmpImg = iObj.convert("RGB")

                # Check if the user wants to scale the image down to fit within
                # a screen size ...
                if screenWidth >= 100 and screenHeight >= 100:
                    # Resize image in place ...
                    tmpImg.thumbnail(
                        (screenWidth, screenHeight),
                        resample = PIL.Image.Resampling.LANCZOS,
                    )

                # Convert it to whatever mode the user asked for ...
                tmpImgs.append(tmpImg.convert(mode))
            case PIL.Image.Image():
                # Convert image to whatever mode the user asked for ...
                tmpImgs.append(img.convert(mode))
            case _:
                # Crash ...
                raise TypeError(f"\"img\" is an unexpected type ({repr(type(img))})") from None

    # Save it as a PDF ...
    # NOTE: See https://pillow.readthedocs.io/en/stable/handbook/image-file-formats.html#pdf
    tmpImgs[0].save(
        pdf,
        append_images = tmpImgs[1:],
               author = author,
             keywords = keywords,
             optimize = optimize,                                               # NOTE: Passed to the JPEG encoder.
          progressive = progressive,                                            # NOTE: Passed to the JPEG encoder.
              quality = quality,                                                # NOTE: Passed to the JPEG encoder.
           resolution = resolution,
             save_all = True,
              subject = subject,
                title = title,
    )
