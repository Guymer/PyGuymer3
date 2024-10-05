#!/usr/bin/env python3

# Define function ...
def images2webp(
    imgs,
    webp,
    /,
    *,
             exif = None,
              fps = 25.0,
         lossless = False,
           method = 6,
    minimize_size = True,
             mode = "RGB",
          quality = 100,
     screenHeight = -1,
      screenWidth = -1,
):
    """Convert a sequence of images to a WEBP animation.

    This function makes a WEBP animation from either a list of PIL Images or a
    list of file paths. The user is able to set the framerate.

    Parameters
    ----------
    imgs : list of PIL.Image.Image or list of str
        the list of input PIL Images or list of paths to the input images
    webp : str
        the path to the output WEBP
    exif : dict, optional
        a dictionary of EXIF data to save in the output WEBP (default None)
    fps : float, optional
        the framerate, default 25.0
    lossless : bool, optional
        save a lossless WEBP (default False)
    method : int, optional
        the method to use when saving the WEBP (default 6)
    minimize_size : bool, optional
        minimize the output size (default True)
    mode : str, optional
        the mode of the outout WEBP (default "RGB")
    quality : int, optional
        the quality of the output WEBP (default 100)
    screenHeight : int, optional
        the height of the screen to downscale the input images to fit within,
        currently only implemented if "imgs" is a list of str (default -1;
        integers less than 100 imply no downscaling)
    screenWidth : int, optional
        the width of the screen to downscale the input images to fit within,
        currently only implemented if "imgs" is a list of str (default -1;
        integers less than 100 imply no downscaling)

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

    # Import sub-functions ...
    from ..image import dict2exif

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

    # Save it as a WEBP ...
    # NOTE: See https://pillow.readthedocs.io/en/stable/handbook/image-file-formats.html#webp
    tmpImgs[0].save(
        webp,
            append_images = tmpImgs[1:],
                 duration = round(1000.0 / fps),
                     exif = dict2exif(exif, mode = mode),
                     loop = 0,
                 lossless = lossless,
                   method = method,
            minimize_size = minimize_size,
                  quality = quality,
                 save_all = True,
    )
