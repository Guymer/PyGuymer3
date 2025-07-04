#!/usr/bin/env python3

# Define function ...
def images2gif(
    imgs,
    gif,
    /,
    *,
       chunksize = 1048576,
           debug = __debug__,
    exiftoolPath = None,
             fps = 25.0,
    gifsiclePath = None,
    jpegtranPath = None,
            mode = "RGB",
        optimize = True,
     optipngPath = None,
    screenHeight = -1,
     screenWidth = -1,
           strip = False,
         timeout = 60.0,
):
    """Convert a sequence of images to a GIF animation.

    This function makes a GIF animation from either a list of PIL Images or a
    list of file paths. The user is able to set the framerate.

    Parameters
    ----------
    imgs : list of PIL.Image.Image or list of str
        the list of input PIL Images or list of paths to the input images
    gif : str
        the path to the output GIF
    chunksize : int, optional
        the size of the chunks of any files which are read in (in bytes)
    debug : bool, optional
        print debug messages (default False)
    exiftoolPath : str, optional
        the path to the "exiftool" binary (if not provided then Python will attempt to
        find the binary itself)
    fps : float, optional
        the framerate, default 25.0
    gifsiclePath : str, optional
        the path to the "gifsicle" binary (if not provided then Python will attempt to
        find the binary itself)
    jpegtranPath : str, optional
        the path to the "jpegtran" binary (if not provided then Python will attempt to
        find the binary itself)
    mode : str, optional
        the mode of the outout GIF (default "RGB")
    optimize : bool, optional
        optimize the output GIF (default True)
    optipngPath : str, optional
        the path to the "optipng" binary (if not provided then Python will attempt to
        find the binary itself)
    screenHeight : int, optional
        the height of the screen to downscale the input images to fit within,
        currently only implemented if "imgs" is a list of str (default -1;
        integers less than 100 imply no downscaling)
    screenWidth : int, optional
        the width of the screen to downscale the input images to fit within,
        currently only implemented if "imgs" is a list of str (default -1;
        integers less than 100 imply no downscaling)
    strip : bool, optional
        strip metadata from the output GIF (default False)
    timeout : float, optional
        the timeout for any requests/subprocess calls

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
    from ..image import optimize_image

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

    # Save it as a GIF ...
    # NOTE: See https://pillow.readthedocs.io/en/stable/handbook/image-file-formats.html#gif
    tmpImgs[0].save(
        gif,
            append_images = tmpImgs[1:],
                 duration = round(1000.0 / fps),
                     loop = 0,
                 optimize = optimize,
                 save_all = True,
    )

    # Optimize GIF ...
    if strip:
        optimize_image(
            gif,
               chunksize = chunksize,
                   debug = debug,
            exiftoolPath = exiftoolPath,
            gifsiclePath = gifsiclePath,
            jpegtranPath = jpegtranPath,
             optipngPath = optipngPath,
                    pool = None,
                   strip = strip,
                 timeout = timeout,
        )
