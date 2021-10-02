def optimize_image(fname, kwArgCheck = None, debug = False, strip = False):
    """
    Please read the documentation for the four functions: "exiftool",
    "gifsicle", "jpegtran" and "optipng". It is not safe to keep on running the
    programs "gifsicle" and "jpegtran" on images, but it is safe to keep on
    running all of my wrapper functions on images instead.
    """

    # Import standard modules ...
    import os

    # Import sub-functions ...
    from .exiftool import exiftool
    from .gifsicle import gifsicle
    from .jpegtran import jpegtran
    from .optipng import optipng

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check that the image exists ...
    if not os.path.exists(fname):
        raise Exception("\"{:s}\" does not exist".format(fname)) from None

    # Extract file extension ...
    ext = os.path.splitext(fname)[1]

    # Optimize image depending the file extension ...
    if ext.lower() in [".gif"]:
        gifsicle(fname, debug = debug)
    elif ext.lower() in [".jpg", ".jpeg"]:
        jpegtran(fname, debug = debug)
    elif ext.lower() in [".png"]:
        optipng(fname)
    else:
        raise Exception("\"{:s}\" is not a recognised file extension".format(ext)) from None

    # Strip metadata ...
    if strip:
        exiftool(fname)
