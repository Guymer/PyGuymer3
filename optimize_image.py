def optimize_image(fname, debug = False, strip = False):
    # Import standard modules ...
    import os

    # Load sub-functions ...
    from .exiftool import exiftool
    from .gifsicle import gifsicle
    from .jpegtran import jpegtran
    from .optipng import optipng

    # Check that the image exists ...
    if not os.path.exists(fname):
        raise Exception("\"{:s}\" does not exist".format(fname))

    # Extract file extension ...
    ext = os.path.splitext(fname)[1]

    # Optimize image depending the file's extension ...
    if ext.lower() in [".gif"]:
        gifsicle(fname, debug = debug)
    elif ext.lower() in [".jpg", ".jpeg"]:
        jpegtran(fname, debug = debug)
    elif ext.lower() in [".png"]:
        optipng(fname)
    else:
        raise Exception("\"{:s}\" is not a recognised file extension".format(ext))

    # Strip metadata ...
    if strip:
        exiftool(fname)
