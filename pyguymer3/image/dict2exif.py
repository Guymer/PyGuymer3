#!/usr/bin/env python3

# Define function ...
def dict2exif(
    exif,
    /,
    *,
    mode = "RGB",
):
    """Convert a dictionary to an EXIF class

    This function accepts a dictionary of key/value pairs and uses it to
    construct an EXIF class suitable for PIL to write to an image upon saving.

    Parameters
    ----------
    exif : dict
        the dictionary
    mode : str, optional
        the mode of the temporary image that is created to initialize the EXIF class

    Returns
    -------
    exifClass : PIL.Image.Exif
        the EXIF class

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
        import PIL.ExifTags
        import PIL.Image
        PIL.Image.MAX_IMAGE_PIXELS = 1024 * 1024 * 1024                         # [px]
    except:
        raise Exception("\"PIL\" is not installed; run \"pip install --user Pillow\"") from None

    # Create an empty EXIF class (by obtaining the EXIF data from a blank image) ...
    # NOTE: See https://pillow.readthedocs.io/en/stable/reference/Image.html#PIL.Image.Image.getexif
    # NOTE: See https://pillow.readthedocs.io/en/stable/releasenotes/6.0.0.html#added-exif-class
    exifClass = PIL.Image.new(mode, (1, 1)).getexif()

    # Check if the user supplied any EXIF tags ...
    if exif is not None:
        # Loop over user-supplied EXIF tags ...
        for userKey, userValue in exif.items():
            # Initialize flag ...
            found = False

            # Loop over standard EXIF tags ...
            # NOTE: See https://www.exiftool.org/TagNames/EXIF.html
            for exifKey, exifValue in PIL.ExifTags.TAGS.items():
                # Skip this standard EXIF tag if it is not this user-supplied
                # EXIF tag ...
                if exifValue != userKey:
                    continue

                # Set flag and standard EXIF tag then stop looping ...
                found = True
                exifClass[exifKey] = userValue
                break

            # Check if the standard EXIF tag was not found for this
            # user-supplied EXIF tag ...
            if not found:
                print(f"WARNING: No standard EXIF tag was found for the user-supplied EXIF tag \"{userKey}\".")

    # Return answer ...
    return exifClass
