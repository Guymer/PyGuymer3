def load_GPS_EXIF(fname, kwArgCheck = None, python = True):
    # Load sub-functions ...
    from .load_GPS_EXIF1 import load_GPS_EXIF1
    from .load_GPS_EXIF2 import load_GPS_EXIF2

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check what the user wants ...
    if python:
        # Will use the Python module "exifread" ...
        return load_GPS_EXIF1(fname)

    # Will use the binary "exiftool" ...
    return load_GPS_EXIF2(fname)