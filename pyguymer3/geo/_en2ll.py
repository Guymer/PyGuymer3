def _en2ll(east, north):
    """
    Convert a easting,northing pair to a longitude,latitude pair
    """

    # Import special modules ...
    try:
        import cartopy
    except:
        raise Exception("\"cartopy\" is not installed; run \"pip install --user Cartopy\"") from None

    # Return answer ...
    return cartopy.crs.Geodetic().transform_point(east, north, cartopy.crs.OSGB())
