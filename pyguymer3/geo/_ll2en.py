def _ll2en(lon, lat):
    """
    Convert a longitude,latitude pair to a easting,northing pair
    """

    # Import special modules ...
    try:
        import cartopy
    except:
        raise Exception("\"cartopy\" is not installed; run \"pip install --user Cartopy\"") from None

    # Return answer ...
    return cartopy.crs.OSGB().transform_point(lon, lat, cartopy.crs.Geodetic())
