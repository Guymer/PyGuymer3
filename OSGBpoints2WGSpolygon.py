def OSGBpoints2WGSpolygon(points):
    # Import special modules ...
    try:
        import convertbng
        import convertbng.util
    except:
        raise Exception("run \"pip install --user convertbng\"")
    try:
        import shapely
        import shapely.geometry
    except:
        raise Exception("run \"pip install --user shapely\"")

    # Convert list of OSGB points into lists of OSGB eastings and OSGB northings ...
    easts, norths = zip(*points)

    # Convert lists of OSGB eastings and OSGB northings into lists of WGS
    # longitudes and WGS latitudes ...
    lons, lats = convertbng.util.convert_lonlat(easts, norths)                  # [°], [°]

    # Convert lists of WGS longitudes and WGS latitudes into list of WGS points
    # and return as WGS polygon ...
    return shapely.geometry.polygon.Polygon(zip(lons, lats))
