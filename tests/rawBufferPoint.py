#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.8/library/multiprocessing.html#multiprocessing-programming
if __name__ == "__main__":
    # This is a test suite for "buffer_Point()” with:
    #     A) a point that span the whole numerical range;
    #     B) a point that cross the equator;
    #     C) a point that cross the anti-meridian;
    #     D) a point that cross a pole;
    #     E) a point that cross both the equator and the anti-meridian; and
    #     F) a point that cross both a pole and the anti-meridian.
    # Each point has a plot with both a top-down projection and a Robinson
    # projection so that you can check it.

    # Import special modules ...
    try:
        import matplotlib
        matplotlib.use("Agg")                                                   # NOTE: See https://matplotlib.org/stable/gallery/user_interfaces/canvasagg.html
        import matplotlib.pyplot
    except:
        raise Exception("\"matplotlib\" is not installed; run \"pip install --user matplotlib\"") from None
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None
    try:
        import shapely
        import shapely.geometry
        import shapely.ops
        import shapely.validation
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Import my modules ...
    try:
        import pyguymer3
        import pyguymer3.geo
        import pyguymer3.image
    except:
        raise Exception("\"pyguymer3\" is not installed; you need to have the Python module from https://github.com/Guymer/PyGuymer3 located somewhere in your $PYTHONPATH") from None

    print(f"Testing \"{pyguymer3.__path__[0]}\" ...")

    # Define points ...
    points = [
        (-180.0, +90.0, 1000.0e3), # Satisfies test A, C, D, F
        ( -90.0, +45.0, 1000.0e3), # Satisfies test A
        (   0.0,   0.0, 1000.0e3), # Satisfies test A, B
        ( +90.0, -45.0, 1000.0e3), # Satisfies test A
        (+180.0, -90.0, 1000.0e3), # Satisfies test A, C, D, F
        (+170.0, +10.0, 4000.0e3), # Satisfies test B, C, E
        (+170.0, +80.0, 4000.0e3), # Satisfies test C, D, F
    ]

    # Loop over points ...
    for i, (lon, lat, dist) in enumerate(points):
        # Determine file name ...
        fname = f"rawBufferPoint{i:d}.png"

        print(f" > Making \"{fname}\" ...")

        # Create figure and axis ...
        fg = matplotlib.pyplot.figure(figsize = (6, 3), dpi = 150)
        ax = fg.subplots(1, 1)

        # Buffer point ...
        points1 = numpy.zeros((1, 2), dtype = numpy.float64)
        points1[0, 0] = lon
        points1[0, 1] = lat
        points2 = pyguymer3.geo._buffer_points_crudely(points1, dist, nang = 19)

        wedges = []

        ring = shapely.geometry.LineString(points2[0, :, :])
        minLon, minLat, maxLon, maxLat = ring.bounds
        if minLon > lon:
            print(f"longitude: {minLon:.1f}° > {lon:.1f}°")
        if maxLon < lon:
            print(f"longitude: {maxLon:.1f}° < {lon:.1f}°")
        if minLat > lat:
            print(f"latitude: {minLat:.1f}° > {lat:.1f}°")
            wedge = shapely.geometry.polygon.Polygon(
                [
                    (-360.0, -90.0),
                    (+360.0, -90.0),
                    (+360.0, minLat),
                    (-360.0, minLat),
                    (-360.0, -90.0),
                ]
            )
            if not isinstance(wedge, shapely.geometry.polygon.Polygon):
                raise Exception("\"wedge\" is not a Polygon") from None
            if not wedge.is_valid:
                raise Exception(f"\"wedge\" is not a valid Polygon ({shapely.validation.explain_validity(wedge)})") from None
            if wedge.is_empty:
                raise Exception("\"wedge\" is an empty Polygon") from None
            wedges.append(wedge)
            ring = numpy.array(wedge.exterior)
            ax.plot(ring[:, 0], ring[:, 1], color = "C0", marker = "d")
        if maxLat < lat:
            print(f"latitude: {maxLat:.1f}° < {lat:.1f}°")
            wedge = shapely.geometry.polygon.Polygon(
                [
                    (-360.0, 90.0),
                    (+360.0, 90.0),
                    (+360.0, maxLat),
                    (-360.0, maxLat),
                    (-360.0, 90.0),
                ]
            )
            if not isinstance(wedge, shapely.geometry.polygon.Polygon):
                raise Exception("\"wedge\" is not a Polygon") from None
            if not wedge.is_valid:
                raise Exception(f"\"wedge\" is not a valid Polygon ({shapely.validation.explain_validity(wedge)})") from None
            if wedge.is_empty:
                raise Exception("\"wedge\" is an empty Polygon") from None
            wedges.append(wedge)
            ring = numpy.array(wedge.exterior)
            ax.plot(ring[:, 0], ring[:, 1], color = "C0", marker = "d")

        for iang in range(18):
            wedge = shapely.geometry.polygon.Polygon(
                [
                    (lon, lat),
                    (points2[0, iang    , 0], points2[0, iang    , 1]),
                    (points2[0, iang + 1, 0], points2[0, iang + 1, 1]),
                    (lon, lat),
                ]
            )
            if not isinstance(wedge, shapely.geometry.polygon.Polygon):
                raise Exception("\"wedge\" is not a Polygon") from None
            if not wedge.is_valid:
                raise Exception(f"\"wedge\" is not a valid Polygon ({shapely.validation.explain_validity(wedge)})") from None
            if wedge.is_empty:
                raise Exception("\"wedge\" is an empty Polygon") from None
            wedges.append(wedge)
            ring = numpy.array(wedge.exterior)
            ax.plot(ring[:, 0], ring[:, 1], color = "C1", marker = "d")
        wedges = shapely.ops.unary_union(wedges)
        if not isinstance(wedges, shapely.geometry.polygon.Polygon):
            raise Exception("\"wedges\" is not a Polygon") from None
        if not wedges.is_valid:
            raise Exception(f"\"wedges\" is not a valid Polygon ({shapely.validation.explain_validity(wedges)})") from None
        if wedges.is_empty:
            raise Exception("\"wedges\" is an empty Polygon") from None

        # Plot data ..
        ax.plot(points2[0, :, 0], points2[0, :, 1], color = "C2", marker = "d")

        # Configure figure ...
        fg.suptitle(f"({lon:.1f}°,{lat:.1f}°) buffered by {0.001 * dist:,.1f}km")

        # Configure axis ...
        ax.grid()
        ax.set_xlabel("lon [°]")
        ax.set_ylabel("lat [°]")

        # Save figure ...
        fg.savefig(fname, bbox_inches = "tight", dpi = 150, pad_inches = 0.1)
        matplotlib.pyplot.close(fg)

        # Optimize figure ...
        pyguymer3.image.optimize_image(fname, strip = True)
