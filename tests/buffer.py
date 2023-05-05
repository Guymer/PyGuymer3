#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.11/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # This is a test suite for “geo.buffer()” with:
    #     A) a polygon that span the whole numerical range;
    #     B) a polygon that cross the equator;
    #     C) a polygon that cross the anti-meridian;
    #     D) a polygon that cross a pole;
    #     E) a polygon that cross both the equator and the anti-meridian; and
    #     F) a polygon that cross both a pole and the anti-meridian.
    # Each polygon has a plot with both a top-down projection and a Robinson
    # projection so that you can check it, along with an equirectangular plot.

    # Import special modules ...
    try:
        import cartopy
    except:
        raise Exception("\"cartopy\" is not installed; run \"pip install --user Cartopy\"") from None
    try:
        import geojson
        geojson.geometry.Geometry.__init__.__defaults__ = (None, False, 12)     # NOTE: See https://github.com/jazzband/geojson/issues/135#issuecomment-596509669
    except:
        raise Exception("\"geojson\" is not installed; run \"pip install --user geojson\"") from None
    try:
        import matplotlib
        matplotlib.rcParams.update(
            {
                   "backend" : "Agg",                                           # NOTE: See https://matplotlib.org/stable/gallery/user_interfaces/canvasagg.html
                "figure.dpi" : 300,
                 "font.size" : 8,
            }
        )
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

    # Configure functions ...
    debug = False
    fill = 1.0                                                                  # [°]
    fillSpace = "EuclideanSpace"
    nang = 361                                                                  # [#]
    simp = -1.0                                                                 # [°]
    tol = 1.0e-10                                                               # [°]

    # Define polygons ...
    polys = [
        (-180.0, +90.0,  1000000.0,  900000.0), # Satisfies test A, C, D, F
        ( -90.0, +45.0,  1000000.0,  900000.0), # Satisfies test A
        (   0.0,   0.0,  1000000.0,  900000.0), # Satisfies test A, B
        ( +90.0, -45.0,  1000000.0,  900000.0), # Satisfies test A
        (+180.0, -90.0,  1000000.0,  900000.0), # Satisfies test A, C, D, F
        (+170.0, +10.0,  1000000.0, 4000000.0), # Satisfies test B, C, E
        (+170.0, +80.0,  1000000.0, 4000000.0), # Satisfies test C, D, F
        (   0.0, +83.0,  1000000.0,  900000.0), # Satisfies test C, D, F
        ( -90.0, -83.0,  1000000.0,  900000.0), # Satisfies test C, D, F
        (   0.0,   0.0, 10000000.0,    1500.0), # Satisfies test A, B
    ]

    # Loop over polygons ...
    for i, (lon, lat, dist1, dist2) in enumerate(polys):
        # Determine file names ...
        fname = f"buffer{i:d}.png"
        jname = f"buffer{i:d}.geojson"

        print(f" > Making \"{jname}\" and \"{fname}\" ...")

        # Create figure ...
        fg = matplotlib.pyplot.figure(figsize = (6, 6))

        # Create axis ...
        ax1 = fg.add_subplot(
            2,
            2,
            1,
            projection = cartopy.crs.Robinson(),
        )

        # Configure axis ...
        ax1.set_global()
        pyguymer3.geo.add_coastlines(ax1, resolution = "c")
        pyguymer3.geo.add_map_background(ax1)
        pyguymer3.geo.add_horizontal_gridlines(
            ax1,
            locs = range(-90, 135, 45),
        )
        pyguymer3.geo.add_vertical_gridlines(
            ax1,
            locs = range(-180, 225, 45),
        )

        # Create axis ...
        ax2 = fg.add_subplot(
            2,
            2,
            2,
            projection = cartopy.crs.Orthographic(
                central_longitude = lon,
                 central_latitude = lat,
            ),
        )

        # Configure axis ...
        ax2.set_global()
        pyguymer3.geo.add_coastlines(ax2, resolution = "c")
        pyguymer3.geo.add_map_background(ax2)
        pyguymer3.geo.add_horizontal_gridlines(
            ax2,
            locs = range(-90, 135, 45),
        )
        pyguymer3.geo.add_vertical_gridlines(
            ax2,
            locs = range(-180, 225, 45),
        )

        # Create axis ...
        ax3 = fg.add_subplot(2, 2, (3, 4))

        # Configure axis ...
        ax3.grid()
        ax3.set_aspect("equal")
        ax3.set_xlabel("Longitude [°]")
        ax3.set_xlim(-180.0, +180.0)
        ax3.set_xticks(range(-180, 225, 45))
        ax3.set_ylabel("Latitude [°]")
        ax3.set_ylim(-90.0, +90.0)
        ax3.set_yticks(range(-90, 135, 45))

        # Create point ...
        point = shapely.geometry.point.Point(lon, lat)

        # Buffer Point and plot it thrice ...
        buff0 = pyguymer3.geo.buffer(
            point,
            dist1 + dist2,
                debug = debug,
                 fill = fill,
            fillSpace = fillSpace,
                 nang = nang,
                 simp = simp,
                  tol = tol,
        )
        ax1.add_geometries(
            pyguymer3.geo.extract_polys(buff0),
            cartopy.crs.PlateCarree(),
            edgecolor = (1.0, 0.0, 0.0, 1.0),
            facecolor = "none",
            linewidth = 1.0,
        )
        ax2.add_geometries(
            pyguymer3.geo.extract_polys(buff0),
            cartopy.crs.PlateCarree(),
            edgecolor = (1.0, 0.0, 0.0, 1.0),
            facecolor = "none",
            linewidth = 1.0,
        )
        for poly in pyguymer3.geo.extract_polys(buff0):
            coords = numpy.array(poly.exterior.coords)                          # [°]
            ax3.plot(
                coords[:, 0],
                coords[:, 1],
                color = (1.0, 0.0, 0.0, 1.0),
            )

        # Buffer Point and plot it twice ...
        buff1 = pyguymer3.geo.buffer(
            point,
            dist1,
                debug = debug,
                 fill = fill,
            fillSpace = fillSpace,
                 nang = nang,
                 simp = simp,
                  tol = tol,
        )
        ax1.add_geometries(
            pyguymer3.geo.extract_polys(buff1),
            cartopy.crs.PlateCarree(),
            edgecolor = (0.0, 1.0, 0.0, 1.0),
            facecolor = "none",
            linewidth = 1.0,
        )
        ax2.add_geometries(
            pyguymer3.geo.extract_polys(buff1),
            cartopy.crs.PlateCarree(),
            edgecolor = (0.0, 1.0, 0.0, 1.0),
            facecolor = "none",
            linewidth = 1.0,
        )
        for poly in pyguymer3.geo.extract_polys(buff1):
            coords = numpy.array(poly.exterior.coords)                          # [°]
            ax3.plot(
                coords[:, 0],
                coords[:, 1],
                color = (0.0, 1.0, 0.0, 1.0),
            )

        # Buffer Polygon and plot it thrice ...
        buff2 = pyguymer3.geo.buffer(
            buff1,
            dist2,
                debug = debug,
                 fill = fill,
            fillSpace = fillSpace,
                 nang = nang,
                 simp = simp,
                  tol = tol,
        )
        ax1.add_geometries(
            pyguymer3.geo.extract_polys(buff2),
            cartopy.crs.PlateCarree(),
            edgecolor = (0.0, 0.0, 1.0, 1.0),
            facecolor = (0.0, 0.0, 1.0, 0.5),
            linewidth = 1.0,
        )
        ax2.add_geometries(
            pyguymer3.geo.extract_polys(buff2),
            cartopy.crs.PlateCarree(),
            edgecolor = (0.0, 0.0, 1.0, 1.0),
            facecolor = (0.0, 0.0, 1.0, 0.5),
            linewidth = 1.0,
        )
        for poly in pyguymer3.geo.extract_polys(buff2):
            coords = numpy.array(poly.exterior.coords)                          # [°]
            ax3.plot(
                coords[:, 0],
                coords[:, 1],
                color = (0.0, 0.0, 1.0, 1.0),
            )

        # Save GeoJSON ...
        with open(jname, "wt", encoding = "utf-8") as fObj:
            geojson.dump(
                buff2,
                fObj,
                ensure_ascii = False,
                      indent = 4,
                   sort_keys = True,
            )

        # Configure figure ...
        fg.suptitle(f"({lon:.1f},{lat:.1f}) buffered by {0.001 * dist1:,.1f}km & {0.001 * dist2:,.1f}km\nred = {0.001 * (dist1 + dist2):,.1f}km; green = {0.001 * dist1:,.1f}km; blue = {0.001 * dist1:,.1f}km & {0.001 * dist2:,.1f}km")
        fg.tight_layout()

        # Save figure ...
        fg.savefig(fname)
        matplotlib.pyplot.close(fg)

        # Optimize PNG ..
        pyguymer3.image.optimize_image(fname, strip = True)
