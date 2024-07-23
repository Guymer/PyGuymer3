#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.11/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # Import standard modules ...
    import argparse
    import json
    import os

    # Import special modules ...
    try:
        import cartopy
        cartopy.config.update(
            {
                "cache_dir" : os.path.expanduser("~/.local/share/cartopy_cache"),
            }
        )
    except:
        raise Exception("\"cartopy\" is not installed; run \"pip install --user Cartopy\"") from None
    try:
        import matplotlib
        matplotlib.rcParams.update(
            {
                       "backend" : "Agg",                                       # NOTE: See https://matplotlib.org/stable/gallery/user_interfaces/canvasagg.html
                    "figure.dpi" : 300,
                "figure.figsize" : (9.6, 7.2),                                  # NOTE: See https://github.com/Guymer/misc/blob/main/README.md#matplotlib-figure-sizes
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

    # **************************************************************************

    # Create argument parser and parse the arguments ...
    parser = argparse.ArgumentParser(
           allow_abbrev = False,
            description = "Demonstrate the nuances of finding the middle of a collection of locations.",
        formatter_class = argparse.ArgumentDefaultsHelpFormatter,
    )
    parser.add_argument(
        "--debug",
        action = "store_true",
          help = "print debug messages",
    )
    parser.add_argument(
        "--eps",
        default = 1.0e-12,
           dest = "eps",
           help = "the tolerance of the Vincenty formula iterations",
           type = float,
    )
    parser.add_argument(
        "--geodesic-convergence",
        default = 1.0e3,
           dest = "geodesicConv",
           help = "the Geodesic distance that defines the middle as being converged (in metres)",
           type = float,
    )
    parser.add_argument(
        "--nang",
        default = 361,
           help = "the number of angles around each circle",
           type = int,
    )
    parser.add_argument(
        "--nmax",
        default = 100,
           dest = "nmax",
           help = "the maximum number of the Vincenty formula iterations",
           type = int,
    )
    parser.add_argument(
        "--tolerance",
        default = 1.0e-10,
           dest = "tol",
           help = "the Euclidean distance that defines two points as being the same (in degrees)",
           type = float,
    )
    args = parser.parse_args()

    # **************************************************************************

    # Load data and convert to NumPy array ...
    with open("findMiddleOfLocs/lons.json", "rt", encoding = "utf-8") as fObj:
        lons = json.load(fObj)                                                  # [°]
    lons = numpy.array(lons, dtype = numpy.float64)                             # [°]

    # Load data and convert to NumPy array ...
    with open("findMiddleOfLocs/lats.json", "rt", encoding = "utf-8") as fObj:
        lats = json.load(fObj)                                                  # [°]
    lats = numpy.array(lats, dtype = numpy.float64)                             # [°]

    # Calculate convergence criteria ...
    euclideanConv = args.geodesicConv / pyguymer3.RESOLUTION_OF_EARTH           # [°]

    # **************************************************************************

    # Calculate the Euclidean bounding box ...
    midLon1, midLat1, maxDist1 = pyguymer3.geo.find_middle_of_locs(
        lons,
        lats,
          conv = euclideanConv,
         debug = args.debug,
           eps = args.eps,
        method = "EuclideanBox",
         nIter = 1000,
          nmax = args.nmax,
           pad = 10.0 * euclideanConv,
    )                                                                           # [°], [°], [°]
    EuclideanBox = shapely.geometry.point.Point(midLon1, midLat1).buffer(
        maxDist1,
        quad_segs = (args.nang - 1) // 4,
    )
    print(f"   EuclideanBox: ({midLon1:.6f}°, {midLat1:.6f}°) and {maxDist1:.6f}°.")

    # Calculate the Geodesic bounding box ...
    midLon2, midLat2, maxDist2 = pyguymer3.geo.find_middle_of_locs(
        lons,
        lats,
          conv = args.geodesicConv,
         debug = args.debug,
           eps = args.eps,
        method = "GeodesicBox",
         nIter = 1000,
          nmax = args.nmax,
           pad = 10.0 * args.geodesicConv,
    )                                                                           # [°], [°], [m]
    GeodesicBox = pyguymer3.geo.buffer(
        shapely.geometry.point.Point(midLon2, midLat2),
        maxDist2,
        debug = args.debug,
          eps = args.eps,
         fill = -1.0,
         nang = args.nang,
         nmax = args.nmax,
         simp = -1.0,
          tol = args.tol,
    )
    print(f"    GeodesicBox: ({midLon2:.6f}°, {midLat2:.6f}°) and {0.001 * maxDist2:,.1f} km.")

    # Calculate the Euclidean bounding circle ...
    midLon3, midLat3, maxDist3 = pyguymer3.geo.find_middle_of_locs(
        lons,
        lats,
          conv = euclideanConv,
         debug = args.debug,
           eps = args.eps,
        method = "EuclideanCircle",
         nIter = 1000,
          nmax = args.nmax,
           pad = 10.0 * euclideanConv,
    )                                                                           # [°], [°], [°]
    EuclideanCircle = shapely.geometry.point.Point(midLon3, midLat3).buffer(
        maxDist3,
        quad_segs = (args.nang - 1) // 4,
    )
    print(f"EuclideanCircle: ({midLon3:.6f}°, {midLat3:.6f}°) and {maxDist3:.6f}°.")

    # Calculate the Geodesic bounding circle ...
    midLon4, midLat4, maxDist4 = pyguymer3.geo.find_middle_of_locs(
        lons,
        lats,
          conv = args.geodesicConv,
         debug = args.debug,
           eps = args.eps,
        method = "GeodesicCircle",
         nIter = 1000,
          nmax = args.nmax,
           pad = 10.0 * args.geodesicConv,
    )                                                                           # [°], [°], [m]
    GeodesicCircle = pyguymer3.geo.buffer(
        shapely.geometry.point.Point(midLon4, midLat4),
        maxDist4,
        debug = args.debug,
          eps = args.eps,
         fill = -1.0,
         nang = args.nang,
         nmax = args.nmax,
         simp = -1.0,
          tol = args.tol,
    )
    print(f" GeodesicCircle: ({midLon4:.6f}°, {midLat4:.6f}°) and {0.001 * maxDist4:,.1f} km.")

    # **************************************************************************

    # Create figure ...
    fg = matplotlib.pyplot.figure(figsize = (2 * 12.8, 2 * 7.2))

    # Create axes ...
    axBot = []
    axTop = []
    for iCol in range(4):
        axBot.append(
            fg.add_subplot(
                2,
                4,
                iCol + 5,
            )
        )
        axTop.append(
            pyguymer3.geo.add_axis(
                fg,
                debug = args.debug,
                 dist = maxDist1 * pyguymer3.RESOLUTION_OF_EARTH,
                  eps = args.eps,
                index = iCol + 1,
                  lat = midLat1,
                  lon = midLon1,
                ncols = 4,
                 nmax = args.nmax,
                nrows = 2,
                  tol = args.tol,
            )
        )

    # Plot the data ...
    for iCol in range(4):
        axBot[iCol].scatter(
            lons,
            lats,
             color = (0.0, 0.0, 1.0, 1.0),
            zorder = 5.0,
        )
        axTop[iCol].scatter(
            lons,
            lats,
                color = (0.0, 0.0, 1.0, 1.0),
            transform = cartopy.crs.Geodetic(),
               zorder = 5.0,
        )

    # Plot the Euclidean bounding box and configure axes ...
    for poly in pyguymer3.geo.extract_polys(EuclideanBox):
        axBot[0].plot(
            numpy.array(poly.exterior.coords)[:, 0],
            numpy.array(poly.exterior.coords)[:, 1],
                color = (1.0, 0.0, 0.0, 1.0),
            linestyle = "solid",
            linewidth = 1.0,
        )
    axTop[0].add_geometries(
        pyguymer3.geo.extract_polys(EuclideanBox),
        cartopy.crs.PlateCarree(),
        edgecolor = (1.0, 0.0, 0.0, 1.0),
        facecolor = (1.0, 0.0, 0.0, 0.5),
        linestyle = "solid",
        linewidth = 1.0,
    )
    axBot[0].set_title(f"EuclideanBox: ({midLon1:.6f}°, {midLat1:.6f}°) and {maxDist1:.6f}°.")
    axTop[0].set_title(f"EuclideanBox: ({midLon1:.6f}°, {midLat1:.6f}°) and {maxDist1:.6f}°.")

    # Plot the Geodesic bounding box and configure axes ...
    for poly in pyguymer3.geo.extract_polys(GeodesicBox):
        axBot[1].plot(
            numpy.array(poly.exterior.coords)[:, 0],
            numpy.array(poly.exterior.coords)[:, 1],
                color = (1.0, 0.0, 0.0, 1.0),
            linestyle = "solid",
            linewidth = 1.0,
        )
    axTop[1].add_geometries(
        pyguymer3.geo.extract_polys(GeodesicBox),
        cartopy.crs.PlateCarree(),
        edgecolor = (1.0, 0.0, 0.0, 1.0),
        facecolor = (1.0, 0.0, 0.0, 0.5),
        linestyle = "solid",
        linewidth = 1.0,
    )
    axBot[1].set_title(f"GeodesicBox: ({midLon2:.6f}°, {midLat2:.6f}°) and {0.001 * maxDist2:,.1f} km.")
    axTop[1].set_title(f"GeodesicBox: ({midLon2:.6f}°, {midLat2:.6f}°) and {0.001 * maxDist2:,.1f} km.")

    # Plot the Euclidean bounding circle and configure axes ...
    for poly in pyguymer3.geo.extract_polys(EuclideanCircle):
        axBot[2].plot(
            numpy.array(poly.exterior.coords)[:, 0],
            numpy.array(poly.exterior.coords)[:, 1],
                color = (1.0, 0.0, 0.0, 1.0),
            linestyle = "solid",
            linewidth = 1.0,
        )
    axTop[2].add_geometries(
        pyguymer3.geo.extract_polys(EuclideanCircle),
        cartopy.crs.PlateCarree(),
        edgecolor = (1.0, 0.0, 0.0, 1.0),
        facecolor = (1.0, 0.0, 0.0, 0.5),
        linestyle = "solid",
        linewidth = 1.0,
    )
    axBot[2].set_title(f"EuclideanCircle: ({midLon3:.6f}°, {midLat3:.6f}°) and {maxDist3:.6f}°.")
    axTop[2].set_title(f"EuclideanCircle: ({midLon3:.6f}°, {midLat3:.6f}°) and {maxDist3:.6f}°.")

    # Plot the Geodesic bounding circle and configure axes ...
    for poly in pyguymer3.geo.extract_polys(GeodesicCircle):
        axBot[3].plot(
            numpy.array(poly.exterior.coords)[:, 0],
            numpy.array(poly.exterior.coords)[:, 1],
                color = (1.0, 0.0, 0.0, 1.0),
            linestyle = "solid",
            linewidth = 1.0,
        )
    axTop[3].add_geometries(
        pyguymer3.geo.extract_polys(GeodesicCircle),
        cartopy.crs.PlateCarree(),
        edgecolor = (1.0, 0.0, 0.0, 1.0),
        facecolor = (1.0, 0.0, 0.0, 0.5),
        linestyle = "solid",
        linewidth = 1.0,
    )
    axBot[3].set_title(f"GeodesicCircle: ({midLon4:.6f}°, {midLat4:.6f}°) and {0.001 * maxDist4:,.1f} km.")
    axTop[3].set_title(f"GeodesicCircle: ({midLon4:.6f}°, {midLat4:.6f}°) and {0.001 * maxDist4:,.1f} km.")

    # Configure axes ...
    for iCol in range(4):
        axBot[iCol].grid()
        axBot[iCol].set_aspect("equal")
        axBot[iCol].set_xlabel("Longitude [°]")
        axBot[iCol].set_xlim(138.0, 180.0)
        axBot[iCol].set_xticks(range(138, 182, 2))
        axBot[iCol].set_ylabel("Latitude [°]")
        axBot[iCol].set_ylim(-52.0, -10.0)
        axBot[iCol].set_yticks(range(-52, -8, 2))

    # **************************************************************************

    # Configure figure ...
    fg.suptitle("Photos From My Holiday")
    fg.tight_layout()

    # Save figure ...
    fg.savefig("findMiddleOfLocs/comparison.png")
    matplotlib.pyplot.close(fg)

    # Optimize PNG ...
    pyguymer3.image.optimize_image(
        "findMiddleOfLocs/comparison.png",
        debug = args.debug,
        strip = True,
    )
