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
        default = 10000.0,
           dest = "geodesicConv",
           help = "the *initial* Geodesic distance that defines the middle as being converged (in metres)",
           type = float,
    )
    parser.add_argument(
        "--nAng",
        default = 361,
           dest = "nAng",
           help = "the number of angles around each circle",
           type = int,
    )
    parser.add_argument(
        "--nDiv",
        default = 100,
           dest = "nDiv",
           help = "the number of divisions when showing the shape of the surface",
           type = int,
    )
    parser.add_argument(
        "--nIter",
        default = 1000000,
           dest = "nIter",
           help = "the maximum number of iterations (particularly the Vincenty formula)",
           type = int,
    )
    parser.add_argument(
        "--nRefine",
        default = 6,
           dest = "nRefine",
           help = "the number of refinements to make (each refinement halves the \"geodesic-convergence\" distance)",
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

    print(f"The *initial* Geodesic convergence criteria is {0.001 * args.geodesicConv:,.1f} km.")
    print(f"The *initial* Euclidean convergence criteria is {euclideanConv:.6f}°.")

    # **************************************************************************

    # Calculate the Euclidean bounding box ...
    midLon1, midLat1, maxDist1 = pyguymer3.geo.find_middle_of_locs(
        lons,
        lats,
          conv = euclideanConv,                                                 # ~10 km
         debug = args.debug,
           eps = args.eps,
        method = "EuclideanBox",
         nIter = args.nIter,
           pad = euclideanConv,                                                 # ~10 km
    )                                                                           # [°], [°], [°]
    EuclideanBox = shapely.geometry.point.Point(midLon1, midLat1).buffer(
        maxDist1,
        quad_segs = (args.nAng - 1) // 4,
    )
    print(f"   EuclideanBox: ({midLon1:.6f}°, {midLat1:.6f}°) and {maxDist1:.6f}° (inc. padding).")

    # Calculate the Geodesic bounding box ...
    midLon2, midLat2, maxDist2 = pyguymer3.geo.find_middle_of_locs(
        lons,
        lats,
           conv = args.geodesicConv,                                            # 10 km
          debug = args.debug,
            eps = args.eps,
         method = "GeodesicBox",
          nIter = args.nIter,
        nRefine = args.nRefine,                                                 # 156.25 m
            pad = args.geodesicConv,                                            # 10 km
    )                                                                           # [°], [°], [m]
    GeodesicBox = pyguymer3.geo.buffer(
        shapely.geometry.point.Point(midLon2, midLat2),
        maxDist2,
        debug = args.debug,
          eps = args.eps,
         fill = -1.0,
         nang = args.nAng,
        nIter = args.nIter,
         simp = -1.0,
          tol = args.tol,
    )
    print(f"    GeodesicBox: ({midLon2:.6f}°, {midLat2:.6f}°) and {0.001 * maxDist2:,.1f} km (inc. padding).")

    # Calculate the Euclidean bounding circle ...
    midLon3, midLat3, maxDist3 = pyguymer3.geo.find_middle_of_locs(
        lons,
        lats,
           conv = euclideanConv,                                                # ~10 km
          debug = args.debug,
            eps = args.eps,
         method = "EuclideanCircle",
          nIter = args.nIter,
        nRefine = args.nRefine,                                                 # ~156.25 m
            pad = euclideanConv,                                                # ~10 km
    )                                                                           # [°], [°], [°]
    EuclideanCircle = shapely.geometry.point.Point(midLon3, midLat3).buffer(
        maxDist3,
        quad_segs = (args.nAng - 1) // 4,
    )
    print(f"EuclideanCircle: ({midLon3:.6f}°, {midLat3:.6f}°) and {maxDist3:.6f}° (inc. padding).")

    # Calculate the Geodesic bounding circle ...
    midLon4, midLat4, maxDist4 = pyguymer3.geo.find_middle_of_locs(
        lons,
        lats,
           conv = args.geodesicConv,                                            # 10 km
          debug = args.debug,
            eps = args.eps,
         method = "GeodesicCircle",
          nIter = args.nIter,
        nRefine = args.nRefine,                                                 # 156.25 m
            pad = args.geodesicConv,                                            # 10 km
    )                                                                           # [°], [°], [m]
    GeodesicCircle = pyguymer3.geo.buffer(
        shapely.geometry.point.Point(midLon4, midLat4),
        maxDist4,
        debug = args.debug,
          eps = args.eps,
         fill = -1.0,
         nang = args.nAng,
        nIter = args.nIter,
         simp = -1.0,
          tol = args.tol,
    )
    print(f" GeodesicCircle: ({midLon4:.6f}°, {midLat4:.6f}°) and {0.001 * maxDist4:,.1f} km (inc. padding).")

    # **************************************************************************

    print("Making \"findMiddleOfLocs/comparison.json\" ...")

    # Populate database ...
    db = {
           "EuclideanBox" : {
             "lon" : midLon1,
             "lat" : midLat1,
            "dist" : maxDist1,
        },
            "GeodesicBox" : {
             "lon" : midLon2,
             "lat" : midLat2,
            "dist" : maxDist2,
        },
        "EuclideanCircle" : {
             "lon" : midLon3,
             "lat" : midLat3,
            "dist" : maxDist3,
        },
         "GeodesicCircle" : {
             "lon" : midLon4,
             "lat" : midLat4,
            "dist" : maxDist4,
        },
    }

    # Save database ...
    with open("findMiddleOfLocs/comparison.json", "wt", encoding = "utf-8") as fObj:
        json.dump(
            db,
            fObj,
            ensure_ascii = False,
                  indent = 4,
               sort_keys = True,
        )

    # **************************************************************************

    print("Making \"findMiddleOfLocs/comparison.png\" ...")

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
                nIter = args.nIter,
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
    axBot[0].set_title(f"EuclideanBox: ({midLon1:.6f}°, {midLat1:.6f}°) and {maxDist1:.6f}° (inc. padding).")
    axTop[0].set_title(f"EuclideanBox: ({midLon1:.6f}°, {midLat1:.6f}°) and {maxDist1:.6f}° (inc. padding).")

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
    axBot[1].set_title(f"GeodesicBox: ({midLon2:.6f}°, {midLat2:.6f}°) and {0.001 * maxDist2:,.1f} km (inc. padding).")
    axTop[1].set_title(f"GeodesicBox: ({midLon2:.6f}°, {midLat2:.6f}°) and {0.001 * maxDist2:,.1f} km (inc. padding).")

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
    axBot[2].set_title(f"EuclideanCircle: ({midLon3:.6f}°, {midLat3:.6f}°) and {maxDist3:.6f}° (inc. padding).")
    axTop[2].set_title(f"EuclideanCircle: ({midLon3:.6f}°, {midLat3:.6f}°) and {maxDist3:.6f}° (inc. padding).")

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
    axBot[3].set_title(f"GeodesicCircle: ({midLon4:.6f}°, {midLat4:.6f}°) and {0.001 * maxDist4:,.1f} km (inc. padding).")
    axTop[3].set_title(f"GeodesicCircle: ({midLon4:.6f}°, {midLat4:.6f}°) and {0.001 * maxDist4:,.1f} km (inc. padding).")

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

    # **************************************************************************

    # Find the extent of the bounding box of all four methods (with some
    # padding) ...
    minLon =  180.0                                                             # [°]
    maxLon = -180.0                                                             # [°]
    minLat =   90.0                                                             # [°]
    maxLat =  -90.0                                                             # [°]
    for method, info in db.items():
        minLon = min(minLon, info["lon"])                                       # [°]
        maxLon = max(maxLon, info["lon"])                                       # [°]
        minLat = min(minLat, info["lat"])                                       # [°]
        maxLat = max(maxLat, info["lat"])                                       # [°]
    minLon -= euclideanConv                                                     # [°]
    maxLon += euclideanConv                                                     # [°]
    minLat -= 1.5 * euclideanConv                                               # [°]
    maxLat += 1.5 * euclideanConv                                               # [°]

    # Calculate the ranges and the minimum range ...
    lonRange = maxLon - minLon                                                  # [°]
    latRange = maxLat - minLat                                                  # [°]
    maxRange = max(lonRange, latRange)                                          # [°]

    # Create some axes to survey the bounding box ...
    # NOTE: Make the fence panels the same Euclidean size in both axes.
    lonsDiv = numpy.linspace(
        minLon,
        maxLon,
        dtype = numpy.float64,
          num = 1 + round(args.nDiv * lonRange / maxRange),
    )                                                                           # [°]
    latsDiv = numpy.linspace(
        minLat,
        maxLat,
        dtype = numpy.float64,
          num = 1 + round(args.nDiv * latRange / maxRange),
    )                                                                           # [°]

    # Find the maximum Geodesic distance over the bounding box and convert to
    # useful units ...
    maxDist = numpy.zeros((latsDiv.size, lonsDiv.size), dtype = numpy.float64)  # [m]
    for iLon in range(lonsDiv.size):
        for iLat in range(latsDiv.size):
            maxDist[iLat, iLon] = pyguymer3.geo.max_dist(
                lons,
                lats,
                lonsDiv[iLon],
                latsDiv[iLat],
                  eps = args.eps,
                nIter = args.nIter,
                space = "GeodesicSpace",
            )                                                                   # [m]
    maxDist *= 0.001                                                            # [km]

    # **************************************************************************

    print("Making \"findMiddleOfLocs/locations.png\" ...")

    # Create figure ...
    fg = matplotlib.pyplot.figure(figsize = (12.8, 7.2))

    # Create axis ...
    ax = fg.add_subplot()

    # Plot data ...
    im = ax.pcolormesh(
        lonsDiv,
        latsDiv,
        maxDist,
           cmap = "jet",
        shading = "nearest",
         zorder = 1.0,
    )
    ax.contour(
        lonsDiv,
        latsDiv,
        maxDist,
        colors = "white",
        levels = numpy.linspace(
            maxDist.min() +  2.0,
            maxDist.min() + 20.0,
            num = 10,
        ),
        zorder = 1.5,
    )

    # Plot locations ...
    for method, info in db.items():
        if "Geodesic" in method:
            label = f'{method}\n{0.001 * info["dist"]:,.1f} km (inc. padding)'
        else:
            label = f'{method}\n{info["dist"]:.6f}° (inc. padding)'
        x = info["lon"]                                                         # [°]
        if x > 0.5 * (minLon + maxLon):
            x -= 3.0 * euclideanConv                                            # [°]
        else:
            x += 3.0 * euclideanConv                                            # [°]
        y = info["lat"]                                                         # [°]
        if "Geodesic" in method:
            y -= 0.5 * euclideanConv                                            # [°]
        else:
            y += 0.5 * euclideanConv                                            # [°]
        ax.annotate(
            label,
            (info["lon"], info["lat"]),
                    arrowprops = {
                "edgecolor" : "black",
                "facecolor" : "white",
                "linewidth" : 1.0,
            },
                          bbox = {
                "edgecolor" : "black",
                "facecolor" : "white",
                "linewidth" : 1.0,
            },
                          color = "black",
            horizontalalignment = "center",
              verticalalignment = "center",
                         xytext = (x, y),
                         zorder = 2.0,
        )
        ax.scatter(
            [info["lon"]],
            [info["lat"]],
            edgecolor = "black",
            facecolor = "gold",
            linewidth = 1.0,
               marker = "*",
                    s = 100.0,
               zorder = 2.5,
        )

    # Add colour bar ...
    cb = fg.colorbar(
        im,
                 ax = ax,
        orientation = "vertical",
    )

    # Configure colour bar ...
    cb.set_label("Geodesic Distance (excl. padding) [km]")

    # Configure axis ...
    ax.grid()
    ax.set_aspect("equal")
    ax.set_title("How Different Are The Methods?")
    ax.set_xlabel("Longitude [°]")
    ax.set_xlim(minLon, maxLon)
    ax.set_ylabel("Latitude [°]")
    ax.set_ylim(minLat, maxLat)

    # Configure figure ...
    fg.tight_layout()

    # Save figure ...
    fg.savefig("findMiddleOfLocs/locations.png")
    matplotlib.pyplot.close(fg)

    # Optimize PNG ...
    pyguymer3.image.optimize_image(
        "findMiddleOfLocs/locations.png",
        debug = args.debug,
        strip = True,
    )
