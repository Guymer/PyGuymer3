#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.12/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # Import standard modules ...
    import argparse
    import json
    import os
    import pathlib
    import sys

    # Import special modules ...
    try:
        import cartopy
        cartopy.config.update(
            {
                "cache_dir" : pathlib.PosixPath("~/.local/share/cartopy").expanduser(),
            }
        )
    except:
        raise Exception("\"cartopy\" is not installed; run \"pip install --user Cartopy\"") from None
    try:
        import matplotlib
        matplotlib.rcParams.update(
            {
                       "axes.xmargin" : 0.01,
                       "axes.ymargin" : 0.01,
                            "backend" : "Agg",                                  # NOTE: See https://matplotlib.org/stable/gallery/user_interfaces/canvasagg.html
                         "figure.dpi" : 300,
                     "figure.figsize" : (9.6, 7.2),                             # NOTE: See https://github.com/Guymer/misc/blob/main/README.md#matplotlib-figure-sizes
                          "font.size" : 8,
                "image.interpolation" : "none",                                 # NOTE: See https://matplotlib.org/stable/gallery/images_contours_and_fields/interpolation_methods.html
                     "image.resample" : False,
            }
        )
        import matplotlib.pyplot
    except:
        raise Exception("\"matplotlib\" is not installed; run \"pip install --user matplotlib\"") from None
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None

    # Import my modules ...
    try:
        import pyguymer3
        import pyguymer3.geo
        import pyguymer3.image
    except:
        raise Exception("\"pyguymer3\" is not installed; run \"pip install --user PyGuymer3\"") from None

    # **************************************************************************

    # Create argument parser and parse the arguments ...
    parser = argparse.ArgumentParser(
           allow_abbrev = False,
            description = "Demonstrate the convergence of finding the middle of a collection of locations.",
        formatter_class = argparse.ArgumentDefaultsHelpFormatter,
    )
    parser.add_argument(
        "--absolute-path-to-repository",
        default = os.path.dirname(os.path.dirname(__file__)),
           dest = "absPathToRepo",
           help = "the absolute path to the PyGuymer3 repository",
           type = str,
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
        "--initial-geodesic-convergence",
        default = 1000.0e3,
           dest = "initialGeodesicConv",
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
        "--timeout",
        default = 60.0,
           help = "the timeout for any requests/subprocess calls (in seconds)",
           type = float,
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

    # Create short-hands ...
    pName = f'{os.path.basename(__file__).removesuffix(".py")}.png'
    midLon = 157.343904                                                         # [°]
    midLat = -30.949886                                                         # [°]
    maxDist = 1950.149873                                                       # [km]

    # **************************************************************************

    # Load data and convert to NumPy array ...
    with open(f"{args.absPathToRepo}/tests/exampleLons.json", "rt", encoding = "utf-8") as fObj:
        lons = json.load(fObj)                                                  # [°]
    lons = numpy.array(lons, dtype = numpy.float64)                             # [°]

    # Load data and convert to NumPy array ...
    with open(f"{args.absPathToRepo}/tests/exampleLats.json", "rt", encoding = "utf-8") as fObj:
        lats = json.load(fObj)                                                  # [°]
    lats = numpy.array(lats, dtype = numpy.float64)                             # [°]

    print(f"The *initial* Geodesic convergence criteria is {0.001 * args.initialGeodesicConv:,.1f} km.")

    # **************************************************************************

    print(f"Making \"{pName}\" ...")

    # Create figure ...
    fg = matplotlib.pyplot.figure(figsize = (3 * 9.6, 2 * 7.2))

    # Create axis ...
    axTL = fg.add_subplot(2, 3, 1)
    axTM = fg.add_subplot(2, 3, 2)
    axTR = fg.add_subplot(2, 3, 3)
    axBL = fg.add_subplot(2, 3, 4)
    axBM = fg.add_subplot(2, 3, 5)
    axBR = fg.add_subplot(2, 3, 6)

    # **************************************************************************

    print("Calculating (top-left) surface ...")

    # Set the extent of the Euclidean bounding box ...
    minLonL = midLon - 2.0                                                      # [°]
    maxLonL = midLon + 2.0                                                      # [°]
    minLatL = midLat - 2.0                                                      # [°]
    maxLatL = midLat + 2.0                                                      # [°]

    # Create some axes to survey the Euclidean bounding box ...
    lonsDivL = numpy.linspace(
        minLonL,
        maxLonL,
        dtype = numpy.float64,
          num = args.nDiv,
    )                                                                           # [°]
    latsDivL = numpy.linspace(
        minLatL,
        maxLatL,
        dtype = numpy.float64,
          num = args.nDiv,
    )                                                                           # [°]

    # Find the maximum Geodesic distance over the Euclidean bounding box and
    # convert to useful units ...
    maxDistL = numpy.zeros(
        (args.nDiv, args.nDiv),
        dtype = numpy.float64,
    )                                                                           # [m]
    for iLon in range(args.nDiv):
        for iLat in range(args.nDiv):
            maxDistL[iLat, iLon] = pyguymer3.geo.max_dist(
                lons,
                lats,
                lonsDivL[iLon],
                latsDivL[iLat],
                  eps = args.eps,
                nIter = args.nIter,
                space = "GeodesicSpace",
            )                                                                   # [m]
    maxDistL *= 0.001                                                           # [km]

    # Plot data ...
    imL = axTL.pcolormesh(
        lonsDivL,
        latsDivL,
        maxDistL,
           cmap = "gist_yarg",
        shading = "nearest",
         zorder = 1.0,
    )
    axTL.contour(
        lonsDivL,
        latsDivL,
        maxDistL,
        colors = "red",
        levels = numpy.linspace(
            maxDist +  1.0,
            maxDist + 10.0,
            num = 10,
        ),
        zorder = 1.5,
    )

    # **************************************************************************

    print("Calculating (top-middle) surface ...")

    # Set the extent of the Euclidean bounding box ...
    minLonM = midLon - 0.2                                                      # [°]
    maxLonM = midLon + 0.2                                                      # [°]
    minLatM = midLat - 0.2                                                      # [°]
    maxLatM = midLat + 0.2                                                      # [°]

    # Create some axes to survey the Euclidean bounding box ...
    lonsDivM = numpy.linspace(
        minLonM,
        maxLonM,
        dtype = numpy.float64,
          num = args.nDiv,
    )                                                                           # [°]
    latsDivM = numpy.linspace(
        minLatM,
        maxLatM,
        dtype = numpy.float64,
          num = args.nDiv,
    )                                                                           # [°]

    # Find the maximum Geodesic distance over the Euclidean bounding box and
    # convert to useful units ...
    maxDistM = numpy.zeros(
        (args.nDiv, args.nDiv),
        dtype = numpy.float64,
    )                                                                           # [m]
    for iLon in range(args.nDiv):
        for iLat in range(args.nDiv):
            maxDistM[iLat, iLon] = pyguymer3.geo.max_dist(
                lons,
                lats,
                lonsDivM[iLon],
                latsDivM[iLat],
                  eps = args.eps,
                nIter = args.nIter,
                space = "GeodesicSpace",
            )                                                                   # [m]
    maxDistM *= 0.001                                                           # [km]

    # Plot data ...
    imM = axTM.pcolormesh(
        lonsDivM,
        latsDivM,
        maxDistM,
           cmap = "gist_yarg",
        shading = "nearest",
         zorder = 1.0,
    )
    axTM.contour(
        lonsDivM,
        latsDivM,
        maxDistM,
        colors = "red",
        levels = numpy.linspace(
            maxDist +  1.0,
            maxDist + 10.0,
            num = 10,
        ),
        zorder = 1.5,
    )

    # **************************************************************************

    print("Calculating (top-right) surface ...")

    # Set the extent of the Euclidean bounding box ...
    minLonR = midLon - 0.02                                                     # [°]
    maxLonR = midLon + 0.02                                                     # [°]
    minLatR = midLat - 0.02                                                     # [°]
    maxLatR = midLat + 0.02                                                     # [°]

    # Create some axes to survey the Euclidean bounding box ...
    lonsDivR = numpy.linspace(
        minLonR,
        maxLonR,
        dtype = numpy.float64,
          num = args.nDiv,
    )                                                                           # [°]
    latsDivR = numpy.linspace(
        minLatR,
        maxLatR,
        dtype = numpy.float64,
          num = args.nDiv,
    )                                                                           # [°]

    # Find the maximum Geodesic distance over the Euclidean bounding box and
    # convert to useful units ...
    maxDistR = numpy.zeros(
        (args.nDiv, args.nDiv),
        dtype = numpy.float64,
    )                                                                           # [m]
    for iLon in range(args.nDiv):
        for iLat in range(args.nDiv):
            maxDistR[iLat, iLon] = pyguymer3.geo.max_dist(
                lons,
                lats,
                lonsDivR[iLon],
                latsDivR[iLat],
                  eps = args.eps,
                nIter = args.nIter,
                space = "GeodesicSpace",
            )                                                                   # [m]
    maxDistR *= 0.001                                                           # [km]

    # Plot data ...
    imR = axTR.pcolormesh(
        lonsDivR,
        latsDivR,
        maxDistR,
           cmap = "gist_yarg",
        shading = "nearest",
         zorder = 1.0,
    )
    axTR.contour(
        lonsDivR,
        latsDivR,
        maxDistR,
        colors = "red",
        levels = numpy.linspace(
            maxDist +  1.0,
            maxDist + 10.0,
            num = 10,
        ),
        zorder = 1.5,
    )

    # **************************************************************************

    print("Calculating (bottom-middle) surface ...")

    # Set the extent of the Euclidean bounding box ...
    minLonB = lons.min() - 1.0                                                     # [°]
    maxLonB = lons.max() + 1.0                                                     # [°]
    minLatB = lats.min() - 1.0                                                     # [°]
    maxLatB = lats.max() + 1.0                                                     # [°]

    # Create some axes to survey the Euclidean bounding box ...
    lonsDivB = numpy.linspace(
        minLonB,
        maxLonB,
        dtype = numpy.float64,
          num = args.nDiv,
    )                                                                           # [°]
    latsDivB = numpy.linspace(
        minLatB,
        maxLatB,
        dtype = numpy.float64,
          num = args.nDiv,
    )                                                                           # [°]

    # Find the maximum Geodesic distance over the Euclidean bounding box and
    # convert to useful units ...
    maxDistB = numpy.zeros(
        (args.nDiv, args.nDiv),
        dtype = numpy.float64,
    )                                                                           # [m]
    for iLon in range(args.nDiv):
        for iLat in range(args.nDiv):
            maxDistB[iLat, iLon] = pyguymer3.geo.max_dist(
                lons,
                lats,
                lonsDivB[iLon],
                latsDivB[iLat],
                  eps = args.eps,
                nIter = args.nIter,
                space = "GeodesicSpace",
            )                                                                   # [m]
    maxDistB *= 0.001                                                           # [km]

    # Plot data ...
    imB = axBM.pcolormesh(
        lonsDivB,
        latsDivB,
        maxDistB,
           cmap = "turbo",
        shading = "nearest",
         zorder = 1.0,
    )

    # **************************************************************************
    # **************************************************************************
    # **************************************************************************

    # Initialise minimum ...
    minMaxDist = 999.999e3                                                      # [km]

    # Loop over settings ...
    for angConv, attemptFortran, useSciPy in [
        (1.0  ,  True, False),
        (1.0  , False, False),
        (0.1  ,  True, False),
        (0.1  , False, False),
        (0.01 ,  True, False),
        (0.01 , False, False),
        (0.001,  True, False),
        (0.001, False, False),
    ]:
        print(f"Testing \"angConv={angConv:.3f}°, attemptFortran={repr(attemptFortran)[0]}, useSciPy={repr(useSciPy)[0]}\" ... ")

        # Initialise lists ...
        n = []                                                                  # [#]
        t = []                                                                  # [s]
        x = []                                                                  # [°]
        y = []                                                                  # [°]
        z = []                                                                  # [km]

        # Loop over refinements ...
        for nRefine in range(1, 15):
            # Create short-hand ...
            finalGeodesicConv = args.initialGeodesicConv / pow(2, nRefine - 1)  # [m]

            print(f"  Converging to {0.001 * finalGeodesicConv:9,.3f} km ... ", end = "")

            # Calculate the Geodesic bounding circle ...
            now = pyguymer3.now()
            midLon, midLat, maxDist = pyguymer3.geo.find_middle_of_locs(
                lons,
                lats,
                       angConv = angConv,
                attemptFortran = attemptFortran,
                          conv = args.initialGeodesicConv,
                         debug = args.debug,
                           eps = args.eps,
                        method = "GeodesicCircle",
                        midLat = None,
                        midLon = None,
                          nAng = args.nAng,
                         nIter = args.nIter,
                       nRefine = nRefine,
                           pad = -1.0,
                      useSciPy = useSciPy,
            )                                                                   # [°], [°], [m]
            dur = (pyguymer3.now() - now).total_seconds()                       # [s]

            print(f"({midLon:.6f}°, {midLat:.6f}°) and {0.001 * maxDist:9,.3f} km.")

            # Append values to lists ...
            n.append(nRefine)                                                   # [#]
            t.append(dur)                                                       # [s]
            x.append(midLon)                                                    # [°]
            y.append(midLat)                                                    # [°]
            z.append(0.001 * maxDist)                                           # [km]

        # Plot data ...
        axTL.plot(
            x,
            y,
             label = f"angConv={angConv:.3f}°, attemptFortran={repr(attemptFortran)[0]}, useSciPy={repr(useSciPy)[0]}",
            marker = "d",
            zorder = 2.0,
        )
        axTM.plot(
            x,
            y,
             label = f"angConv={angConv:.3f}°, attemptFortran={repr(attemptFortran)[0]}, useSciPy={repr(useSciPy)[0]}",
            marker = "d",
            zorder = 2.0,
        )
        axTR.plot(
            x,
            y,
             label = f"angConv={angConv:.3f}°, attemptFortran={repr(attemptFortran)[0]}, useSciPy={repr(useSciPy)[0]}",
            marker = "d",
            zorder = 2.0,
        )
        axBL.plot(
            n,
            z,
             label = f"angConv={angConv:.3f}°, attemptFortran={repr(attemptFortran)[0]}, useSciPy={repr(useSciPy)[0]}",
            marker = "d",
        )
        axBR.plot(
            n,
            t,
             label = f"angConv={angConv:.3f}°, attemptFortran={repr(attemptFortran)[0]}, useSciPy={repr(useSciPy)[0]}",
            marker = "d",
        )

        # Update minimum ...
        minMaxDist = min(minMaxDist, min(z))                                    # [km]

    # Plot data ...
    n = numpy.array(n)                                                          # [#]
    axBL.plot(
        n,
        minMaxDist - 0.001 * (args.initialGeodesicConv / numpy.pow(2, n - 1)),
            color = "red",
            label = "~convergence",
        linestyle = "dashed",
    )
    axBL.plot(
        n,
        minMaxDist + 0.001 * (args.initialGeodesicConv / numpy.pow(2, n - 1)),
            color = "red",
        linestyle = "dashed",
    )

    # **************************************************************************
    # **************************************************************************
    # **************************************************************************

    # Loop over records ...
    for record in cartopy.io.shapereader.Reader(
        cartopy.io.shapereader.gshhs(
            level = 1,
            scale = "c",
        )
    ).records():
        # Loop over Polygons ...
        for poly in pyguymer3.geo.extract_polys(record.geometry):
            # Create short-hand ...
            coords = numpy.array(poly.exterior.coords)                          # [°]

            # Plot the LinearRing of the exterior of the Polygon ...
            axBM.plot(
                coords[:, 0],
                coords[:, 1],
                 color = "white",
                zorder = 2.0,
            )

    # Plot data ...
    axBM.scatter(
        lons,
        lats,
        edgecolor = "black",
        facecolor = "gold",
           marker = "*",
                s = 64.0,
           zorder = 2.1,
    )

    # **************************************************************************
    # **************************************************************************
    # **************************************************************************

    # Add colour bars ...
    cbL = fg.colorbar(
        imL,
                 ax = axTL,
        orientation = "vertical",
    )
    cbM = fg.colorbar(
        imM,
                 ax = axTM,
        orientation = "vertical",
    )
    cbR = fg.colorbar(
        imR,
                 ax = axTR,
        orientation = "vertical",
    )
    cbB = fg.colorbar(
        imB,
                 ax = axBM,
        orientation = "vertical",
    )

    # Configure colour bars ...
    cbL.set_label("Maximum Geodesic Distance [km]")
    cbM.set_label("Maximum Geodesic Distance [km]")
    cbR.set_label("Maximum Geodesic Distance [km]")
    cbB.set_label("Maximum Geodesic Distance [km]")

    # Configure axis ...
    # NOTE: Setting the ticks clobbers the limits, so the ticks must be set
    #       before the limits.
    axTL.set_xticks(
        numpy.linspace(-180.0, +180.0, num = 901),
        labels = [f"{_:.1f}" for _ in numpy.linspace(-180.0, +180.0, num = 901)]
    )
    axTL.set_yticks(
        numpy.linspace( -90.0,  +90.0, num = 451),
        labels = [f"{_:.1f}" for _ in numpy.linspace( -90.0,  +90.0, num = 451)]
    )
    axTL.grid()
    axTL.legend(loc = "upper left")
    axTL.set_aspect("equal")
    axTL.set_xlabel("Longitude [°]")
    axTL.set_xlim(minLonL, maxLonL)
    axTL.set_ylabel("Latitude [°]")
    axTL.set_ylim(minLatL, maxLatL)

    # Configure axis ...
    # NOTE: Setting the ticks clobbers the limits, so the ticks must be set
    #       before the limits.
    axTM.set_xticks(
        numpy.linspace(-180.0, +180.0, num = 9001),
        labels = [f"{_:.2f}" for _ in numpy.linspace(-180.0, +180.0, num = 9001)]
    )
    axTM.set_yticks(
        numpy.linspace( -90.0,  +90.0, num = 4501),
        labels = [f"{_:.2f}" for _ in numpy.linspace( -90.0,  +90.0, num = 4501)]
    )
    axTM.grid()
    axTM.legend(loc = "upper left")
    axTM.set_aspect("equal")
    axTM.set_xlabel("Longitude [°]")
    axTM.set_xlim(minLonM, maxLonM)
    axTM.set_ylabel("Latitude [°]")
    axTM.set_ylim(minLatM, maxLatM)

    # Configure axis ...
    # NOTE: Setting the ticks clobbers the limits, so the ticks must be set
    #       before the limits.
    axTR.set_xticks(
        numpy.linspace(-180.0, +180.0, num = 90001),
        labels = [f"{_:.3f}" for _ in numpy.linspace(-180.0, +180.0, num = 90001)]
    )
    axTR.set_yticks(
        numpy.linspace( -90.0,  +90.0, num = 45001),
        labels = [f"{_:.3f}" for _ in numpy.linspace( -90.0,  +90.0, num = 45001)]
    )
    axTR.grid()
    axTR.legend(loc = "upper left")
    axTR.set_aspect("equal")
    axTR.set_xlabel("Longitude [°]")
    axTR.set_xlim(minLonR, maxLonR)
    axTR.set_ylabel("Latitude [°]")
    axTR.set_ylim(minLatR, maxLatR)

    # Configure axis ...
    axBL.grid()
    axBL.legend(loc = "upper right")
    axBL.set_xlabel("Number Of Refinements [#]")
    axBL.set_xlim(0)
    axBL.set_ylabel("Maximum Geodesic Distance [km]")
    axBL.set_ylim(minMaxDist - 0.25, minMaxDist + 2.5)

    # Configure axis ...
    # NOTE: Setting the ticks clobbers the limits, so the ticks must be set
    #       before the limits.
    axBM.set_xticks(range(-180, +182, 2))
    axBM.set_yticks(range( -90,  +92, 2))
    axBM.grid()
    axBM.set_aspect("equal")
    axBM.set_xlabel("Longitude [°]")
    axBM.set_xlim(minLonB, maxLonB)
    axBM.set_ylabel("Latitude [°]")
    axBM.set_ylim(minLatB, maxLatB)

    # Configure axis ...
    axBR.grid()
    axBR.legend(loc = "upper left")
    axBR.set_xlabel("Number Of Refinements [#]")
    axBR.set_xlim(0)
    axBR.set_ylabel("Duration [s]")
    axBR.set_ylim(0)

    # Configure figure ...
    fg.tight_layout()

    # Save figure ...
    fg.savefig(pName)
    matplotlib.pyplot.close(fg)

    # Optimise PNG ...
    pyguymer3.image.optimise_image(
        pName,
          debug = args.debug,
          strip = True,
        timeout = args.timeout,
    )
