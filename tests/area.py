#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.12/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # Import standard modules ...
    import math
    import os

    # Import special modules ...
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
        import shapely.ops
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Import my modules ...
    try:
        import pyguymer3
        import pyguymer3.geo
        import pyguymer3.image
    except:
        raise Exception("\"pyguymer3\" is not installed; run \"pip install --user PyGuymer3\"") from None

    print(f"Testing \"{pyguymer3.__path__[0]}\" ...")

    # **************************************************************************

    # Create short-hands ...
    dist = 1000.0                                                               # [m]
    nAng = 41                                                                   # [#]
    area = math.pi * pow(dist, 2)                                               # [m²]

    # Make output directory ...
    if not os.path.exists("area"):
        os.mkdir("area")

    # **************************************************************************

    # Create short-hands ...
    lons = [float(lon) for lon in range(-179, 180)]                             # [°]
    lats = [float(lat) for lat in range(-89, 90)]                               # [°]

    # Check if the BIN file needs making ...
    if not os.path.exists("area/fencePosts.bin"):
        print("Making \"area/fencePosts.bin\" ...")

        # Initialize array to hold answer ...
        ratios = numpy.zeros(
            (len(lats), len(lons)),
            dtype = numpy.float64,
        )

        # Loop over longitudes ...
        for iLon, lon in enumerate(lons):
            # Loop over latitudes ...
            for iLat, lat in enumerate(lats):
                # Buffer Point ...
                buff = pyguymer3.geo.buffer(
                    shapely.geometry.point.Point(
                        lon,
                        lat,
                    ),
                    dist,
                    debug = False,
                     fill = -1.0,
                     nAng = nAng,
                     simp = -1.0,
                )

                # Populate array with the ratio of the estimated area to the
                # exact area ...
                ratios[iLat, iLon] = pyguymer3.geo.area(buff) / area

        # Save array as BIN ...
        ratios.tofile("area/fencePosts.bin")

    # Load BIN ...
    ratios = numpy.fromfile(
        "area/fencePosts.bin",
        dtype = numpy.float64,
    ).reshape(len(lats), len(lons))

    # Save array as PNG (scaled from -1% to +1% of the correct answer) ...
    pyguymer3.image.save_array_as_image(
        255.0 * (ratios - 0.99) / 0.02,
        "area/fencePosts.png",
           ct = "jet",
        scale = False,
    )

    # Print outliers ...
    for iLon, lon in enumerate(lons):
        for iLat, lat in enumerate(lats):
            if 0.99 <= ratios[iLat, iLon] <= 1.01:
                continue
            print(f"fencePosts  :: (lon = {lon:+6.1f}°, lat = {lat:+5.1f}°) has a ratio of {ratios[iLat, iLon]:.3f}.")

    # **************************************************************************

    # Create short-hands ...
    lons = [float(lon) + 0.5 for lon in range(-180, 180)]                       # [°]
    lats = [float(lat) + 0.5 for lat in range(-90, 90)]                         # [°]

    # Check if the BIN file needs making ...
    if not os.path.exists("area/fencePanels.bin"):
        print("Making \"area/fencePanels.bin\" ...")

        # Initialize array to hold answer ...
        ratios = numpy.zeros(
            (len(lats), len(lons)),
            dtype = numpy.float64,
        )

        # Loop over longitudes ...
        for iLon, lon in enumerate(lons):
            # Loop over latitudes ...
            for iLat, lat in enumerate(lats):
                # Buffer Point ...
                buff = pyguymer3.geo.buffer(
                    shapely.geometry.point.Point(
                        lon,
                        lat,
                    ),
                    dist,
                    debug = False,
                     fill = -1.0,
                     nAng = nAng,
                     simp = -1.0,
                )

                # Populate array with the ratio of the estimated area to the
                # exact area ...
                ratios[iLat, iLon] = pyguymer3.geo.area(buff) / area

        # Save array as BIN ...
        ratios.tofile("area/fencePanels.bin")

    # Load BIN ...
    ratios = numpy.fromfile(
        "area/fencePanels.bin",
        dtype = numpy.float64,
    ).reshape(len(lats), len(lons))

    # Save array as PNG (scaled from -1% to +1% of the correct answer) ...
    pyguymer3.image.save_array_as_image(
        255.0 * (ratios - 0.99) / 0.02,
        "area/fencePanels.png",
           ct = "jet",
        scale = False,
    )

    # Print outliers ...
    for iLon, lon in enumerate(lons):
        for iLat, lat in enumerate(lats):
            if 0.99 <= ratios[iLat, iLon] <= 1.01:
                continue
            print(f"fencePanels :: (lon = {lon:+6.1f}°, lat = {lat:+5.1f}°) has a ratio of {ratios[iLat, iLon]:.3f}.")

    # **************************************************************************

    # Buffer Point ...
    buff = pyguymer3.geo.buffer(
        shapely.geometry.point.Point(
            0.0,
            0.0,
        ),
        dist,
        debug = False,
         fill = -1.0,
         nAng = nAng,
         simp = -1.0,
    )

    # Create figure and axes ...
    fg = matplotlib.pyplot.figure(figsize = (2 * 7.2, 2 * 7.2))
    ax = fg.subplots(7, 7).flatten()

    # Loop over Polygons in the buffer of the Point ...
    for iBuffPoly, buffPoly in enumerate(pyguymer3.geo.extract_polys(buff)):
        # Make the Voronoi diagram ...
        voro = shapely.ops.voronoi_diagram(buffPoly)

        # Loop over Polygons in the Voronoi diagram ...
        for iVoroPoly, voroPoly in enumerate(pyguymer3.geo.extract_polys(voro)):
            print(f"iBuffPoly = {iBuffPoly:1d} :: iVoroPoly = {iVoroPoly:2d} :: voroPoly area = {voroPoly.area:8.6f}°²")

            # Plot the current Polygon in the buffer of the Point ...
            coords = numpy.array(buffPoly.exterior.coords)                      # [°]
            ax[iVoroPoly].plot(
                coords[:, 0],
                coords[:, 1],
                color = "C0",
            )

            # Plot the current Polygon in the Voronoi diagram ...
            coords = numpy.array(voroPoly.exterior.coords)                      # [°]
            ax[iVoroPoly].plot(
                coords[:, 0],
                coords[:, 1],
                color = "C1",
            )

            # Configure axis ...
            ax[iVoroPoly].grid()
            ax[iVoroPoly].set_aspect("equal")
            ax[iVoroPoly].set_title(f"iVoroPoly = {iVoroPoly:d}")
            ax[iVoroPoly].set_xlabel("Longitude [°]")
            ax[iVoroPoly].set_xlim(-0.03, +0.03)
            ax[iVoroPoly].set_xticks(
                [-0.03, -0.02, -0.01, 0.0, +0.01, +0.02, +0.03],
                labels = ["-0.03", "", "", "0.0", "", "", "+0.03"],
            )
            ax[iVoroPoly].set_ylabel("Latitude [°]")
            ax[iVoroPoly].set_ylim(-0.03, +0.03)
            ax[iVoroPoly].set_yticks(
                [-0.03, -0.02, -0.01, 0.0, +0.01, +0.02, +0.03],
                labels = ["-0.03", "", "", "0.0", "", "", "+0.03"],
            )

    # Configure figure ...
    fg.tight_layout()

    # Save figure ...
    fg.savefig("area/bad.png")
    matplotlib.pyplot.close(fg)

    # Optimize PNG ...
    pyguymer3.image.optimize_image(
        "area/bad.png",
        debug = False,
        strip = True,
    )
