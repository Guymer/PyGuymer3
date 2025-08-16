#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.12/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # Import standard modules ...
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
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Import my modules ...
    try:
        import pyguymer3
        import pyguymer3.geo
        import pyguymer3.image
    except:
        raise Exception("\"pyguymer3\" is not installed; run \"pip install --user PyGuymer3\"") from None

    # **************************************************************************

    # Define starting location ...
    lon = 0.0                                                                   # [°]
    lat = 0.0                                                                   # [°]

    # Configure functions ...
    debug = False
    fill = -1.0                                                                 # [°]
    fillSpace = "EuclideanSpace"
    simp = -1.0                                                                 # [°]

    # Create point ...
    point = shapely.geometry.point.Point(lon, lat)

    # **************************************************************************

    # for dist in [
    #     9984,                           # good
    #     9985,                           # good
    #     9986,                           # bad
    #     9987,                           # bad
    # ]:
    #     buff = pyguymer3.geo.buffer(
    #         point,
    #         float(1000 * dist),
    #             debug = True,
    #              fill = fill,
    #         fillSpace = fillSpace,
    #              nAng = 9,
    #              simp = simp,
    #     )
    #     print(dist, buff.area)
    # exit()

    # **************************************************************************

    # Create short-hand ...
    pName = f'{__file__.removesuffix(".py")}.png'

    # Create figure ...
    fg = matplotlib.pyplot.figure()

    # Create axis ...
    ax = fg.add_subplot()

    # Loop over number of angles ...
    for i in range(3, 16):
        # Create short-hands ...
        nAng = pow(2, i) + 1                                                    # [#]
        cName = f'{__file__.removesuffix(".py")}_nAng={nAng:d}.csv'

        print(f"Processing {nAng:,d} angles ...")

        # **********************************************************************

        # Check if the CSV is missing ...
        if not os.path.exists(cName):
            print(f"  Making \"{cName}\" ...")

            # Open CSV ...
            with open(cName, "wt", encoding = "utf-8") as fObj:
                # Write header ...
                fObj.write("buffer distance [km],buffer Euclidean area [°2]\n")

                # Loop over distances ...
                for dist in range(9986 - 10, 10001 + 11, 1):
                    # Create short-hand ...
                    huge = bool(float(1000 * dist) > 0.25 * pyguymer3.CIRCUMFERENCE_OF_EARTH)

                    print(f"    Processing {dist:,d} km ({huge}) ...")

                    # Buffer Point and append values to list ...
                    buff = pyguymer3.geo.buffer(
                        point,
                        float(1000 * dist),
                            debug = debug,
                             fill = fill,
                        fillSpace = fillSpace,
                             nAng = nAng,
                             simp = simp,
                    )

                    # Write data ...
                    fObj.write(f"{dist:d},{buff.area:.15e}\n")

        # **********************************************************************

        print(f"  Loading \"{cName}\" ...")

        # Load data ...
        dists, areas = numpy.loadtxt(
            cName,
            delimiter = ",",
             skiprows = 1,
               unpack = True,
        )                                                                       # [km], [°2]

        # Plot data ...
        ax.plot(
            dists,
            areas,
            label = f"{nAng:,d} angles",
        )

    # Configure axis ...
    ax.grid()
    ax.legend(loc = "upper left")
    ax.set_xlabel("Buffer Distance [km]")
    ax.set_ylabel("Buffer Area [°2]")

    # Configure figure ...
    fg.tight_layout()

    # Save figure ...
    fg.savefig(pName)
    matplotlib.pyplot.close(fg)

    # Optimise PNG ...
    pyguymer3.image.optimise_image(
        pName,
        debug = debug,
        strip = True,
    )
