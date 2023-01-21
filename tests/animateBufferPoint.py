#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.10/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # Import standard modules ...
    import os
    import shutil

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
        import PIL
        import PIL.Image
        PIL.Image.MAX_IMAGE_PIXELS = 1024 * 1024 * 1024                         # [px]
    except:
        raise Exception("\"PIL\" is not installed; run \"pip install --user Pillow\"") from None
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
        import pyguymer3.media
    except:
        raise Exception("\"pyguymer3\" is not installed; you need to have the Python module from https://github.com/Guymer/PyGuymer3 located somewhere in your $PYTHONPATH") from None

    print(f"Testing \"{pyguymer3.__path__[0]}\" ...")

    # Configure functions ...
    debug = False
    dist = 2.0e6                                                                # [m]
    fill = 1.0                                                                  # [°]
    fillSpace = "EuclideanSpace"
    nang = 361                                                                  # [#]
    simp = -1.0                                                                 # [°]
    tol = 1.0e-10                                                               # [°]

    # Make output directory ...
    if not os.path.exists("animateBufferPoint"):
        os.mkdir("animateBufferPoint")

    # **************************************************************************

    # Loop over latitude ...
    for lat in range(-90, +92, 2):
        # Loop over longitude ...
        for lon in range(-180, +182, 2):
            # Determine file names ...
            fname = f"animateBufferPoint/lon={lon:+04d},lat={lat:+03d}.png"
            jname = f"animateBufferPoint/lon={lon:+04d},lat={lat:+03d}.geojson"

            # Skip if both outputs already exist ...
            if os.path.exists(fname) and os.path.exists(jname):
                continue

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
            ax1.coastlines(
                     color = "black",
                 linewidth = 0.1,
                resolution = "110m",
            )
            ax1.set_global()
            pyguymer3.geo.add_map_background(ax1)
            pyguymer3.geo.add_horizontal_gridlines(
                ax1,
                [-180.0, +180.0, -90.0, +90.0],
                locs = range(-90, 135, 45),
            )
            pyguymer3.geo.add_vertical_gridlines(
                ax1,
                [-180.0, +180.0, -90.0, +90.0],
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
            ax2.coastlines(
                     color = "black",
                 linewidth = 0.1,
                resolution = "110m",
            )
            ax2.set_global()
            pyguymer3.geo.add_map_background(ax2)
            pyguymer3.geo.add_horizontal_gridlines(
                ax2,
                [-180.0, +180.0, -90.0, +90.0],
                locs = range(-90, 135, 45),
            )
            pyguymer3.geo.add_vertical_gridlines(
                ax2,
                [-180.0, +180.0, -90.0, +90.0],
                locs = range(-180, 225, 45),
            )

            # Create axis ...
            ax3 = fg.add_subplot(
                2,
                2,
                (3, 4),
            )

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

            # Buffer Point ...
            buff = pyguymer3.geo.buffer(
                point,
                dist,
                    debug = debug,
                     fill = fill,
                fillSpace = fillSpace,
                     nang = nang,
                     simp = simp,
                      tol = tol,
            )

            # Plot Point thrice ...
            ax1.add_geometries(
                pyguymer3.geo.extract_polys(buff),
                cartopy.crs.PlateCarree(),
                edgecolor = (1.0, 0.0, 0.0, 1.0),
                facecolor = (1.0, 0.0, 0.0, 0.5),
                linewidth = 1.0,
            )
            ax2.add_geometries(
                pyguymer3.geo.extract_polys(buff),
                cartopy.crs.PlateCarree(),
                edgecolor = (1.0, 0.0, 0.0, 1.0),
                facecolor = (1.0, 0.0, 0.0, 0.5),
                linewidth = 1.0,
            )
            for poly in pyguymer3.geo.extract_polys(buff):
                coords = numpy.array(poly.exterior.coords)                      # [°]
                ax3.plot(
                    coords[:, 0],
                    coords[:, 1],
                    color = (1.0, 0.0, 0.0, 1.0),
                )
                del coords

            # Save GeoJSON ...
            with open(jname, "wt", encoding = "utf-8") as fObj:
                geojson.dump(
                    buff,
                    fObj,
                    ensure_ascii = False,
                          indent = 4,
                       sort_keys = True,
                )

            # Clean up ...
            del buff

            # Configure figure ...
            fg.suptitle(f"({lon:+.1f}°,{lat:+.1f}°) buffered by {0.001 * dist:,.1f}km")
            fg.tight_layout()

            # Save figure ...
            fg.savefig(fname)
            matplotlib.pyplot.close(fg)

            # Optimize PNG ...
            pyguymer3.image.optimize_image(fname, strip = True)

    # **************************************************************************

    # Initialize list ...
    frames = []

    # Loop over latitude ...
    for lat in range(-90, +92, 2):
        # Loop over longitude ...
        for lon in range(-180, +182, 2):
            # Determine file name ...
            frame = f"animateBufferPoint/lon={lon:+04d},lat={lat:+03d}.png"

            # Append it to the list ...
            frames.append(frame)

    print(" > Making \"animateBufferPoint.mp4\" ...")

    # Save 60fps MP4 ...
    vname = pyguymer3.media.images2mp4(
        frames,
        debug = debug,
          fps = 60.0,
    )
    shutil.move(vname, "animateBufferPoint.mp4")

    # Set heights ...
    # NOTE: By inspection, the PNG frames are 1,800px tall.
    heights = [256, 512, 1024]                                                  # [px]

    # Loop over heights ...
    for height in heights:
        print(f" > Making \"animateBufferPoint{height:04d}px.mp4\" ...")

        # Save 60fps MP4 ...
        vname = pyguymer3.media.images2mp4(
            frames,
                   debug = debug,
                     fps = 60.0,
             screenWidth = height,
            screenHeight = height,
        )
        shutil.move(vname, f"animateBufferPoint{height:04d}px.mp4")

    # Clean up ...
    del frames
