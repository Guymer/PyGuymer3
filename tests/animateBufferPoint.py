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
    except:
        raise Exception("\"geojson\" is not installed; run \"pip install --user geojson\"") from None
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
        import PIL
        import PIL.Image
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

    # Configure PIL to open images up to 1 GiP ...
    PIL.Image.MAX_IMAGE_PIXELS = 1024 * 1024 * 1024                             # [px]

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
    for lat in range(-90, +95, 5):
        # Loop over longitude ...
        for lon in range(-180, +185, 5):
            # Determine file names ...
            fname = f"animateBufferPoint/lon={lon:+04d},lat={lat:+03d}.png"
            jname = f"animateBufferPoint/lon={lon:+04d},lat={lat:+03d}.geojson"

            # Skip if both outputs already exist ...
            if os.path.exists(fname) and os.path.exists(jname):
                continue

            print(f" > Making \"{jname}\" and \"{fname}\" ...")

            # Create figure ...
            fg = matplotlib.pyplot.figure(figsize = (6, 6), dpi = 150)

            # Create first subplot ...
            ax1 = fg.add_subplot(2, 2, 1, projection = cartopy.crs.Robinson())
            ax1.set_global()
            pyguymer3.geo.add_map_background(ax1)
            pyguymer3.geo.add_horizontal_gridlines(ax1, [-180.0, +180.0, -90.0, +90.0], locs = range(-90, 135, 45))
            pyguymer3.geo.add_vertical_gridlines(ax1, [-180.0, +180.0, -90.0, +90.0], locs = range(-180, 225, 45))
            ax1.coastlines(resolution = "110m", color = "black", linewidth = 0.1)

            # Create second subplot ...
            ax2 = fg.add_subplot(2, 2, 2, projection = cartopy.crs.Orthographic(central_longitude = lon, central_latitude = lat))
            ax2.set_global()
            pyguymer3.geo.add_map_background(ax2)
            pyguymer3.geo.add_horizontal_gridlines(ax2, [-180.0, +180.0, -90.0, +90.0], locs = range(-90, 135, 45))
            pyguymer3.geo.add_vertical_gridlines(ax2, [-180.0, +180.0, -90.0, +90.0], locs = range(-180, 225, 45))
            ax2.coastlines(resolution = "110m", color = "black", linewidth = 0.1)

            # Create third subplot ...
            ax3 = fg.add_subplot(2, 2, (3, 4))
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
            buff0 = pyguymer3.geo.buffer(point, dist, debug = debug, fill = fill, fillSpace = fillSpace, nang = nang, simp = simp, tol = tol)
            ax1.add_geometries([buff0], cartopy.crs.PlateCarree(), edgecolor = (1.0, 0.0, 0.0, 1.0), facecolor = (1.0, 0.0, 0.0, 0.5), linewidth = 1.0)
            ax2.add_geometries([buff0], cartopy.crs.PlateCarree(), edgecolor = (1.0, 0.0, 0.0, 1.0), facecolor = (1.0, 0.0, 0.0, 0.5), linewidth = 1.0)
            for poly in pyguymer3.geo.extract_polys(buff0):
                coords = numpy.array(poly.exterior.coords)
                ax3.plot(coords[:, 0], coords[:, 1], color = (1.0, 0.0, 0.0, 1.0))
                del coords

            # Save GeoJSON ...
            with open(jname, "wt", encoding = "utf-8") as fobj:
                geojson.dump(
                    buff0,
                    fobj,
                    ensure_ascii = False,
                          indent = 4,
                       sort_keys = True,
                )

            # Clean up ...
            del buff0

            # Save figure ...
            fg.suptitle(f"({lon:.1f},{lat:.1f}) buffered by {0.001 * dist:,.1f}km")
            fg.savefig(fname, bbox_inches = "tight", dpi = 150, pad_inches = 0.1)
            pyguymer3.image.optimize_image(fname, strip = True)
            matplotlib.pyplot.close(fg)

    # **************************************************************************

    print(" > Making \"animateBufferPoint.mp4\" ...")

    # Initialize list ...
    frames = []

    # Loop over latitude ...
    for lat in range(-90, +95, 5):
        # Loop over longitude ...
        for lon in range(-180, +185, 5):
            # Determine file name ...
            frame = f"animateBufferPoint/lon={lon:+04d},lat={lat:+03d}.png"

            # Append it to the list ...
            frames.append(frame)

    # Save 25fps MP4 ...
    vname = pyguymer3.media.images2mp4(frames, debug = debug)
    shutil.move(vname, "animateBufferPoint.mp4")

    # Clean up ...
    del frames

    # **************************************************************************

    print(" > Making \"animateBufferPoint.gif\" and \"animateBufferPoint.webp\" ...")

    # Initialize list ...
    images = []

    # Loop over latitude ...
    for lat in range(-90, +95, 5):
        # Loop over longitude ...
        for lon in range(-180, +185, 5):
            # Determine file name ...
            fname = f"animateBufferPoint/lon={lon:+04d},lat={lat:+03d}.png"

            # Open image as RGB (even if it is paletted) ...
            with PIL.Image.open(fname) as iObj:
                image = iObj.convert("RGB")

            # Append it to the list ...
            images.append(image)

    # Save 25fps GIF ...
    pyguymer3.media.images2gif(
        images,
        "animateBufferPoint.gif",
        strip = True,
    )

    # Save 25fps WEBP ...
    pyguymer3.media.images2webp(
        images,
        "animateBufferPoint.webp",
        strip = True,
    )

    # Clean up ...
    del images

    # **************************************************************************

    # Set heights ...
    # NOTE: By inspection, the PNG frames are 875px tall.
    heights = [256, 512]                                                        # [px]

    # Loop over heights ...
    for height in heights:
        print(f" > Making \"animateBufferPoint{height:04d}px.mp4\" ...")

        # Initialize list ...
        frames = []

        # Loop over latitude ...
        for lat in range(-90, +95, 5):
            # Loop over longitude ...
            for lon in range(-180, +185, 5):
                # Determine file name ...
                frame = f"animateBufferPoint/lon={lon:+04d},lat={lat:+03d}.png"

                # Append it to the list ...
                frames.append(frame)

        # Save 25fps MP4 ...
        vname = pyguymer3.media.images2mp4(frames, debug = debug, screenWidth = height, screenHeight = height)
        shutil.move(vname, f"animateBufferPoint{height:04d}px.mp4")

        # Clean up ...
        del frames

        # **********************************************************************

        print(f" > Making \"animateBufferPoint{height:04d}px.gif\" and \"animateBufferPoint{height:04d}px.webp\" ...")

        # Initialize list ...
        images = []

        # Loop over latitude ...
        for lat in range(-90, +95, 5):
            # Loop over longitude ...
            for lon in range(-180, +185, 5):
                # Determine file name ...
                fname = f"animateBufferPoint/lon={lon:+04d},lat={lat:+03d}.png"

                # Open image as RGB (even if it is paletted) ...
                with PIL.Image.open(fname) as iObj:
                    image = iObj.convert("RGB")

                # Calculate width ...
                ratio = float(image.width) / float(image.height)                # [px/px]
                width = round(ratio * float(height))                            # [px]

                # Downscale the image and append it to the list ...
                images.append(image.resize((width, height), resample = PIL.Image.Resampling.LANCZOS))

        # Save 25fps GIF ...
        pyguymer3.media.images2gif(
            images,
            f"animateBufferPoint{height:04d}px.gif",
            strip = True,
        )

        # Save 25fps WEBP ...
        pyguymer3.media.images2webp(
            images,
            f"animateBufferPoint{height:04d}px.webp",
            strip = True,
        )

        # Clean up ...
        del images
