#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.12/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # Import standard modules ...
    import argparse
    import os
    import pathlib

    # Import special modules ...
    try:
        import cartopy
        cartopy.config.update(
            {
                "cache_dir" : pathlib.PosixPath("~/.local/share/cartopy_cache").expanduser(),
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
        import PIL.ImageDraw
    except:
        raise Exception("\"PIL\" is not installed; run \"pip install --user Pillow\"") from None
    try:
        import shapely
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

    # Create argument parser and parse the arguments ...
    parser = argparse.ArgumentParser(
           allow_abbrev = False,
            description = "Rasterize the GSHHG datasets.",
        formatter_class = argparse.ArgumentDefaultsHelpFormatter,
    )
    parser.add_argument(
        "--debug",
        action = "store_true",
          help = "print debug messages",
    )
    args = parser.parse_args()

    # **************************************************************************

    # Find the absolute path to the repository ...
    absPathToRepo = os.path.dirname(os.path.dirname(__file__))

    # Define colours and create palette ...
    colours = [
        "lightblue",                    # background, level 2, level 4
        "darkkhaki",                    # level 1, level 3
        "aliceblue",                    # level 5
        "snow"     ,                    # level 6
    ]
    pal = numpy.zeros(
        (len(colours), 3),
        dtype = numpy.uint8,
    )
    for i, colour in enumerate(colours):
        asHex = matplotlib.colors.CSS4_COLORS[colour][1:]
        pal[i, 0] = int(asHex[ :2], 16)
        pal[i, 1] = int(asHex[2:4], 16)
        pal[i, 2] = int(asHex[4: ], 16)

    # Create short-hand ...
    # NOTE: See "pyguymer3/data/png/README.md".
    tileSize = 600                                                              # [px]

    # Start ~infinite loop ...
    for zoomLevel in range(10):
        # Create short-hands and stop looping if this zoom level is too large ...
        nTilesY = pow(2, zoomLevel)                                             # [#]
        nTilesX = 2 * nTilesY                                                   # [#]
        nx = nTilesX * tileSize                                                 # [px]
        ny = nTilesY * tileSize                                                 # [px]
        if (nx * ny) > PIL.Image.MAX_IMAGE_PIXELS:
            break

        print(f"Processing zoom level {zoomLevel:,d} ...")

        # **********************************************************************

        # Loop over resolutions ...
        for res in [
            "c",                        # crude
            "l",                        # low
            "i",                        # intermediate
            "h",                        # high
            "f",                        # full
        ]:
            print(f"  Processing resolution \"{res}\" ...")

            # ******************************************************************

            # Create the PIL image and drawing object ...
            img = PIL.Image.new(
                color = 0,              # lightblue
                 mode = "L",
                 size = (nx, ny),
            )
            draw = PIL.ImageDraw.Draw(img)

            # Loop over levels and their colours ...
            for level, color in [
                (1, 1,),                # darkkhaki
                (2, 0,),                # lightblue
                (3, 1,),                # darkkhaki
                (4, 0,),                # lightblue
                (5, 2,),                # aliceblue
                (6, 3,),                # snow
            ]:
                print(f"    Adding level \"{level:d}\" ...")

                # Deduce Shapefile name (catching missing datasets) ...
                try:
                    sfile = cartopy.io.shapereader.gshhs(
                        level = level,
                        scale = res,
                    )
                except RuntimeError:
                    print(f"      Skipping \"{sfile}\" (RuntimeError).")
                    continue
                if os.path.basename(sfile) != f"GSHHS_{res}_L{level:d}.shp":
                    print(f"      Skipping \"{sfile}\" (filename does not match request).")
                    continue

                # Create a list of all of Polygons ......
                polys = []
                for record in cartopy.io.shapereader.Reader(sfile).records():
                    polys.extend(
                        pyguymer3.geo.extract_polys(
                            record.geometry,
                            onlyValid = True,
                               repair = True,
                        )
                    )

                # Loop over Polygons ...
                for poly in polys:
                    # Check that it is a Polygon ...
                    assert isinstance(poly, shapely.geometry.polygon.Polygon), poly

                    # Convert the CoordinateSequence of the exterior ring (in
                    # degrees) to a list of tuples (in pixels) ...
                    coords = numpy.array(poly.exterior.coords)                  # [°]
                    pixels = []                                                 # [px]
                    for iCoord in range(coords.shape[0]):
                        x = float(nx) * (( coords[iCoord, 0] + 180.0) / 360.0)  # [px]
                        y = float(ny) * ((-coords[iCoord, 1] +  90.0) / 180.0)  # [px]
                        pixels.append((x, y))                                   # [px]
                    del coords

                    # Draw the Polygon ...
                    draw.polygon(
                        pixels,
                         fill = color,
                        width = 0,
                    )
                    del pixels
                del polys
            del draw

            print("    Converting PIL image in to NumPy array ...")

            # Convert PIL image to NumPy array ...
            arr = numpy.array(img).reshape((ny, nx, 1))
            del img

            # **********************************************************************

            # Loop over x tiles ...
            for iTileX in range(nTilesX):
                # Loop over y tiles ...
                for iTileY in range(nTilesY):
                    # Create short-hands, make sure that the directory exists
                    # and skip this tile if it already exists ...
                    dName = f"{absPathToRepo}/pyguymer3/data/png/gshhg/{nTilesX:d}x{nTilesY:d}/res={res}/x={iTileX:d}"
                    pName = f"{dName}/y={iTileY:d}.png"
                    if not os.path.exists(dName):
                        os.makedirs(dName)
                    if os.path.exists(pName):
                        continue

                    print(f"    Making \"{pName}\" ...")

                    # Make PNG source and write it ...
                    src = pyguymer3.image.makePng(
                        arr[iTileY * tileSize:(iTileY + 1) * tileSize, iTileX * tileSize:(iTileX + 1) * tileSize, :],
                             debug = args.debug,
                            levels = [9,],
                         memLevels = [9,],
                          palUint8 = pal,
                        strategies = None,
                            wbitss = [15,],
                    )
                    with open(pName, "wb") as fObj:
                        fObj.write(src)
                    del src
            del arr
