#!/usr/bin/env python3

# Define function ...
def add_GLOBE_tiles(
    fg,
    ax,
    fov,
    /,
    *,
         chunksize = 1048576,
             debug = __debug__,
      exiftoolPath = None,
      gifsiclePath = None,
              grid = "18x9",
     interpolation = "none",
      jpegtranPath = None,
           maxElev = 1000,
    mergedTileName = None,
       optipngPath = None,
              pool = None,
          resample = False,
           timeout = 60.0,
):
    # Import standard modules ...
    import os
    import pathlib

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

    # Import sub-functions ...
    from ..image import optimise_image

    # **************************************************************************

    # Create short-hands ...
    # NOTE: See "pyguymer3/data/png/README.md".
    nx, ny = grid.split("x")
    nx = int(nx)                                                                # [#]
    ny = int(ny)                                                                # [#]
    tileSize = 300                                                              # [px]

    # Initialise array ...
    usedTiles = numpy.empty(
        (ny, nx),
        dtype = numpy.bool,
    )

    # Loop over tiles ...
    for iy in range(ny):
        for ix in range(nx):
            # Create short-hands ...
            left   = -180.0 + 360.0 * float(ix    ) / float(nx)                 # [°]
            right  = -180.0 + 360.0 * float(ix + 1) / float(nx)                 # [°]
            bottom =  +90.0 - 180.0 * float(iy + 1) / float(ny)                 # [°]
            top    =  +90.0 - 180.0 * float(iy    ) / float(ny)                 # [°]

            # Make a correctly oriented Polygon of the border of the tile and
            # set flag as to whether this tile intersects with the field-of-view
            # or not ...
            tile = shapely.geometry.polygon.orient(
                shapely.geometry.polygon.Polygon(
                    shapely.geometry.polygon.LinearRing(
                        [
                            (left , top   ),
                            (right, top   ),
                            (right, bottom),
                            (left , bottom),
                            (left , top   ),
                        ]
                    )
                )
            )
            usedTiles[iy, ix] = tile.intersects(fov)

    # Find the bounding box of the used tiles ...
    usedLats = numpy.any(usedTiles, axis = 1)
    usedLons = numpy.any(usedTiles, axis = 0)

    # Find the scope of the bounding box of the used tiles ...
    frstUsedLat = numpy.where(usedLats)[0][0]                                   # [#]
    lastUsedLat = numpy.where(usedLats)[0][-1]                                  # [#]
    frstUsedLon = numpy.where(usedLons)[0][0]                                   # [#]
    lastUsedLon = numpy.where(usedLons)[0][-1]                                  # [#]

    # Create merged tile ...
    mergedTile = PIL.Image.new(
        color = (127, 127, 127),
         mode = "RGB",
         size = (
            (lastUsedLon - frstUsedLon + 1) * tileSize,
            (lastUsedLat - frstUsedLat + 1) * tileSize,
        ),
    )
    for iy in range(ny):
        for ix in range(nx):
            if not usedTiles[iy, ix]:
                continue
            tName = f"{os.path.dirname(__file__)}/../data/png/globe/{nx:d}x{ny:d}/maxElev={maxElev:d}m/x={ix:d}/y={iy:d}.png"
            if not os.path.exists(tName):
                tName = f"{os.path.dirname(__file__)}/../data/png/missingTile.png"
            if debug:
                print(f"  Adding \"{tName}\" to merged tile ...")
            with PIL.Image.open(tName) as iObj:
                mergedTile.paste(
                    iObj.convert("RGB"),
                    (
                        (ix - frstUsedLon) * tileSize,
                        (iy - frstUsedLat) * tileSize,
                    ),
                )

    # Save merged tile if the user passed a file name ...
    if mergedTileName:
        # Save tile ...
        mergedTile.save(
            mergedTileName,
            optimize = True,
        )

        # Optimise PNG ...
        optimise_image(
            mergedTileName,
               chunksize = chunksize,
                   debug = debug,
            exiftoolPath = exiftoolPath,
            gifsiclePath = gifsiclePath,
            jpegtranPath = jpegtranPath,
             optipngPath = optipngPath,
                    pool = pool,
                   strip = True,
                 timeout = timeout,
        )

    # Draw merged tile ...
    # NOTE: I am explicitly setting the regrid shape based off the resolution
    #       and the size of the figure, as well as a safety factor of 2
    #       (remembering Nyquist).
    # NOTE: There is an off-by-one error in Cartopy somewhere ... I *think* that
    #       "cartopy.img_transform.mesh_projection()" shrinks the array by half
    #       a pixel at both ends.
    ax.imshow(
        mergedTile,
               extent = [
            -180.0 + 360.0 * float(frstUsedLon) / float(nx),
            -180.0 + 360.0 * float(lastUsedLon + 1) / float(nx),
             +90.0 - 180.0 * float(lastUsedLat + 1) / float(ny),
             +90.0 - 180.0 * float(frstUsedLat) / float(ny),
        ],                                                                      # [°], [°], [°], [°]
        interpolation = interpolation,
               origin = "upper",
         regrid_shape = (
            round(2.0 * fg.get_figwidth() * fg.get_dpi()),
            round(2.0 * fg.get_figheight() * fg.get_dpi()),
        ),                                                                      # [px], [px]
             resample = resample,
            transform = cartopy.crs.PlateCarree(),
    )
