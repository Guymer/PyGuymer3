#!/usr/bin/env python3

# Define function ...
def add_OSterrain_tiles(
    ax,
    fov,
    /,
    *,
         chunksize = 1048576,
             debug = __debug__,
      exiftoolPath = None,
      gifsiclePath = None,
              grid = "22x41",
     interpolation = "none",
      jpegtranPath = None,
           maxElev = 1000,
    mergedTileName = None,
       optipngPath = None,
              pool = None,
            prefix = ".",
      regrid_shape = 750,
          resample = False,
           timeout = 60.0,
               tol = 1.0e-10,
):
    """Add "OS Terrain 50" dataset tiles as a background to a Cartopy axis.

    Parameters
    ----------
    ax : cartopy.mpl.geoaxes.GeoAxes
        The axis to add the "OS Terrain 50" dataset tiles as a background to.
    fov : shapely.geometry.polygon.Polygon
        The field-of-view of the axis. This is used to determine which tiles are
        viewable on the axis and, therefore, which tiles need to be merged
        together.
    chunksize : int, optional
        The size of the chunks of any files which are read in (in bytes).
    debug : bool, optional
        Print debug messages.
    exiftoolPath : None or str, optional
        The path to the "exiftool" binary (if not provided then Python will
        attempt to find the binary itself).
    gifsiclePath : None or str, optional
        The path to the "gifsicle" binary (if not provided then Python will
        attempt to find the binary itself).
    grid : str, optional
        The grid to fetch tiles from.
    interpolation : str, optional
        The interpolation method used when drawing the final merged and warped
        image on the axis. Due to the use of ``**kwargs`` within Cartopy, this
        is passed all the way down the stack to the MatPlotLib ``.imshow()``
        call.
    jpegtranPath : None or str, optional
        The path to the "jpegtran" binary (if not provided then Python will
        attempt to find the binary itself).
    maxElev : int, optional
        The maximum elevation used by the colour map of the tiles (in metres).
    mergedTileName : None or str, optional
        If provided, then save the merged tile to this file.
    optipngPath : None or str, optional
        The path to the "optipng" binary (if not provided then Python will
        attempt to find the binary itself).
    pool : None or multiprocessing.pool.Pool, optional
        If provided, then run any "optipng" calls as a ``apply_async()`` job in
        this pool.
    prefix : str, optional
        Change the name of the output debugging CSVs.
    regrid_shape: int or tuple of int, optional
        The smallest dimension of the merged image of all of the tiles **after**
        it has been warped by Cartopy to be the same projection as the figure
        (in pixels). Due to the use of **kwargs within Cartopy, this is passed
        all the way down the stack to the Cartopy ".imshow()" call. The size of
        a figure, in inches, can be found by calling "fg.get_size_inches()". The
        resolution of a figure can be found by calling "fg.get_dpi()". The size
        of a figure, in pixels, can be found by multiplying the tuple by the
        scalar.
    resample : bool, optional
        Use a full resampling method when drawing the final merged and warped
        image on the axis. Due to the use of ``**kwargs`` within Cartopy, this
        is passed all the way down the stack to the MatPlotLib ``.imshow()``
        call.
    timeout : float, optional
        The timeout for any requests/subprocess calls (in seconds).
    tol : float, optional
        The Euclidean distance that defines two points as being the same (in
        degrees).

    Notes
    -----
    See the `MatPlotLib documentation about interpolation methods
    <https://matplotlib.org/stable/gallery/images_contours_and_fields/interpolation_methods.html>`_
    and the `MatPlotLib documentation about anti-aliasing
    <https://matplotlib.org/stable/gallery/images_contours_and_fields/image_antialiasing.html>`_.

    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

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
    from .en2ll import en2ll
    from ..image import optimise_image

    # **************************************************************************

    # Create short-hands ...
    # NOTE: See "pyguymer3/data/png/README.md".
    nx, ny = grid.split("x")
    nx = int(nx)                                                                # [#]
    ny = int(ny)                                                                # [#]
    origRes = 50                                                                # [m/px]
    origNumX = 13200                                                            # [px]
    origNumY = 24600                                                            # [px]
    origWidth = origRes * origNumX                                              # [m]
    origHeight = origRes * origNumY                                             # [m]
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
            left   =                     float(origWidth  *  ix     ) / float(nx)   # [m]
            right  =                     float(origWidth  * (ix + 1)) / float(nx)   # [m]
            bottom = float(origHeight) - float(origHeight * (iy + 1)) / float(ny)   # [m]
            top    = float(origHeight) - float(origHeight *  iy     ) / float(ny)   # [m]

            # Make a correctly oriented Polygon of the border of the tile and
            # set flag as to whether this tile intersects with the field-of-view
            # or not ...
            tile = en2ll(
                shapely.geometry.polygon.orient(
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
                ),
                 debug = debug,
                prefix = prefix,
                   tol = tol,
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
            tName = f"{os.path.dirname(__file__)}/../data/png/osTerrain/{nx:d}x{ny:d}/maxElev={maxElev:d}m/x={ix:d}/y={iy:d}.png"
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
    # NOTE: There is an off-by-one error in Cartopy somewhere ... I *think* that
    #       "cartopy.img_transform.mesh_projection()" shrinks the array by half
    #       a pixel at both ends.
    ax.imshow(
        mergedTile,
               extent = [
                                float(origWidth  *  frstUsedLon     ) / float(nx),
                                float(origWidth  * (lastUsedLon + 1)) / float(nx),
            float(origHeight) - float(origHeight * (lastUsedLat + 1)) / float(ny),
            float(origHeight) - float(origHeight *  frstUsedLat     ) / float(ny),
        ],                                                                      # [m], [m], [m], [m]
        interpolation = interpolation,
               origin = "upper",
         regrid_shape = regrid_shape,
             resample = resample,
            transform = cartopy.crs.OSGB(),
    )
