#!/usr/bin/env python3

# Define function ...
def add_GSHHG_tiles(
    ax,
    fov,
    /,
    *,
         chunksize = 1048576,
             debug = __debug__,
      exiftoolPath = None,
      gifsiclePath = None,
              grid = "2x1",
     interpolation = "none",
      jpegtranPath = None,
    mergedTileName = None,
       optipngPath = None,
              pool = None,
      regrid_shape = 750,
          resample = False,
        resolution = "i",
           timeout = 60.0,
):
    """Add GSHHG dataset tiles as a background to a Cartopy axis.

    Parameters
    ----------
    ax : cartopy.mpl.geoaxes.GeoAxes
        The axis to add the GSHHG dataset tiles as a background to.
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
    mergedTileName : None or str, optional
        If provided, then save the merged tile to this file.
    optipngPath : None or str, optional
        The path to the "optipng" binary (if not provided then Python will
        attempt to find the binary itself).
    pool : None or multiprocessing.pool.Pool, optional
        If provided, then run any "optipng" calls as a ``apply_async()`` job in
        this pool.
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
    resolution : str, optional
        The resolution of the GSHHG datasets.
    timeout : float, optional
        The timeout for any requests/subprocess calls (in seconds).

    Notes
    -----
    There is one argument relating to the `Global Self-Consistent Hierarchical
    High-Resolution Geography dataset <https://www.ngdc.noaa.gov/mgg/shorelines/>`_ :

    * *resolution*.

    There are five resolutions to choose from:

    * crude ("c");
    * low ("l");
    * intermediate ("i");
    * high ("h"); and
    * full ("f").

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
            tName = f"{os.path.dirname(__file__)}/../data/png/gshhg/{nx:d}x{ny:d}/res={resolution}/x={ix:d}/y={iy:d}.png"
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
            -180.0 + 360.0 * float(frstUsedLon) / float(nx),
            -180.0 + 360.0 * float(lastUsedLon + 1) / float(nx),
             +90.0 - 180.0 * float(lastUsedLat + 1) / float(ny),
             +90.0 - 180.0 * float(frstUsedLat) / float(ny),
        ],                                                                      # [°], [°], [°], [°]
        interpolation = interpolation,
               origin = "upper",
         regrid_shape = regrid_shape,
             resample = resample,
            transform = cartopy.crs.PlateCarree(),
    )
