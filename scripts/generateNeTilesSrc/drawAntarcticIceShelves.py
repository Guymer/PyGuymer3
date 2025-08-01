#!/usr/bin/env python3

# Define function ...
def drawAntarcticIceShelves(
    img,
    draw,
    res,
    /,
):
    # Import standard modules ...
    import os
    import pathlib
    import urllib

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
        import shapely
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Import my modules ...
    try:
        import pyguymer3
        import pyguymer3.geo
    except:
        raise Exception("\"pyguymer3\" is not installed; run \"pip install --user PyGuymer3\"") from None

    # **************************************************************************

    # Find file containing the shapes ...
    try:
        sfile = cartopy.io.shapereader.natural_earth(
            resolution = res,
              category = "physical",
                  name = "antarctic_ice_shelves_polys",
        )
    except urllib.error.HTTPError:
        return

    # Check (again) that the file was found ...
    if os.path.exists(sfile):
        # Create a list of all of Polygons ...
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

            # Convert the CoordinateSequence of the exterior ring (in degrees)
            # to a list of tuples (in pixels) ...
            coords = numpy.array(poly.exterior.coords)                          # [°]
            pixels = []                                                         # [px]
            for iCoord in range(coords.shape[0]):
                x = float(img.width) * (( coords[iCoord, 0] + 180.0) / 360.0)   # [px]
                y = float(img.height) * ((-coords[iCoord, 1] + 90.0) / 180.0)   # [px]
                pixels.append((x, y))                                           # [px]
            del coords

            # Draw the Polygon ...
            draw.polygon(
                pixels,
                 fill = matplotlib.colors.CSS4_COLORS["aliceblue"],
                width = 0,
            )
            del pixels
        del polys
