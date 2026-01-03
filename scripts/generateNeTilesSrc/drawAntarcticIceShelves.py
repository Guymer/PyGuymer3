#!/usr/bin/env python3

# Define function ...
def drawAntarcticIceShelves(
    img,
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
                "cache_dir" : pathlib.PosixPath("~/.local/share/cartopy").expanduser(),
            }
        )
    except:
        raise Exception("\"cartopy\" is not installed; run \"pip install --user Cartopy\"") from None
    try:
        import matplotlib
        matplotlib.rcParams.update(
            {
                            "backend" : "Agg",                                  # NOTE: See https://matplotlib.org/stable/gallery/user_interfaces/canvasagg.html
                         "figure.dpi" : 300,
                     "figure.figsize" : (9.6, 7.2),                             # NOTE: See https://github.com/Guymer/misc/blob/main/README.md#matplotlib-figure-sizes
                          "font.size" : 8,
                "image.interpolation" : "none",
                     "image.resample" : False,
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
        import pyguymer3.image
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
        # Create a list of all of the valid Polygons ...
        shapelyPolys = []
        for record in cartopy.io.shapereader.Reader(sfile).records():
            if not hasattr(record, "geometry"):
                continue
            shapelyPolys.extend(
                pyguymer3.geo.extract_polys(
                    record.geometry,
                    onlyValid = True,
                       repair = True,
                )
            )

        # Initialise lists ...
        pilHoles = []
        pilPolys = []

        # Loop over valid Polygons ...
        for shapelyPoly in shapelyPolys:
            # Check that it is a Polygon ...
            assert isinstance(shapelyPoly, shapely.geometry.polygon.Polygon), shapelyPoly

            # Convert the CoordinateSequence of the exterior ring (in degrees)
            # to a list of tuples (in pixels) and append it to the list ...
            coords = numpy.array(shapelyPoly.exterior.coords)                   # [°]
            pixels = []                                                         # [px]
            for iCoord in range(coords.shape[0]):
                x = float(img.width) * (( coords[iCoord, 0] + 180.0) / 360.0)   # [px]
                y = float(img.height) * ((-coords[iCoord, 1] + 90.0) / 180.0)   # [px]
                pixels.append((x, y))                                           # [px]
            del coords
            pilPolys.append(pixels)
            del pixels

            # Loop over interior rings ...
            for interior in shapelyPoly.interiors:
                # Convert the CoordinateSequence of the interior ring (in
                # degrees) to a list of tuples (in pixels) and append it to the
                # list ...
                coords = numpy.array(interior.coords)                           # [°]
                pixels = []                                                     # [px]
                for iCoord in range(coords.shape[0]):
                    x = float(img.width) * (( coords[iCoord, 0] + 180.0) / 360.0)   # [px]
                    y = float(img.height) * ((-coords[iCoord, 1] + 90.0) / 180.0)   # [px]
                    pixels.append((x, y))                                       # [px]
                del coords
                pilHoles.append(pixels)
                del pixels
        del shapelyPolys

        # Draw polygons with holes ...
        pyguymer3.image.drawPolygonsWithHoles(
            img,
            pilPolys,
            pilHoles,
            matplotlib.colors.CSS4_COLORS["aliceblue"],
        )
        del pilHoles, pilPolys
