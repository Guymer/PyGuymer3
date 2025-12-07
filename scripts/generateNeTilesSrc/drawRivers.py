#!/usr/bin/env python3

# Define function ...
def drawRivers(
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
    except:
        raise Exception("\"pyguymer3\" is not installed; run \"pip install --user PyGuymer3\"") from None

    # **************************************************************************

    # Loop over names ...
    for name in [
        "rivers_australia",
        "rivers_europe",
        "rivers_lake_centerlines",
        "rivers_north_america",
    ]:
        # Find file containing the shapes ...
        try:
            sfile = cartopy.io.shapereader.natural_earth(
                resolution = res,
                  category = "physical",
                      name = name,
            )
        except urllib.error.HTTPError:
            continue

        # Check (again) that the file was found ...
        if os.path.exists(sfile):
            # Create a list of all of LineStrings ...
            lines = []
            for record in cartopy.io.shapereader.Reader(sfile).records():
                lines.extend(
                    pyguymer3.geo.extract_lines(
                        record.geometry,
                        onlyValid = True,
                    )
                )

            # Loop over LineStrings ...
            for line in lines:
                # Check that it is a LineString ...
                assert isinstance(line, shapely.geometry.linestring.LineString), line

                # Convert the CoordinateSequence (in degrees) to a list of
                # tuples (in pixels) ...
                coords = numpy.array(line.coords)                               # [°]
                pixels = []                                                     # [px]
                for iCoord in range(coords.shape[0]):
                    x = float(img.width) * (( coords[iCoord, 0] + 180.0) / 360.0)   # [px]
                    y = float(img.height) * ((-coords[iCoord, 1] + 90.0) / 180.0)   # [px]
                    pixels.append((x, y))                                       # [px]
                del coords

                # Draw the LineString ...
                draw.line(
                    pixels,
                     fill = matplotlib.colors.CSS4_COLORS["lightblue"],
                    width = 1,
                )
                del pixels
            del lines
