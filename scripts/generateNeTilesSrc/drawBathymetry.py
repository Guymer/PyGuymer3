#!/usr/bin/env python3

# Define function ...
def drawBathymetry(
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

    # Create suitable colour map ...
    cmap = matplotlib.colors.LinearSegmentedColormap.from_list(
        "bathymetry",
        [
            matplotlib.colors.to_rgba(matplotlib.colors.CSS4_COLORS["lightblue"]),
            matplotlib.colors.to_rgba(matplotlib.colors.CSS4_COLORS["darkblue"]),
        ]
    )

    # Loop over depths ...
    for depth, name in [
        (    0, "bathymetry_L_0"    ),
        (  200, "bathymetry_K_200"  ),
        ( 1000, "bathymetry_J_1000" ),
        ( 2000, "bathymetry_I_2000" ),
        ( 3000, "bathymetry_H_3000" ),
        ( 4000, "bathymetry_G_4000" ),
        ( 5000, "bathymetry_F_5000" ),
        ( 6000, "bathymetry_E_6000" ),
        ( 7000, "bathymetry_D_7000" ),
        ( 8000, "bathymetry_C_8000" ),
        ( 9000, "bathymetry_B_9000" ),
        (10000, "bathymetry_A_10000"),
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
            # Create a list of all of Polygons ...
            polys = []
            for record in cartopy.io.shapereader.Reader(sfile).records():
                if not hasattr(record, "geometry"):
                    continue
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
                coords = numpy.array(poly.exterior.coords)                      # [°]
                pixels = []                                                     # [px]
                for iCoord in range(coords.shape[0]):
                    x = float(img.width) * (( coords[iCoord, 0] + 180.0) / 360.0)   # [px]
                    y = float(img.height) * ((-coords[iCoord, 1] + 90.0) / 180.0)   # [px]
                    pixels.append((x, y))                                       # [px]
                del coords

                # Draw the Polygon ...
                r, g, b, _ = cmap(float(depth) / 10000.0, bytes = True)
                assert isinstance(r, numpy.uint8)
                assert isinstance(g, numpy.uint8)
                assert isinstance(b, numpy.uint8)
                draw.polygon(
                    pixels,
                     fill = (int(r), int(g), int(b)),
                    width = 0,
                )
                del pixels
            del polys
    del cmap
