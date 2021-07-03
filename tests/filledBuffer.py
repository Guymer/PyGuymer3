#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.8/library/multiprocessing.html#multiprocessing-programming
if __name__ == "__main__":
    # This is a test suite for “buffer()”. Each polygon has a plot with both a
    # top-down projection and a Robinson projection so that you can check it.

    # Import special modules ...
    try:
        import cartopy
    except:
        raise Exception("\"cartopy\" is not installed; run \"pip install --user Cartopy\"") from None
    try:
        import matplotlib
        matplotlib.use("Agg")                                                   # NOTE: See https://matplotlib.org/stable/gallery/user_interfaces/canvasagg.html
        import matplotlib.pyplot
    except:
        raise Exception("\"matplotlib\" is not installed; run \"pip install --user matplotlib\"") from None
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
    except:
        raise Exception("\"pyguymer3\" is not installed; you need to have the Python module from https://github.com/Guymer/PyGuymer3 located somewhere in your $PYTHONPATH") from None

    print(f"Testing \"{pyguymer3.__path__[0]}\" ...")

    # Define distance and polygon ...
    dist = 1000.0e3                                                             # [m]
    poly = shapely.geometry.polygon.Polygon(
        [
            (  0.0, +25.0),
            (+25.0,   0.0),
            (  0.0, -25.0),
            (-25.0,   0.0),
            (  0.0, +25.0),
        ]
    )

    # **************************************************************************

    # Determine file name ...
    fname = "filledBuffer0.png"

    print(f" > Making \"{fname}\" ...")

    # Create figure ...
    fg = matplotlib.pyplot.figure(figsize = (6, 3), dpi = 150)

    # Create first subplot ...
    ax1 = matplotlib.pyplot.subplot(1, 2, 1, projection = cartopy.crs.Robinson())
    ax1.set_global()
    pyguymer3.geo.add_map_background(ax1)
    ax1.coastlines(resolution = "110m", color = "black", linewidth = 0.1)

    # Create second subplot ...
    ax2 = matplotlib.pyplot.subplot(1, 2, 2, projection = cartopy.crs.Orthographic(central_longitude = poly.centroid.x, central_latitude = poly.centroid.y))
    ax2.set_global()
    pyguymer3.geo.add_map_background(ax2)
    ax2.coastlines(resolution = "110m", color = "black", linewidth = 0.1)

    # Plot polygon twice ...
    ax1.add_geometries([poly], cartopy.crs.PlateCarree(), edgecolor = (1.0, 0.0, 0.0, 1.0), facecolor = (1, 0.0, 0.0, 0.5), linewidth = 1.0)
    ax2.add_geometries([poly], cartopy.crs.PlateCarree(), edgecolor = (1.0, 0.0, 0.0, 1.0), facecolor = (1, 0.0, 0.0, 0.5), linewidth = 1.0)

    # Save figure ...
    fg.suptitle("Simple Polygon")
    fg.savefig(fname, bbox_inches = "tight", dpi = 150, pad_inches = 0.1)
    pyguymer3.image.optimize_image(fname, strip = True)
    matplotlib.pyplot.close(fg)

    # **************************************************************************

    # Determine file name ...
    fname = "filledBuffer1.png"

    print(f" > Making \"{fname}\" ...")

    # Create figure ...
    fg = matplotlib.pyplot.figure(figsize = (6, 3), dpi = 150)

    # Create first subplot ...
    ax1 = matplotlib.pyplot.subplot(1, 2, 1, projection = cartopy.crs.Robinson())
    ax1.set_global()
    pyguymer3.geo.add_map_background(ax1)
    ax1.coastlines(resolution = "110m", color = "black", linewidth = 0.1)

    # Create second subplot ...
    ax2 = matplotlib.pyplot.subplot(1, 2, 2, projection = cartopy.crs.Orthographic(central_longitude = poly.centroid.x, central_latitude = poly.centroid.y))
    ax2.set_global()
    pyguymer3.geo.add_map_background(ax2)
    ax2.coastlines(resolution = "110m", color = "black", linewidth = 0.1)

    # Buffer polygon and plot it twice ...
    buff = pyguymer3.geo.buffer(poly, dist, debug = True, fill = -1.0, nang = 361, simp = -1.0)
    ax1.add_geometries([buff], cartopy.crs.PlateCarree(), edgecolor = (1.0, 0.0, 0.0, 1.0), facecolor = (1, 0.0, 0.0, 0.5), linewidth = 1.0)
    ax2.add_geometries([buff], cartopy.crs.PlateCarree(), edgecolor = (1.0, 0.0, 0.0, 1.0), facecolor = (1, 0.0, 0.0, 0.5), linewidth = 1.0)

    # Save figure ...
    fg.suptitle(f"Simple Polygon buffered by {0.001 * dist:,.1f}km\n(no filling; no simplification)")
    fg.savefig(fname, bbox_inches = "tight", dpi = 150, pad_inches = 0.1)
    pyguymer3.image.optimize_image(fname, strip = True)
    matplotlib.pyplot.close(fg)

    # **************************************************************************

    # Determine file name ...
    fname = "filledBuffer2.png"

    print(f" > Making \"{fname}\" ...")

    # Create figure ...
    fg = matplotlib.pyplot.figure(figsize = (6, 3), dpi = 150)

    # Create first subplot ...
    ax1 = matplotlib.pyplot.subplot(1, 2, 1, projection = cartopy.crs.Robinson())
    ax1.set_global()
    pyguymer3.geo.add_map_background(ax1)
    ax1.coastlines(resolution = "110m", color = "black", linewidth = 0.1)

    # Create second subplot ...
    ax2 = matplotlib.pyplot.subplot(1, 2, 2, projection = cartopy.crs.Orthographic(central_longitude = poly.centroid.x, central_latitude = poly.centroid.y))
    ax2.set_global()
    pyguymer3.geo.add_map_background(ax2)
    ax2.coastlines(resolution = "110m", color = "black", linewidth = 0.1)

    # Buffer polygon and plot it twice ...
    buff = pyguymer3.geo.buffer(poly, dist, debug = True, fill = -1.0, nang = 361, simp = 0.1)
    ax1.add_geometries([buff], cartopy.crs.PlateCarree(), edgecolor = (1.0, 0.0, 0.0, 1.0), facecolor = (1, 0.0, 0.0, 0.5), linewidth = 1.0)
    ax2.add_geometries([buff], cartopy.crs.PlateCarree(), edgecolor = (1.0, 0.0, 0.0, 1.0), facecolor = (1, 0.0, 0.0, 0.5), linewidth = 1.0)

    # Save figure ...
    fg.suptitle(f"Simple Polygon buffered by {0.001 * dist:,.1f}km\n(no filling; simp=0.1°)")
    fg.savefig(fname, bbox_inches = "tight", dpi = 150, pad_inches = 0.1)
    pyguymer3.image.optimize_image(fname, strip = True)
    matplotlib.pyplot.close(fg)

    # **************************************************************************

    # Determine file name ...
    fname = "filledBuffer3.png"

    print(f" > Making \"{fname}\" ...")

    # Create figure ...
    fg = matplotlib.pyplot.figure(figsize = (6, 3), dpi = 150)

    # Create first subplot ...
    ax1 = matplotlib.pyplot.subplot(1, 2, 1, projection = cartopy.crs.Robinson())
    ax1.set_global()
    pyguymer3.geo.add_map_background(ax1)
    ax1.coastlines(resolution = "110m", color = "black", linewidth = 0.1)

    # Create second subplot ...
    ax2 = matplotlib.pyplot.subplot(1, 2, 2, projection = cartopy.crs.Orthographic(central_longitude = poly.centroid.x, central_latitude = poly.centroid.y))
    ax2.set_global()
    pyguymer3.geo.add_map_background(ax2)
    ax2.coastlines(resolution = "110m", color = "black", linewidth = 0.1)

    # Buffer polygon and plot it twice ...
    buff = pyguymer3.geo.buffer(poly, dist, debug = True, fill = 1.0, nang = 361, simp = 0.1)
    ax1.add_geometries([buff], cartopy.crs.PlateCarree(), edgecolor = (1.0, 0.0, 0.0, 1.0), facecolor = (1, 0.0, 0.0, 0.5), linewidth = 1.0)
    ax2.add_geometries([buff], cartopy.crs.PlateCarree(), edgecolor = (1.0, 0.0, 0.0, 1.0), facecolor = (1, 0.0, 0.0, 0.5), linewidth = 1.0)

    # Save figure ...
    fg.suptitle(f"Simple Polygon buffered by {0.001 * dist:,.1f}km\n(fill = 1.0; simp = 0.1°)")
    fg.savefig(fname, bbox_inches = "tight", dpi = 150, pad_inches = 0.1)
    pyguymer3.image.optimize_image(fname, strip = True)
    matplotlib.pyplot.close(fg)

    # **************************************************************************

    # Determine file name ...
    fname = "filledBuffer4.png"

    print(f" > Making \"{fname}\" ...")

    # Create figure ...
    fg = matplotlib.pyplot.figure(figsize = (6, 3), dpi = 150)

    # Create first subplot ...
    ax1 = matplotlib.pyplot.subplot(1, 2, 1, projection = cartopy.crs.Robinson())
    ax1.set_global()
    pyguymer3.geo.add_map_background(ax1)
    ax1.coastlines(resolution = "110m", color = "black", linewidth = 0.1)

    # Create second subplot ...
    ax2 = matplotlib.pyplot.subplot(1, 2, 2, projection = cartopy.crs.Orthographic(central_longitude = poly.centroid.x, central_latitude = poly.centroid.y))
    ax2.set_global()
    pyguymer3.geo.add_map_background(ax2)
    ax2.coastlines(resolution = "110m", color = "black", linewidth = 0.1)

    # Buffer polygon and plot it twice ...
    buff = pyguymer3.geo.buffer(poly, dist, debug = True, fill = 1.0, nang = 361, simp = 1.0)
    ax1.add_geometries([buff], cartopy.crs.PlateCarree(), edgecolor = (1.0, 0.0, 0.0, 1.0), facecolor = (1, 0.0, 0.0, 0.5), linewidth = 1.0)
    ax2.add_geometries([buff], cartopy.crs.PlateCarree(), edgecolor = (1.0, 0.0, 0.0, 1.0), facecolor = (1, 0.0, 0.0, 0.5), linewidth = 1.0)

    # Save figure ...
    fg.suptitle(f"Simple Polygon buffered by {0.001 * dist:,.1f}km\n(fill = 1.0; simp = 1.0°)")
    fg.savefig(fname, bbox_inches = "tight", dpi = 150, pad_inches = 0.1)
    pyguymer3.image.optimize_image(fname, strip = True)
    matplotlib.pyplot.close(fg)

    # **************************************************************************

    # Determine file name ...
    fname = "filledBuffer5.png"

    print(f" > Making \"{fname}\" ...")

    # Create figure ...
    fg = matplotlib.pyplot.figure(figsize = (6, 3), dpi = 150)

    # Create first subplot ...
    ax1 = matplotlib.pyplot.subplot(1, 2, 1, projection = cartopy.crs.Robinson())
    ax1.set_global()
    pyguymer3.geo.add_map_background(ax1)
    ax1.coastlines(resolution = "110m", color = "black", linewidth = 0.1)

    # Create second subplot ...
    ax2 = matplotlib.pyplot.subplot(1, 2, 2, projection = cartopy.crs.Orthographic(central_longitude = poly.centroid.x, central_latitude = poly.centroid.y))
    ax2.set_global()
    pyguymer3.geo.add_map_background(ax2)
    ax2.coastlines(resolution = "110m", color = "black", linewidth = 0.1)

    # Buffer polygon and plot it twice ...
    buff = pyguymer3.geo.buffer(poly, dist, debug = True, fill = 1.0, nang = 361, simp = 10.0)
    ax1.add_geometries([buff], cartopy.crs.PlateCarree(), edgecolor = (1.0, 0.0, 0.0, 1.0), facecolor = (1, 0.0, 0.0, 0.5), linewidth = 1.0)
    ax2.add_geometries([buff], cartopy.crs.PlateCarree(), edgecolor = (1.0, 0.0, 0.0, 1.0), facecolor = (1, 0.0, 0.0, 0.5), linewidth = 1.0)

    # Save figure ...
    fg.suptitle(f"Simple Polygon buffered by {0.001 * dist:,.1f}km\n(fill = 1.0; simp = 10.0°)")
    fg.savefig(fname, bbox_inches = "tight", dpi = 150, pad_inches = 0.1)
    pyguymer3.image.optimize_image(fname, strip = True)
    matplotlib.pyplot.close(fg)
