#!/usr/bin/env python3

# This is a test suite for "find_point_on_great_circle()”.

# Import special modules ...
try:
    import cartopy
except:
    raise Exception("\"cartopy\" is not installed; run \"pip install --user Cartopy\"") from None
try:
    import matplotlib
    matplotlib.use("Agg")                                                       # NOTE: https://matplotlib.org/gallery/user_interfaces/canvasagg.html
    import matplotlib.pyplot
except:
    raise Exception("\"matplotlib\" is not installed; run \"pip install --user matplotlib\"") from None
try:
    import numpy
except:
    raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None

# Import my modules ...
try:
    import pyguymer3
except:
    raise Exception("\"pyguymer3\" is not installed; you need to have the Python module from https://github.com/Guymer/PyGuymer3 located somewhere in your $PYTHONPATH") from None

print("Testing \"{:s}\" ...".format(pyguymer3.__path__[0]))

# Define points ...
p1 = (-90.0, +15.0)
p2 = (+90.0, +15.0)

# Define number of sub-divisions ...
ndivs = [2, 3, 4, 8, 16, 1000]

# Create figure ...
fg = matplotlib.pyplot.figure(figsize = (6, 3), dpi = 150)

# Create plot ...
ax = matplotlib.pyplot.axes(projection = cartopy.crs.Robinson())
ax.set_global()
pyguymer3.add_map_background(ax)
ax.coastlines(resolution = "110m", color = "black", linewidth = 0.1)

# Loop over number of sub-divisions ...
for i, ndiv in enumerate(ndivs):
    # Create fractions and initialize arrays of lon/lat ...
    frac = numpy.linspace(0.0, 1.0, num = ndiv)
    lon = numpy.zeros(ndiv, dtype = numpy.float64)
    lat = numpy.zeros(ndiv, dtype = numpy.float64)

    # Loop over sub-divisions ...
    for j in range(ndiv):
        # Check what to do ...
        if j == 0:
            # Set start point ...
            lon[j] = p1[0]
            lat[j] = p1[1]
        elif j == ndiv - 1:
            # Set end point ...
            lon[j] = p2[0]
            lat[j] = p2[1]
        else:
            # Set intermediate point ...
            lon[j], lat[j] = pyguymer3.find_point_on_great_circle(frac[j], p1[0], p1[1], p2[0], p2[1])

        # Transform point ...
        # NOTE: See https://stackoverflow.com/a/52861074
        lon[j], lat[j] = cartopy.crs.Robinson().transform_point(lon[j], lat[j], cartopy.crs.Geodetic())

    # Plot great circle ...
    ax.plot(lon, lat, transform = cartopy.crs.Robinson(), linewidth = 1.0, color = matplotlib.pyplot.cm.rainbow(float(i) / float(len(ndivs) - 1)))

# Save figure ...
fg.savefig("greatCircle.png", bbox_inches = "tight", dpi = 150, pad_inches = 0.1)
pyguymer3.optimize_image("greatCircle.png", strip = True)
matplotlib.pyplot.close("all")