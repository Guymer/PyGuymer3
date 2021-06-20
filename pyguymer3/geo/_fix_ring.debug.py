#!/usr/bin/env python3

# Import standard modules ...
import os

# Import special modules ...
try:
    import numpy
except:
    raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None
try:
    import matplotlib
    matplotlib.use("Agg")                                                       # NOTE: See https://matplotlib.org/stable/gallery/user_interfaces/canvasagg.html
    import matplotlib.pyplot
except:
    raise Exception("\"matplotlib\" is not installed; run \"pip install --user matplotlib\"") from None

# Create figure ...
fg = matplotlib.pyplot.figure(figsize = (12, 8), dpi = 300)
ax = fg.subplots(1, 3)

# Loop over CSVs ...
for i, csv in enumerate(["_fix_ring.debug.ring.csv", "_fix_ring.debug.part1.csv", "_fix_ring.debug.part2.csv"]):
    # Configure axis ...
    ax[i].grid()
    ax[i].set_title(csv.split(".")[-2])
    ax[i].set_xlabel("lon [°]")
    ax[i].set_ylabel("lat [°]")

    # Skip this CSV if it does not exist ...
    if not os.path.exists(csv):
        continue

    # Plot data ...
    x, y = numpy.loadtxt(csv, delimiter = ",", skiprows = 1, unpack = True)     # [°], [°]
    ax[i].plot(x, y, marker = "d")

# Save figure ...
fg.savefig("_fix_ring.debug.png", bbox_inches = "tight", dpi = 300, pad_inches = 0.1)
matplotlib.pyplot.close(fg)
