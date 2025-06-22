#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.12/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # Import standard modules ...
    import json

    # Import special modules ...
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
        import matplotlib.pyplot
    except:
        raise Exception("\"matplotlib\" is not installed; run \"pip install --user matplotlib\"") from None

    # **************************************************************************

    # Initialize dictionary ...
    colourTables = {}

    # Loop over MatPlotLib colour map names ...
    # NOTE: See https://matplotlib.org/stable/users/explain/colors/colormaps.html
    for name in [
        "viridis",                      # Perceptually uniform sequential colour map.
        "plasma",                       # Perceptually uniform sequential colour map.
        "inferno",                      # Perceptually uniform sequential colour map.
        "magma",                        # Perceptually uniform sequential colour map.
        "cividis",                      # Perceptually uniform sequential colour map.
        "coolwarm",                     # A replacement for "bwr" which has smooth lightness.
        "turbo",                        # A replacement for "jet" which has smooth lightness.
    ]:
        # Initialize list ...
        colourTables[name] = []

        # Loop over levels ...
        for i in range(256):
            # Find colour values and convert them to the range [0, 255] ...
            r, g, b, _ = matplotlib.colormaps[name](float(i) / 255.0)
            r *= 255.0
            g *= 255.0
            b *= 255.0

            # Append values to list ...
            colourTables[name].append(
                [
                    round(r),
                    round(g),
                    round(b),
                ]
            )

    # **********************************************************************

    # Initialize list ...
    # NOTE: This colour map will go from (0,255,0) to (0,0,255).
    colourTables["g2b"] = []

    # Loop over levels ...
    for i in range(256):
        # Find colour values in the range [0, 255] ...
        r = 0.0
        g = float(255 - i)
        b = float(i)

        # Append values to list ...
        colourTables["g2b"].append(
            [
                round(r),
                round(g),
                round(b),
            ]
        )

    # **********************************************************************

    # Initialize list ...
    # NOTE: This colour map will go from (255,0,0) to (0,255,0).
    colourTables["r2g"] = []

    # Loop over levels ...
    for i in range(256):
        # Find colour values in the range [0, 255] ...
        r = float(255 - i)
        g = float(i)
        b = 0.0

        # Append values to list ...
        colourTables["r2g"].append(
            [
                round(r),
                round(g),
                round(b),
            ]
        )

    # **********************************************************************

    # Initialize list ...
    # NOTE: This colour map will go from (255,0,0) to (255,127,0) to (0,255,0).
    colourTables["r2o2g"] = []

    # Loop over levels ...
    for i in range(256):
        # Find colour values in the range [0, 255] ...
        if i <= 127:
            r = 255.0
        else:
            r = float(255 - 2 * (i - 127) + 1)
        g = float(i)
        b = 0.0

        # Append values to list ...
        colourTables["r2o2g"].append(
            [
                round(r),
                round(g),
                round(b),
            ]
        )

    # **********************************************************************

    # Initialize list ...
    # NOTE: This colour map will go from (0,0,0) to (255,255,255).
    colourTables["grey"] = []

    # Loop over levels ...
    for i in range(256):
        # Find colour values in the range [0, 255] ...
        r = float(i)
        g = float(i)
        b = float(i)

        # Append values to list ...
        colourTables["grey"].append(
            [
                round(r),
                round(g),
                round(b),
            ]
        )

    # **********************************************************************

    # Save colour maps ...
    with open("pyguymer3/data/json/colourTables.json", "wt", encoding = "utf-8") as fObj:
        json.dump(
            colourTables,
            fObj,
            ensure_ascii = False,
                  indent = 4,
               sort_keys = True,
        )
