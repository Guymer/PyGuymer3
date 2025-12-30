#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.12/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # Import standard modules ...
    import argparse
    import glob
    import os

    # Import special modules ...
    try:
        import matplotlib
        matplotlib.rcParams.update(
            {
                       "axes.xmargin" : 0.01,
                       "axes.ymargin" : 0.01,
                            "backend" : "Agg",                                  # NOTE: See https://matplotlib.org/stable/gallery/user_interfaces/canvasagg.html
                         "figure.dpi" : 300,
                     "figure.figsize" : (9.6, 7.2),                             # NOTE: See https://github.com/Guymer/misc/blob/main/README.md#matplotlib-figure-sizes
                          "font.size" : 8,
                "image.interpolation" : "none",
                     "image.resample" : False,
            }
        )
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
        import pyguymer3.image
    except:
        raise Exception("\"pyguymer3\" is not installed; run \"pip install --user PyGuymer3\"") from None

    # **************************************************************************

    # Create argument parser and parse the arguments ...
    parser = argparse.ArgumentParser(
           allow_abbrev = False,
            description = "Calculate the size of the tiles.",
        formatter_class = argparse.ArgumentDefaultsHelpFormatter,
    )
    parser.add_argument(
        "--absolute-path-to-repository",
        default = os.path.dirname(os.path.dirname(__file__)),
           dest = "absPathToRepo",
           help = "the absolute path to the PyGuymer3 repository",
           type = str,
    )
    parser.add_argument(
        "--debug",
        action = "store_true",
          help = "print debug messages",
    )
    parser.add_argument(
        "--GLOBE-maximum-elevation-interval",
        default = 1000,
           dest = "globeMaxElevInt",
           help = "the elevation interval for the GLOBE tiles (in metres)",
           type = int,
    )
    parser.add_argument(
        "--GSHHG-resolutions",
        choices = [
            "c",                        # crude
            "l",                        # low
            "i",                        # intermediate
            "h",                        # high
            "f",                        # full
        ],
        default = [
            "f",                        # full
        ],
           dest = "gshhgRess",
           help = "the resolutions of the GSHHG datasets",
          nargs = "+",
           type = str,
    )
    parser.add_argument(
        "--NE-elevation-band-interval",
        default = 250,
           dest = "neElevBandInt",
           help = "the interval of the elevation bands to shade for the NE tiles (in metres)",
           type = int,
    )
    parser.add_argument(
        "--NE-maximum-elevation-interval",
        default = 1000,
           dest = "neMaxElevInt",
           help = "the elevation interval for the NE tiles (in metres)",
           type = int,
    )
    parser.add_argument(
        "--NE-resolutions",
        choices = [
             "10m",
             "50m",
            "110m",
        ],
        default = [
            "10m",
        ],
           dest = "neRess",
           help = "the resolutions of the NE datasets",
          nargs = "+",
           type = str,
    )
    parser.add_argument(
        "--OS-Terrain-maximum-elevation-interval",
        default = 250,
           dest = "osTerrainMaxElevInt",
           help = "the elevation interval for the \"OS Terrain 50\" tiles (in metres)",
           type = int,
    )
    parser.add_argument(
        "--timeout",
        default = 60.0,
           help = "the timeout for any requests/subprocess calls (in seconds)",
           type = float,
    )
    args = parser.parse_args()

    # **************************************************************************

    # Survey the GLOBE tiles ...
    globe = {}
    for dName in sorted(glob.glob(f"{args.absPathToRepo}/pyguymer3/data/png/globe/*x*")):
        w, h = os.path.basename(dName).split("x")
        n = int(w) * int(h)                                                     # [#]
        globe[n] = 0                                                            # [B]
        for maxElev in range(args.globeMaxElevInt, 9000, args.globeMaxElevInt):
            globe[n] += pyguymer3.return_folder_size(
                f"{dName}/maxElev={maxElev:d}m",
                debug = args.debug,
            )                                                                   # [B]

    # Survey the GSHHG tiles ...
    gshhg = {}
    for dName in sorted(glob.glob(f"{args.absPathToRepo}/pyguymer3/data/png/gshhg/*x*")):
        w, h = os.path.basename(dName).split("x")
        n = int(w) * int(h)                                                     # [#]
        gshhg[n] = 0                                                            # [B]
        for res in args.gshhgRess:
            gshhg[n] += pyguymer3.return_folder_size(
                f"{dName}/res={res}",
                debug = args.debug,
            )                                                                   # [B]

    # Survey the NE tiles ...
    ne = {}
    for dName in sorted(glob.glob(f"{args.absPathToRepo}/pyguymer3/data/png/ne/*x*")):
        w, h = os.path.basename(dName).split("x")
        n = int(w) * int(h)                                                     # [#]
        ne[n] = 0                                                               # [B]
        for maxElev in range(args.neMaxElevInt, 9000, args.neMaxElevInt):
            for res in args.neRess:
                ne[n] += pyguymer3.return_folder_size(
                    f"{dName}/maxElev={maxElev:d}m/elevInt={args.neElevBandInt:d}m/res={res}",
                    debug = args.debug,
                )                                                               # [B]

    # Survey the "OS Terrain 50" tiles ...
    osTerrain = {}
    for dName in sorted(glob.glob(f"{args.absPathToRepo}/pyguymer3/data/png/osTerrain/*x*")):
        w, h = os.path.basename(dName).split("x")
        n = int(w) * int(h)                                                     # [#]
        osTerrain[n] = 0                                                        # [B]
        for maxElev in range(args.osTerrainMaxElevInt, 9000, args.osTerrainMaxElevInt):
            osTerrain[n] += pyguymer3.return_folder_size(
                f"{dName}/maxElev={maxElev:d}m",
                debug = args.debug,
            )                                                                   # [B]

    # **************************************************************************

    # Survey the surveys of the tiles ...
    minN = min(
        *globe.keys(),
        *gshhg.keys(),
        *ne.keys(),
        *osTerrain.keys(),
    )                                                                           # [#]
    maxN = max(
        *globe.keys(),
        *gshhg.keys(),
        *ne.keys(),
        *osTerrain.keys(),
    )                                                                           # [#]
    print(f"The smallest grid has {minN:,d} tiles.")
    print(f"The largest grid has {maxN:,d} tiles.")

    # **************************************************************************

    # Open output file ...
    with open(f"{args.absPathToRepo}/scripts/surveyTiles.csv", "wt", encoding = "utf-8") as fObj:
        # Write header ...
        fObj.write("maximum number of tiles in grid [#],GLOBE size [MiB],GSHHG size [MiB],NE size [MiB],\"OS Terrain 50\" size [MiB],total size [MiB]\n")

        # Loop over possible numbers ...
        for n in range(minN, maxN + 1):
            # Initialize counters ...
            globeSize = 0                                                       # [B]
            gshhgSize = 0                                                       # [B]
            neSize = 0                                                          # [B]
            osTerrainSize = 0                                                   # [B]

            # Increment counter for the GLOBE tiles (skipping this possible
            # number if it does not encompass any of the tiles) ...
            for key, val in globe.items():
                if key > n:
                    continue
                globeSize += val                                                # [B]
            if globeSize == 0:
                continue

            # Increment counter for the GSHHG tiles (skipping this possible
            # number if it does not encompass any of the tiles) ...
            for key, val in gshhg.items():
                if key > n:
                    continue
                gshhgSize += val                                                # [B]
            if gshhgSize == 0:
                continue

            # Increment counter for the NE tiles (skipping this possible number
            # if it does not encompass any of the tiles) ...
            for key, val in ne.items():
                if key > n:
                    continue
                neSize += val                                                   # [B]
            if neSize == 0:
                continue

            # Increment counter for the "OS Terrain 50" tiles (skipping this
            # possible number if it does not encompass any of the tiles) ...
            for key, val in osTerrain.items():
                if key > n:
                    continue
                osTerrainSize += val                                            # [B]
            if osTerrainSize == 0:
                continue

            # Calculate total ...
            totSize = globeSize + gshhgSize + neSize + osTerrainSize            # [B]

            # Write data ...
            fObj.write(f"{n:d},{float(globeSize) / (1024.0 * 1024.0):.3f},{float(gshhgSize) / (1024.0 * 1024.0):.3f},{float(neSize) / (1024.0 * 1024.0):.3f},{float(osTerrainSize) / (1024.0 * 1024.0):.3f},{float(totSize) / (1024.0 * 1024.0):.3f}\n")

    # **************************************************************************

    # Create figure ...
    fg = matplotlib.pyplot.figure()

    # Create axis ...
    ax = fg.add_subplot()

    # Load data ...
    n, globeSize, gshhgSize, neSize, osTerrainSize, _ = numpy.loadtxt(
        f"{args.absPathToRepo}/scripts/surveyTiles.csv",
        delimiter = ",",
         skiprows = 1,
           unpack = True,
    )                                                                           # [#], [MiB], [MiB], [MiB], [MiB]

    # Plot data ...
    ax.fill_between(
        n,
        globeSize,
        label = "GLOBE",
    )
    ax.fill_between(
        n,
        globeSize,
        globeSize + gshhgSize,
        label = "GSHHG",
    )
    ax.fill_between(
        n,
        globeSize + gshhgSize,
        globeSize + gshhgSize + neSize,
        label = "NE",
    )
    ax.fill_between(
        n,
        globeSize + gshhgSize + neSize,
        globeSize + gshhgSize + neSize + osTerrainSize,
        label = "OS Terrain 50",
    )

    # Configure axis ...
    ax.grid()
    ax.legend(loc = "upper left")
    ax.set_xlabel("Maximum Number Of Tiles In Grid [#]")
    ax.set_ylabel("Size [MiB]")
    ax.set_ylim(0.0)

    # Configure figure ...
    fg.tight_layout()

    # Save figure ...
    fg.savefig(f"{args.absPathToRepo}/scripts/surveyTiles.png")
    matplotlib.pyplot.close(fg)

    # Optimize PNG ...
    pyguymer3.image.optimise_image(
        f"{args.absPathToRepo}/scripts/surveyTiles.png",
          debug = args.debug,
          strip = True,
        timeout = args.timeout,
    )
