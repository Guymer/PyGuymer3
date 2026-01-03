#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.12/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # Import standard modules ...
    import argparse
    import glob
    import os
    import shutil

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
        "--chunksize",
        default = 1048576,
           help = "the size of the chunks of any files which are read in (in bytes)",
           type = int,
    )
    parser.add_argument(
        "--debug",
        action = "store_true",
          help = "print debug messages",
    )
    parser.add_argument(
        "--exiftool-path",
        default = shutil.which("exiftool"),
           dest = "exiftoolPath",
           help = "the path to the \"exiftool\" binary",
           type = str,
    )
    parser.add_argument(
        "--gifsicle-path",
        default = shutil.which("gifsicle"),
           dest = "gifsiclePath",
           help = "the path to the \"gifsicle\" binary",
           type = str,
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
            "c",                        # crude
            "l",                        # low
            "i",                        # intermediate
            "h",                        # high
            "f",                        # full
        ],
           dest = "gshhgRess",
           help = "the resolutions of the GSHHG datasets",
          nargs = "+",
           type = str,
    )
    parser.add_argument(
        "--jpegtran-path",
        default = shutil.which("jpegtran"),
           dest = "jpegtranPath",
           help = "the path to the \"jpegtran\" binary",
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
             "50m",
            "110m",
        ],
           dest = "neRess",
           help = "the resolutions of the NE datasets",
          nargs = "+",
           type = str,
    )
    parser.add_argument(
        "--optipng-path",
        default = shutil.which("optipng"),
           dest = "optipngPath",
           help = "the path to the \"optipng\" binary",
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
        fObj.write("maximum number of tiles in grid [#],GLOBE size [B],GSHHG size [B],NE size [B],\"OS Terrain 50\" size [B],total size [B]\n")

        # Loop over possible numbers ...
        for n in range(2, 65536 + 1):
            # Initialize counters ...
            globeSize = 0                                                       # [B]
            gshhgSize = 0                                                       # [B]
            neSize = 0                                                          # [B]
            osTerrainSize = 0                                                   # [B]

            # Increment counter for the GLOBE tiles ...
            for key, val in globe.items():
                if key > n:
                    continue
                globeSize += val                                                # [B]

            # Increment counter for the GSHHG tiles ...
            for key, val in gshhg.items():
                if key > n:
                    continue
                gshhgSize += val                                                # [B]

            # Increment counter for the NE tiles ...
            for key, val in ne.items():
                if key > n:
                    continue
                neSize += val                                                   # [B]

            # Increment counter for the "OS Terrain 50" tiles ...
            for key, val in osTerrain.items():
                if key > n:
                    continue
                osTerrainSize += val                                            # [B]

            # Calculate total ...
            totSize = globeSize + gshhgSize + neSize + osTerrainSize            # [B]

            # Write data ...
            fObj.write(f"{n:d},{globeSize:d},{gshhgSize:d},{neSize:d},{osTerrainSize:d},{totSize:d}\n")

    # **************************************************************************

    # Create figure ...
    fg = matplotlib.pyplot.figure()

    # Create axis ...
    ax = fg.add_subplot()

    # Load data ...
    n, globeSize, gshhgSize, neSize, osTerrainSize, _ = numpy.loadtxt(
        f"{args.absPathToRepo}/scripts/surveyTiles.csv",
        delimiter = ",",
            dtype = numpy.uint32,
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

    # Shade region-of-interest (this is the number of tiles above which the
    # original image exceeds my custom PIL maximum image size) ...
    ax.axvspan(
        (1024.0 * 1024.0 * 1024.0) / (300.0 * 300.0),
        65536.0,
            alpha = 0.25,
            color = "red",
        linestyle = "none",
           zorder = 0.75,
    )

    # Configure axis ...
    ax.grid()
    ax.legend(loc = "upper left")
    ax.semilogx()
    ax.semilogy()
    ax.set_xlabel("Maximum Number Of Tiles In Grid")
    ax.set_xlim(2, 65536)
    ax.set_xticks(
        [
                2,
                4,
                8,
               16,
               32,
               64,
              128,
              256,
              512,
             1024,
             2048,
             4096,
             8192,
            16384,
            32768,
            65536,
        ],
        labels = [
                "2",
                "4",
                "8",
               "16",
               "32",
               "64",
              "128",
              "256",
              "512",
             "1 Ki",
             "2 Ki",
             "4 Ki",
             "8 Ki",
            "16 Ki",
            "32 Ki",
            "64 Ki",
        ],
    )
    ax.set_ylabel("Size")
    ax.set_ylim(10 * 1024, 10 * 1024 * 1024 * 1024)
    ax.set_yticks(
        [
                          10 * 1024,
                         100 * 1024,
                        1024 * 1024,
                   10 * 1024 * 1024,
                  100 * 1024 * 1024,
                 1024 * 1024 * 1024,
            10 * 1024 * 1024 * 1024,
        ],
        labels = [
             "10 KiB",
            "100 KiB",
              "1 MiB",
             "10 MiB",
            "100 MiB",
              "1 GiB",
             "10 GiB",
        ],
    )
    ax.minorticks_off()                 # NOTE: This must be called after both
                                        #       "set_xticks()" and
                                        #       "set_yticks()".

    # Configure figure ...
    fg.tight_layout()

    # Save figure ...
    fg.savefig(f"{args.absPathToRepo}/scripts/surveyTiles.png")
    matplotlib.pyplot.close(fg)

    # Optimize PNG ...
    pyguymer3.image.optimise_image(
        f"{args.absPathToRepo}/scripts/surveyTiles.png",
           chunksize = args.chunksize,
               debug = args.debug,
        exiftoolPath = args.exiftoolPath,
        gifsiclePath = args.gifsiclePath,
        jpegtranPath = args.jpegtranPath,
         optipngPath = args.optipngPath,
               strip = True,
             timeout = args.timeout,
    )
