#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.12/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # Import standard modules ...
    import argparse
    import glob
    import os

    # Import my modules ...
    try:
        import pyguymer3
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
        "--OS-Terrain-maximum-elevation-interval",
        default = 250,
           dest = "osTerrainMaxElevInt",
           help = "the elevation interval for the OS Terrain tiles (in metres)",
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

    # Survey the OS Terrain tiles ...
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
        *osTerrain.keys(),
    )                                                                           # [#]
    maxN = max(
        *globe.keys(),
        *gshhg.keys(),
        *osTerrain.keys(),
    )                                                                           # [#]
    print(f"The smallest grid has {minN:,d} tiles.")
    print(f"The largest grid has {maxN:,d} tiles.")

    # **************************************************************************

    # Open output file ...
    with open(f"{args.absPathToRepo}/scripts/surveyTiles.csv", "wt", encoding = "utf-8") as fObj:
        # Write header ...
        fObj.write("maximum number of tiles in grid [#],GLOBE size [MiB],GSHHG size [MiB],OS Terrain size [MiB],total size [MiB]\n")

        # Loop over possible numbers ...
        for n in range(minN, maxN + 1):
            # Initialize counters ...
            globeSize = 0                                                       # [B]
            gshhgSize = 0                                                       # [B]
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

            # Increment counter for the OS Terrain tiles (skipping this possible
            # number if it does not encompass any of the tiles) ...
            for key, val in osTerrain.items():
                if key > n:
                    continue
                osTerrainSize += val                                            # [B]
            if osTerrainSize == 0:
                continue

            # Calculate total ...
            totSize = globeSize + gshhgSize + osTerrainSize                     # [B]

            # Write data ...
            fObj.write(f"{n:d},{float(globeSize) / (1024.0 * 1024.0):.1f},{float(gshhgSize) / (1024.0 * 1024.0):.1f},{float(osTerrainSize) / (1024.0 * 1024.0):.1f},{float(totSize) / (1024.0 * 1024.0):.1f}\n")
