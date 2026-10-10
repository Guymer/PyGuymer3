#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.13/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # Import standard modules ...
    import argparse
    import os
    import zipfile

    # Import my modules ...
    try:
        import pyguymer3
    except:
        raise Exception("\"pyguymer3\" is not installed; run \"pip install --user PyGuymer3\"") from None

    # **************************************************************************

    # Create argument parser and parse the arguments ...
    parser = argparse.ArgumentParser(
           allow_abbrev = False,
            description = "Save raster datasets as tiles.",
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
        "--number-of-children",
        default = os.process_cpu_count() - 1,
           dest = "nChild",
           help = "the number of child \"multiprocessing\" processes to use when making the tiles",
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

    # Define the raster datasets ...
    rasters = {
        "cross-blend-hypso" : "https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/raster/HYP_HR_SR_OB_DR.zip",
               "gray-earth" : "https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/raster/GRAY_HR_SR_OB_DR.zip",
          "natural-earth-1" : "https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/raster/NE1_HR_LC_SR_W_DR.zip",
          "natural-earth-2" : "https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/raster/NE2_HR_LC_SR_W_DR.zip",
            "shaded-relief" : "https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/raster/SR_HR.zip",
    }

    # **************************************************************************

    # Start session ...
    with pyguymer3.start_session() as sess:
        # Loop over raster datasets ...
        for name, url in rasters.items():
            # Create short-hand ...
            zName = f"{args.absPathToRepo}/scripts/{name}.zip"

            # Check if the ZIP file does not exist yet ...
            if not os.path.exists(zName):
                print(f"Downloading \"{zName}\" ...")

                # Download the ZIP file ...
                if not pyguymer3.download_file(
                    sess,
                    url,
                    zName,
                      debug = args.debug,
                    timeout = args.timeout,
                     verify = True,
                ):
                    raise Exception(f"failed to download \"{url}\"") from None

    # **************************************************************************

    # Loop over raster datasets ...
    for name in rasters.keys():
        # Create short-hands ...
        tName = f"{args.absPathToRepo}/scripts/{name}.tif"
        zName = f"{args.absPathToRepo}/scripts/{name}.zip"

        # Check if the TIF file does not exist yet ...
        if not os.path.exists(tName):
            # Open ZIP file ...
            with zipfile.ZipFile(
                zName,
                mode = "r",
            ) as zObj:
                # Loop over contents ...
                for zInfo in zObj.infolist():
                    # Check if this item is a TIF image ...
                    if zInfo.filename.lower().endswith(".tif"):
                        print(f"Extracting \"{tName}\" ...")

                        # Open the input compressed TIF image ...
                        with zObj.open(
                            zInfo,
                            mode = "r",
                        ) as fObjIn:
                            # Open the output uncompressed TIF image ...
                            with open(
                                tName,
                                mode = "wb",
                            ) as fObjOut:
                                # Extract the TIF image ...
                                fObjOut.write(fObjIn.read())

                        # Stop looping over contents ...
                        break
