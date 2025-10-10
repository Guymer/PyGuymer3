#!/usr/bin/env python3

# Define function ...
def return_dict_of_ISO_tracks(
    fname,
    /,
    *,
     cacheDir = "~/.cache/pyguymer3",
        debug = __debug__,
    ensureNFC = True,
    lsdvdPath = None,
      timeout = 60.0,
):
    # Import global (subclassed) dictionary ...
    from .__lsdvd__ import __lsdvd__

    # **************************************************************************

    # Configure global (subclassed) dictionary ...
    # NOTE: If I blindly set "__lsdvd__.lsdvdPath" to "lsdvdPath" each time then
    #       I would clobber any previous calls to "shutil.which()" performed by
    #       the global (subclassed) dictionary itself.
    __lsdvd__.cacheDir = cacheDir
    __lsdvd__.debug = debug
    __lsdvd__.ensureNFC = ensureNFC
    if lsdvdPath is not None:
        __lsdvd__.lsdvdPath = lsdvdPath
    __lsdvd__.timeout = timeout                                                 # [s]

    # **************************************************************************

    # Initialize dictionary ...
    ans = {}

    # Loop over all tracks ...
    for trackInfo in __lsdvd__[fname]["track"]:
        # Append information ...
        ans[trackInfo["ix"]] = trackInfo
        ans[trackInfo["ix"]]["fps"] = float(ans[trackInfo["ix"]]["fps"])        # [Hz]
        ans[trackInfo["ix"]]["height"] = int(ans[trackInfo["ix"]]["height"])    # [px]
        ans[trackInfo["ix"]]["length"] = float(ans[trackInfo["ix"]]["length"])  # [s]
        ans[trackInfo["ix"]]["width"] = int(ans[trackInfo["ix"]]["width"])      # [px]

        # Remove extra information which will be returned by other functions ...
        if "audio" in ans[trackInfo["ix"]]:
            del ans[trackInfo["ix"]]["audio"]
        if "palette" in ans[trackInfo["ix"]]:
            del ans[trackInfo["ix"]]["palette"]
        if "subp" in ans[trackInfo["ix"]]:
            del ans[trackInfo["ix"]]["subp"]

    # Return dictionary ...
    return ans
