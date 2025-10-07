#!/usr/bin/env python3

# Define function ...
def return_dict_of_ISO_tracks(
    fname,
    /,
    *,
     cacheDir = "~/.cache/pyguymer3",
          cwd = None,
        debug = __debug__,
    ensureNFC = True,
    lsdvdPath = None,
      timeout = 60.0,
):
    # Import standard modules ...
    import shutil

    # Import sub-functions ...
    from .__lsdvd__ import __lsdvd__
    from .lsdvd import lsdvd

    # **************************************************************************

    # Try to find the paths if the user did not provide them ...
    if lsdvdPath is None:
        lsdvdPath = shutil.which("lsdvd")
    assert lsdvdPath is not None, "\"lsdvd\" is not installed"

    # **************************************************************************

    # Make sure that this fname is in the global dictionary ...
    if fname not in __lsdvd__:
        if debug:
            print(f"INFO: Running lsdvd(\"{fname}\") ...")
        __lsdvd__[fname] = lsdvd(
            fname,
             cacheDir = cacheDir,
                  cwd = cwd,
                debug = debug,
            ensureNFC = ensureNFC,
            lsdvdPath = lsdvdPath,
              timeout = timeout,
        )

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
