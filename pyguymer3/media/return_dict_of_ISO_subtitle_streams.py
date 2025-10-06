#!/usr/bin/env python3

# Define function ...
def return_dict_of_ISO_subtitle_streams(
    fname,
    /,
    *,
          cwd = None,
        debug = __debug__,
    ensureNFC = True,
    lsdvdPath = None,
      timeout = 60.0,
        track = -1,
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

    # Check input ...
    if track == -1:
        raise Exception("no track was requested") from None

    # **************************************************************************

    # Make sure that this fname is in the global dictionary ...
    if fname not in __lsdvd__:
        if debug:
            print(f"INFO: Running lsdvd(\"{fname}\") ...")
        __lsdvd__[fname] = lsdvd(
            fname,
                  cwd = cwd,
                debug = debug,
            ensureNFC = ensureNFC,
            lsdvdPath = lsdvdPath,
              timeout = timeout,
        )

    # Loop over all tracks ...
    for trackInfo in __lsdvd__[fname]["track"]:
        # Skip if this track is not the chosen one ...
        if int(trackInfo["ix"]) != int(track):
            continue

        # Make a list of all of the subtitle channels in this track, even if
        # there is only one (if there is only one then the function "elem2dict"
        # will return it nakedly rather than make a list of subtitle channels) ...
        match trackInfo["subp"]:
            case dict():
                subtitleInfoList = [trackInfo["subp"]]
            case list():
                subtitleInfoList = trackInfo["subp"]
            case _:
                raise Exception("un-recognised type") from None

        # Initialize dictionary ...
        ans = {}

        # Loop over all subtitle channels in this track ...
        for subtitleInfo in subtitleInfoList:
            # Append information ...
            ans[subtitleInfo["streamid"]] = subtitleInfo

        # Return dictionary ...
        return ans
