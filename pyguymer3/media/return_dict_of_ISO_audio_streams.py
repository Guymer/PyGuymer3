#!/usr/bin/env python3

# Define function ...
def return_dict_of_ISO_audio_streams(
    fname,
    /,
    *,
     cacheDir = "~/.cache/pyguymer3",
        debug = __debug__,
    ensureNFC = True,
    lsdvdPath = None,
      timeout = 60.0,
        track = -1,
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

    # Check input ...
    if track == -1:
        raise Exception("no track was requested") from None

    # **************************************************************************

    # Loop over all tracks ...
    for trackInfo in __lsdvd__[fname]["track"]:
        # Skip if this track is not the chosen one ...
        if int(trackInfo["ix"]) != int(track):
            continue

        # Make a list of all of the audio channels in this track, even if there
        # is only one (if there is only one then the function "elem2dict" will
        # return it nakedly rather than make a list of audio channels) ...
        match trackInfo["audio"]:
            case dict():
                audioInfoList = [trackInfo["audio"]]
            case list():
                audioInfoList = trackInfo["audio"]
            case _:
                raise Exception("un-recognised type") from None

        # Initialize dictionary ...
        ans = {}

        # Loop over all audio channels in this track ...
        for audioInfo in audioInfoList:
            # Append information ...
            ans[audioInfo["streamid"]] = audioInfo
            ans[audioInfo["streamid"]]["channels"] = int(ans[audioInfo["streamid"]]["channels"])
            ans[audioInfo["streamid"]]["frequency"] = int(ans[audioInfo["streamid"]]["frequency"])  # [Hz]

        # Return dictionary ...
        return ans
