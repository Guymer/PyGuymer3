#!/usr/bin/env python3

# Define function ...
def return_ISO_palette(fname, kwArgCheck = None, usr_track = -1):
    # Import standard modules ...
    import html
    import shutil
    import subprocess

    # Import special modules ...
    try:
        import lxml
        import lxml.etree
    except:
        raise Exception("\"lxml\" is not installed; run \"pip install --user lxml\"") from None
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None

    # Import sub-functions ...
    from .yuv2rgb import yuv2rgb

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check input ...
    if usr_track == -1:
        raise Exception("no track was requested") from None

    # Check that "lsdvd" is installed ...
    if shutil.which("lsdvd") is None:
        raise Exception("\"lsdvd\" is not installed") from None

    # Find track info ...
    # NOTE: "lsdvd" specifies the output encoding in the accompanying XML
    #       header, however, this is a lie. By inspection of "oxml.c" in the
    #       "lsdvd" source code it appears that the XML header is hard-coded and
    #       that "lsdvd" does not perform any checks to make sure that the
    #       output is either valid XML or valid UTF-8. Therefore, I must load it
    #       as a byte sequence and manually convert it to a UTF-8 string whilst
    #       replacing the invalid UTF-8 bytes (and remove the XML header).
    # NOTE: Don't merge standard out and standard error together as the result
    #       will probably not be valid XML if standard error is not empty.
    stdout = subprocess.check_output(
        [
            "lsdvd",
            "-x",
            "-Ox",
            fname
        ],
        stderr = subprocess.DEVNULL,
    ).decode("utf-8", errors = "replace")
    stdout = stdout.removeprefix("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")

    # Fix the file name itself ...
    stdout = stdout.replace(f"<device>{fname}</device>", f"<device>{html.escape(fname)}</device>")

    # Fix common errors ...
    stdout = stdout.replace("<df>Pan&Scan</df>", "<df>Pan&amp;Scan</df>")
    stdout = stdout.replace("<df>P&S + Letter</df>", "<df>P&amp;S + Letter</df>")

    # Loop over all tracks ...
    for track in lxml.etree.fromstring(stdout).findall("track"):
        # Skip if this track is not the chosen one ...
        if int(track.find("ix").text) != int(usr_track):
            continue

        # Create empty list ...
        vals = []

        # Loop over all colours in the palette ...
        for color in track.find("palette").findall("color"):
            # Convert YUV to RGB ...
            yuv = numpy.zeros((1, 1, 3), dtype = numpy.uint8)
            yuv[0, 0, 0] = int(color.text[0:2], 16)
            yuv[0, 0, 1] = int(color.text[2:4], 16)
            yuv[0, 0, 2] = int(color.text[4:6], 16)
            rgb = yuv2rgb(yuv, version = "SDTV")
            vals.append(format(rgb[0, 0, 0], "x").rjust(2, '0') + format(rgb[0, 0, 1], "x").rjust(2, '0') + format(rgb[0, 0, 2], "x").rjust(2, '0'))

        # Return answer ...
        return ",".join(vals)
