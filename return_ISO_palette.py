def return_ISO_palette(fname, usr_track = None):
    # Import modules ...
    import numpy
    import shutil
    import subprocess
    import lxml.etree

    # Load sub-functions ...
    from .yuv2rgb import yuv2rgb

    # Check that "lsdvd" is installed ...
    if shutil.which("lsdvd") is None:
        raise Exception("\"lsdvd\" is not installed")

    # Find track info ...
    stderrout = subprocess.check_output(
        [
            "lsdvd",
            "-x",
            "-Ox",
            fname
        ],
        encoding = "utf-8",
        stderr = subprocess.STDOUT
    )

    # Loop over all tracks ...
    for track in lxml.etree.fromstring(stderrout).findall("track"):
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
            rgb = yuv2rgb(yuv)
            vals.append(format(rgb[0, 0, 0], "x").rjust(2, '0') + format(rgb[0, 0, 1], "x").rjust(2, '0') + format(rgb[0, 0, 2], "x").rjust(2, '0'))

        # Return answer ...
        return ",".join(vals)
