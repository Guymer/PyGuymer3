def return_ISO_palette(fname, usr_track = None):
    # Import standard modules ...
    import shutil
    import subprocess

    # Import special modules ...
    try:
        import lxml.etree
    except:
        raise Exception("\"lxml\" is not installed; run \"pip install --user lxml\"")
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"")

    # Load sub-functions ...
    from .yuv2rgb import yuv2rgb

    # Check that "lsdvd" is installed ...
    if shutil.which("lsdvd") is None:
        raise Exception("\"lsdvd\" is not installed")

    # Find track info ...
    # NOTE "lsdvd" specifies the output encoding in the XML header. Therefore,
    #       do not assume that it is UTF-8 by using the "encoding" keyword
    #       argument of subprocess.check_output() and instead just pass "lxml"
    #       a byte sequence and let it handle it.
    rawstderrout = subprocess.check_output(
        [
            "lsdvd",
            "-x",
            "-Ox",
            fname
        ],
        stderr = subprocess.STDOUT
    )

    # Fix common errors ...
    rawstderrout = rawstderrout.replace(b"<df>Pan&Scan</df>", b"<df>Pan&amp;Scan</df>")

    # Loop over all tracks ...
    for track in lxml.etree.fromstring(rawstderrout).findall("track"):
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
