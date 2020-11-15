def return_dict_of_ISO_subtitle_streams(fname, usr_track = -1):
    # Import standard modules ...
    import os
    import shutil
    import subprocess

    # Import special modules ...
    try:
        import lxml.etree
    except:
        raise Exception("\"lxml\" is not installed; run \"pip install --user lxml\"")

    # Check input ...
    if usr_track == -1:
        raise Exception("no track was requested")

    # Check that "lsdvd" is installed ...
    if shutil.which("lsdvd") is None:
        raise Exception("\"lsdvd\" is not installed")

    # Find track info ...
    # NOTE: "lsdvd" specifies the output encoding in the XML header. Therefore,
    #       do not assume that it is UTF-8 by using the "encoding" keyword
    #       argument of subprocess.check_output() and instead just pass "lxml"
    #       a byte sequence and let it handle it.
    # NOTE: Don't merge standard out and standard error together as the result
    #       will probably not be valid XML if standard error is not empty.
    rawstdout = subprocess.check_output(
        [
            "lsdvd",
            "-x",
            "-Ox",
            fname
        ],
        stderr = open(os.devnull, "wt")
    )

    # Fix common errors ...
    rawstdout = rawstdout.replace(b"<df>Pan&Scan</df>", b"<df>Pan&amp;Scan</df>")

    # Loop over all tracks ...
    for track in lxml.etree.fromstring(rawstdout).findall("track"):
        # Skip if this track is not the chosen one ...
        if int(track.find("ix").text) != int(usr_track):
            continue

        # Initialize dictionary ...
        ans = {}

        # Loop over all subtitle channels in this track ...
        for subp in track.findall("subp"):
            # Append information ...
            ans[subp.find("streamid").text] = {
                "content" : subp.find("content").text,
                "langcode" : subp.find("langcode").text,
                "language" : subp.find("language").text
            }

        # Return dictionary ...
        return ans
