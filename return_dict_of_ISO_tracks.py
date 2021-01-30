def return_dict_of_ISO_tracks(fname, errors = "replace"):
    # Import standard modules ...
    import html
    import os
    import shutil
    import subprocess

    # Import special modules ...
    try:
        import lxml
        import lxml.etree
    except:
        raise Exception("\"lxml\" is not installed; run \"pip install --user lxml\"") from None

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
        stderr = open(os.devnull, "wt")
    ).decode("utf-8", errors = errors)
    tmp = stdout.index("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
    stdout = stdout[tmp + len("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"):]

    # Fix the file name itself ...
    stdout = stdout.replace("<device>{:s}</device>".format(fname), "<device>{:s}</device>".format(html.escape(fname)))

    # Fix common errors ...
    stdout = stdout.replace("<df>Pan&Scan</df>", "<df>Pan&amp;Scan</df>")
    stdout = stdout.replace("<df>P&S + Letter</df>", "<df>P&amp;S + Letter</df>")

    # Initialize dictionary ...
    ans = {}

    # Loop over all tracks ...
    for track in lxml.etree.fromstring(stdout).findall("track"):
        # Append information ...
        ans[track.find("ix").text] = {
            "length" : float(track.find("length").text)                         # [s]
        }

    # Return dictionary ...
    return ans
