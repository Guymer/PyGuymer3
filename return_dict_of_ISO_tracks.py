def return_dict_of_ISO_tracks(fname):
    # Import standard modules ...
    import shutil
    import subprocess

    # Import special modules ...
    try:
        import lxml.etree
    except:
        raise Exception("\"lxml\" is not installed; run \"pip install --user lxml\"")

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

    # Initialize dictionary ...
    ans = {}

    # Loop over all tracks ...
    for track in lxml.etree.fromstring(rawstderrout).findall("track"):
        # Append information ...
        ans[track.find("ix").text] = {
            "length" : float(track.find("length").text)                         # [s]
        }

    # Return dictionary ...
    return ans
