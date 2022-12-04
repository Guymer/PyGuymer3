def return_dict_of_ISO_audio_streams(fname, kwArgCheck = None, errors = "replace", usr_track = -1):
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
    ).decode("utf-8", errors = errors)
    tmp = stdout.index("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
    stdout = stdout[tmp + len("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"):]

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

        # Initialize dictionary ...
        ans = {}

        # Loop over all audio channels in this track ...
        for audio in track.findall("audio"):
            # Append information ...
            ans[audio.find("streamid").text] = {
                "content" : audio.find("content").text,
                "langcode" : audio.find("langcode").text,
                "language" : audio.find("language").text,
                "form" : audio.find("format").text.upper(),
                "frequency" : int(audio.find("frequency").text),                # [Hz]
                "channels" : int(audio.find("channels").text)
            }

        # Return dictionary ...
        return ans
