def return_dict_of_ISO_audio_streams(fname, usr_track = -1):
    # Import standard modules ...
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

    # Loop over all tracks ...
    for track in lxml.etree.fromstring(rawstderrout).findall("track"):
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
