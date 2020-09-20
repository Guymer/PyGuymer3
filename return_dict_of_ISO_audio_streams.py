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
