#!/usr/bin/env python3

# Define function ...
def return_dict_of_ISO_subtitle_streams(
    fname,
    /,
    *,
    lsdvdPath = None,
      timeout = 60.0,
    usr_track = -1,
):
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

    # **************************************************************************

    # Try to find the paths if the user did not provide them ...
    if lsdvdPath is None:
        lsdvdPath = shutil.which("lsdvd")
    assert lsdvdPath is not None, "\"lsdvd\" is not installed"

    # Check input ...
    if usr_track == -1:
        raise Exception("no track was requested") from None

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
    resp = subprocess.run(
        [
            lsdvdPath,
            "-x",
            "-Ox",
            fname,
        ],
           check = True,
        encoding = "utf-8",
          errors = "replace",
          stderr = subprocess.DEVNULL,
          stdout = subprocess.PIPE,
         timeout = timeout,
    )
    stdout = resp.stdout.removeprefix("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")

    # Fix the file name itself ...
    stdout = stdout.replace(f"<device>{fname}</device>", f"<device>{html.escape(fname)}</device>")

    # Fix common errors ...
    stdout = stdout.replace("<df>Pan&Scan</df>", "<df>Pan&amp;Scan</df>")
    stdout = stdout.replace("<df>P&S + Letter</df>", "<df>P&amp;S + Letter</df>")

    # Parse the XML ...
    xml = lxml.etree.XML(stdout)

    # Loop over all tracks ...
    for track in xml.findall("track"):
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
                "language" : subp.find("language").text,
            }

        # Return dictionary ...
        return ans
