#!/usr/bin/env python3

# Define function ...
def lsdvd(
    fname,
    /,
    *,
          cwd = None,
        debug = __debug__,
    ensureNFC = True,
    lsdvdPath = None,
      timeout = 60.0,
):
    """
    Run "lsdvd" on a file and return the format and stream information.

    Parameters
    ----------
    fname : str
        the file to be surveyed
    cwd : str, optional
        the child working directory
    debug : bool, optional
        print debug messages
    ensureNFC : bool, optional
        ensure that the Unicode encoding is NFC
    lsdvdPath : str, optional
        the path to the "lsdvd" binary (if not provided then Python will attempt
        to find the binary itself)
    timeout : float, optional
        the timeout for any requests/subprocess calls

    Returns
    -------
    ans : dict
        the format and stream information

    Notes
    -----
    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import standard modules ...
    import html
    import shutil
    import subprocess
    import unicodedata

    # Import special modules ...
    import lxml
    import lxml.etree

    # Import sub-functions ...
    from ..elem2dict import elem2dict

    # **************************************************************************

    # Try to find the paths if the user did not provide them ...
    if lsdvdPath is None:
        lsdvdPath = shutil.which("lsdvd")
    assert lsdvdPath is not None, "\"lsdvd\" is not installed"

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
             cwd = cwd,
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
    if ensureNFC and not unicodedata.is_normalized("NFC", stdout):
        ans = lxml.etree.XML(unicodedata.normalize("NFC", stdout))
    ans = lxml.etree.XML(stdout)

    # Return "lsdvd" output as dictionary ...
    return elem2dict(
        ans,
           debug = debug,
        simplify = True,
    )
