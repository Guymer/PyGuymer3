#!/usr/bin/env python3

# Define function ...
def download_text(
    sess,
    url,
    /,
    *,
      cookies = None,
        debug = __debug__,
    ensureNFC = True,
      headers = None,
      timeout = 10.0,
       verify = True,
):
    """GET a URL and return the text

    This function performs a HTTP GET operation on a URL and returns the content
    as text, and optional ensure that the Unicode encoding is NFC.

    Parameters
    ----------
    sess : requests.sessions.Session
        the :mod:`requests` session to use
    url : str
        the URL
    cookies : dict, optional
        the cookie jar
    debug : bool, optional
        print debug messages
    ensureNFC : bool, optional
        ensure that the text is Unicode NFC rather than Unicode NFD
    headers : dict, optional
        extra headers to send
    timeout : float, optional
        the timeout of the GET request
    verify : bool, optional
        verify the server's certificates

    Returns
    -------
    text : bool, str
        `False` if unsuccessful or a `str` if successful

    Notes
    -----
    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import standard modules ...
    import html
    import unicodedata

    # Import sub-functions ...
    from .download import download

    # Populate default values ...
    if cookies is None:
        cookies = {}
    if headers is None:
        headers = {}

    # **************************************************************************

    # Try to download the page ...
    resp = download(
        sess,
        "get",
        url,
        cookies = cookies,
        headers = headers,
        timeout = timeout,
         verify = verify,
    )

    # Check response ...
    if resp is False:
        return False

    # Convert HTML characters ...
    text = html.unescape(resp.text)

    # Change Unicode encoding if needed ...
    if ensureNFC and unicodedata.is_normalized("NFD", text):
        if debug:
            print(f"DEBUG: Converting \"{url}\" from Unicode NFD to Unicode NFC.")
        text = unicodedata.normalize("NFC", text)

    # Return answer ...
    return text
