#!/usr/bin/env python3

# Define function ...
def download_text(sess, url, /, *, cookies = None, headers = None, timeout = 10.0, verify = True):
    # Import standard modules ...
    import html

    # Import sub-functions ...
    from .download import download

    # Populate default values ...
    if cookies is None:
        cookies = {}
    if headers is None:
        headers = {}

    # **************************************************************************

    # Try to download the page ...
    resp = download(sess, "get", url, cookies = cookies, headers = headers, timeout = timeout, verify = verify)

    # Check response ...
    if resp is False:
        return False

    return html.unescape(resp.text)
