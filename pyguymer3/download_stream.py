#!/usr/bin/env python3

# Define function ...
def download_stream(sess, url, /, *, cookies = None, headers = None, timeout = 10.0, verify = True):
    # Import sub-functions ...
    from .download import download

    # Populate default values ...
    if cookies is None:
        cookies = {}
    if headers is None:
        headers = {}

    # **************************************************************************

    # Try to download the file ...
    resp = download(sess, "get", url, cookies = cookies, headers = headers, timeout = timeout, verify = verify)

    # Check response ...
    if resp is False:
        return False

    return resp.content
