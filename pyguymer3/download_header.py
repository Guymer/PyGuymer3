#!/usr/bin/env python3

# Define function ...
def download_header(sess, url, /, *, cookies = None, headers = None, timeout = 10.0, verify = True):
    # Import sub-functions ...
    from .download import download

    # Populate default values ...
    if cookies is None:
        cookies = {}
    if headers is None:
        headers = {}

    # **************************************************************************

    # Try to get the headers ...
    resp = download(sess, "head", url, cookies = cookies, headers = headers, timeout = timeout, verify = verify)

    # Check response ...
    if resp is False:
        return False

    return resp.headers
