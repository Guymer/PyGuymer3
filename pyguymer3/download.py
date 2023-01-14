#!/usr/bin/env python3

# Define function ...
def download(sess, method, url, kwArgCheck = None, cookies = None, headers = None, timeout = 10.0, verify = True):
    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Populate default values ...
    if cookies is None:
        cookies = {}
    if headers is None:
        headers = {}

    # **************************************************************************

    # Catch common errors ...
    if "&amp;" in url:
        raise Exception("the URL contains \"&amp;\"") from None

    # Try to download the URL and catch common errors ...
    try:
        resp = sess.request(method, url, cookies = cookies, headers = headers, timeout = timeout, verify = verify)
    except:
        return False

    # Exit if the response was bad ...
    if resp.status_code != 200:
        return False

    return resp
