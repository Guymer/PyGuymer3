def download_header(sess, url, kwArgCheck = None, timeout = 10.0, verify = True):
    # Load sub-functions ...
    from .download import download

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Try to get the headers ...
    resp = download(sess, "head", url, timeout = timeout, verify = verify)

    # Check response ...
    if resp is False:
        # Clean up ...
        del resp

        return False

    return resp.headers
