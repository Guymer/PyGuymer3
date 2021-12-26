def download_stream(sess, url, kwArgCheck = None, cookies = {}, headers = {}, timeout = 10.0, verify = True):
    # Import sub-functions ...
    from .download import download

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Try to download the file ...
    resp = download(sess, "get", url, cookies = cookies, headers = headers, timeout = timeout, verify = verify)

    # Check response ...
    if resp is False:
        return False

    return resp.content
