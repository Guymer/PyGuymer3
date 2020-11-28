def download_header(sess, url, timeout = 10.0, verify = True):
    # Load sub-functions ...
    from .download import download

    # Try to get the headers and catch common errors ...
    resp = download(sess, "head", url, timeout = timeout, verify = verify)
    if resp is False:
        return False

    return resp.headers