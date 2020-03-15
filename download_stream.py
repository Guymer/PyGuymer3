def download_stream(sess, url, timeout = 10.0, verify = True):
    # Load sub-functions ...
    from .download import download

    # Try to download the file and catch common errors ...
    resp = download(sess, "get", url, timeout = timeout, verify = verify)
    if resp is False:
        return False

    return resp.content
