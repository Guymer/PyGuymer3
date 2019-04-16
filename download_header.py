def download_header(sess, url):
    # Load sub-functions ...
    from .download import download

    # Try to get the headers and catch common errors ...
    resp = download(sess, "head", url)
    if resp is False:
        return False

    return resp.headers
