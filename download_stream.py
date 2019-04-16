def download_stream(sess, url):
    # Load sub-functions ...
    from .download import download

    # Try to download the file and catch common errors ...
    resp = download(sess, "get", url)
    if resp is False:
        return False

    return resp.content
