def download_text(sess, url):
    # Load sub-functions ...
    from .download import download

    # Try to download the page and catch common errors ...
    resp = download(sess, "get", url)
    if resp is False:
        return False

    return resp.text.encode("utf8", "xmlcharrefreplace")
