def download_text(sess, url, timeout = 10.0, verify = True):
    # Import standard modules ...
    import html

    # Load sub-functions ...
    from .download import download

    # Try to download the page and catch common errors ...
    resp = download(sess, "get", url, timeout = timeout, verify = verify)
    if resp is False:
        return False

    return html.unescape(resp.text)
