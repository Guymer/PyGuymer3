def download_text(sess, url, verify = True):
    # Import modules ...
    import html

    # Load sub-functions ...
    from .download import download

    # Try to download the page and catch common errors ...
    resp = download(sess, "get", url, verify = verify)
    if resp is False:
        return False

    return html.unescape(resp.text)
