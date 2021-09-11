def download_text(sess, url, kwArgCheck = None, timeout = 10.0, verify = True):
    # Import standard modules ...
    import html

    # Import sub-functions ...
    from .download import download

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Try to download the page ...
    resp = download(sess, "get", url, timeout = timeout, verify = verify)

    # Check response ...
    if resp is False:
        return False

    return html.unescape(resp.text)
