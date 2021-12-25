def download_file(sess, url, fname, kwArgCheck = None, cookies = {}, timeout = 10.0, verify = True):
    # Import standard modules ...
    import email
    import email.utils
    import os

    # Import sub-functions ...
    from .download import download

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Try to download the file ...
    resp = download(sess, "get", url, cookies = cookies, timeout = timeout, verify = verify)

    # Check response ...
    if resp is False:
        return False

    # Save file to disk ...
    dname = os.path.dirname(fname)
    if len(dname) > 0:
        if not os.path.exists(dname):
            os.makedirs(dname)
    open(fname, "wb").write(resp.content)

    # Change modification time if present ...
    if "Last-Modified" in resp.headers:
        modtime = email.utils.mktime_tz(email.utils.parsedate_tz(resp.headers["Last-Modified"]))
        os.utime(fname, (modtime, modtime))

    return True
