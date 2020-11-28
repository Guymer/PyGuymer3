def download_file(sess, url, fname, timeout = 10.0, verify = True):
    # Import standard modules ...
    import email.utils
    import os

    # Load sub-functions ...
    from .download import download

    # Try to download the file and catch common errors ...
    resp = download(sess, "get", url, timeout = timeout, verify = verify)
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
