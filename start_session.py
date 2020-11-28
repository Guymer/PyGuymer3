def start_session():
    # Import special modules ...
    try:
        import requests
    except:
        raise Exception("\"requests\" is not installed; run \"pip install --user requests\"") from None

    # Start session ...
    sess = requests.Session()
    sess.allow_redirects = True
    sess.headers.update({"Accept" : "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"})
    sess.headers.update({"Accept-Language" : "en-GB,en;q=0.5"})
    sess.headers.update({"Accept-Encoding" : "gzip, deflate"})
    sess.headers.update({"DNT" : "1"})
    sess.headers.update({"Upgrade-Insecure-Requests" : "1"})
    sess.headers.update({"User-Agent" : "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.14; rv:68.0) Gecko/20100101 Firefox/68.0"})
    sess.max_redirects = 5

    # Return answer ...
    return sess