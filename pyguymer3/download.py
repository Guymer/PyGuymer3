def download(sess, method, url, kwArgCheck = None, cookies = {}, headers = {}, timeout = 10.0, verify = True):
    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Try to download the URL and catch common errors ...
    try:
        resp = sess.request(method, url, cookies = cookies, headers = headers, timeout = timeout, verify = verify)
    except:
        return False

    # Exit if the response was bad ...
    if resp.status_code != 200:
        return False

    return resp
