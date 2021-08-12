def download(sess, method, url, kwArgCheck = None, timeout = 10.0, verify = True):
    # Import special modules ...
    try:
        import requests
    except:
        raise Exception("\"requests\" is not installed; run \"pip install --user requests\"") from None

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Try to download the URL and catch common errors ...
    try:
        resp = sess.request(method, url, timeout = timeout, verify = verify)
    except requests.exceptions.ConnectionError:
        # Clean up ...
        del resp

        return False
    except requests.exceptions.ReadTimeout:
        # Clean up ...
        del resp

        return False
    except requests.exceptions.Timeout:
        # Clean up ...
        del resp

        return False
    except requests.exceptions.TooManyRedirects:
        # Clean up ...
        del resp

        return False

    # Exit if the response was bad ...
    if resp.status_code != 200:
        # Clean up ...
        del resp

        return False

    return resp
