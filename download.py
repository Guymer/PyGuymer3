def download(sess, method, url):
    # Import modules ...
    import requests

    # Try to download the URL and catch common errors ...
    try:
        resp = sess.request(method, url, timeout = 10.0)
    except requests.exceptions.TooManyRedirects:
        return False
    except requests.exceptions.ConnectionError:
        return False
    except requests.exceptions.Timeout:
        return False

    # Exit if the response was bad ...
    if resp.status_code != 200:
        return False

    return resp
