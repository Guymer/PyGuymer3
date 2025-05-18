#!/usr/bin/env python3

# Define function ...
def download(
    sess,
    method,
    url,
    /,
    *,
    cookies = None,
    headers = None,
    timeout = 10.0,
     verify = True,
):
    """Call a URL and return the response

    This function performs a HTTP call, with a GET/POST/PUT/HEAD/OPTIONS method,
    on a URL and returns the response.

    Parameters
    ----------
    sess : requests.sessions.Session
        the :mod:`requests` session to use
    meth : str
        the method
    url : str
        the URL
    cookies : dict, optional
        the cookie jar
    headers : dict, optional
        extra headers to send
    timeout : float, optional
        the timeout of the GET/POST/PUT/HEAD/OPTIONS request
    verify : bool, optional
        verify the server's certificates

    Returns
    -------
    resp : bool, requests.models.Response
        `False` if unsuccessful or a `requests.models.Response` if successful

    Notes
    -----
    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Populate default values ...
    if cookies is None:
        cookies = {}
    if headers is None:
        headers = {}

    # **************************************************************************

    # Catch common errors ...
    if "&amp;" in url:
        raise Exception("the URL contains \"&amp;\"") from None

    # Try to download the URL and catch common errors ...
    try:
        resp = sess.request(
            method,
            url,
            cookies = cookies,
            headers = headers,
            timeout = timeout,
             verify = verify,
        )
    except:
        return False

    # Exit if the response was bad ...
    if resp.status_code != 200:
        return False

    # Return answer ...
    return resp
