#!/usr/bin/env python3

# Define function ...
def start_session():
    # Import special modules ...
    try:
        import requests
    except:
        raise Exception("\"requests\" is not installed; run \"pip install --user requests\"") from None

    # Start session ...
    sess = requests.Session()
    sess.allow_redirects = True
    sess.headers.update(
        {
                               "Accept" : "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
                      "Accept-Encoding" : "gzip, deflate, br",
                      "Accept-Language" : "en-GB,en;q=0.9",
                                  "DNT" : "1",
            "Upgrade-Insecure-Requests" : "1",
                           "User-Agent" : "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.2 Safari/605.1.15",
        }
    )
    sess.max_redirects = 5

    # Return answer ...
    return sess
