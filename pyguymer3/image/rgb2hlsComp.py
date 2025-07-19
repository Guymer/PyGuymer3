#!/usr/bin/env python3

# Define function ...
def rgb2hlsComp(r, g, b, /):
    """
    This function returns the HLS complementary colour of an RGB triplet.
    """

    # Import standard modules ...
    import colorsys

    # Convert the RGB triplet to an HLS triplet ...
    h, l, s = colorsys.rgb_to_hls(
        float(r) / 255.0,
        float(g) / 255.0,
        float(b) / 255.0,
    )

    # Convert the HLS triplet into its HLS complementary colour ...
    if h > 0.5:
        h -= 0.5
    else:
        h += 0.5

    # Convert the HLS triplet to an RGB triplet ...
    tmpR, tmpG, tmpB = colorsys.hls_to_rgb(h, l, s)
    tmpR = round(tmpR * 255.0)
    tmpG = round(tmpG * 255.0)
    tmpB = round(tmpB * 255.0)

    # Return answer ...
    return tmpR, tmpG, tmpB
