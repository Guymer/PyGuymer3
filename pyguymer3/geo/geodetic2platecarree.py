#!/usr/bin/env python3

# Define function ...
def geodetic2platecarree(
    geoms,
    /,
):
    # Import special modules ...
    try:
        import pyproj
    except:
        raise Exception("\"pyproj\" is not installed; run \"pip install --user pyproj\"") from None
    try:
        import shapely
        import shapely.ops
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Import sub-functions ...
    from .._consts import GEODETIC, PLATECARREE

    # **************************************************************************

    # Return answer ...
    return [
        shapely.ops.transform(
            pyproj.Transformer.from_crs(
                GEODETIC._crs,
                PLATECARREE._crs,
                always_xy = True,
            ).transform,
            geom,
        ) for geom in geoms
    ]
