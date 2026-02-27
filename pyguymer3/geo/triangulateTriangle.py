#!/usr/bin/env python3

# Define function ...
def triangulateTriangle(
    triangle,
    /,
    *,
    splitSpace = "EuclideanSpace",
):
    """Split a triangle up in to four triangles.

    Parameters
    ----------
    triangle : shapely.geometry.polygon.Polygon
        the triangle

    Returns
    -------
    triangles : list of shapely.geometry.polygon.Polygon
        the four triangles
    splitSpace : str, optional
        the geometric space to perform the splitting in (either "EuclideanSpace"
        or "GeodesicSpace")

    Notes
    -----
    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import special modules ...
    try:
        import shapely
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Import sub-functions ...
    from .find_middle_of_great_circle import find_middle_of_great_circle

    # **************************************************************************

    # Check input ...
    assert isinstance(triangle, shapely.geometry.polygon.Polygon), "\"triangle\" is not a Polygon"
    assert len(triangle.exterior.coords) == 4, "\"triangle\" is not a triangle"
    assert len(triangle.interiors) == 0, "\"triangle\" has holes"

    # Create short-hands ...
    a = triangle.exterior.coords[0]
    b = triangle.exterior.coords[1]
    c = triangle.exterior.coords[2]

    # Check what space the user wants to split in ...
    match splitSpace:
        case "EuclideanSpace":
            # Calculate middles ...
            ab = (
                0.5 * (a[0] + b[0]),
                0.5 * (a[1] + b[1]),
            )
            ac = (
                0.5 * (a[0] + c[0]),
                0.5 * (a[1] + c[1]),
            )
            bc = (
                0.5 * (b[0] + c[0]),
                0.5 * (b[1] + c[1]),
            )
        case "GeodesicSpace":
            # Calculate middles ...
            ab = find_middle_of_great_circle(
                    a[0],
                    a[1],
                    b[0],
                    b[1],
                )
            ac = find_middle_of_great_circle(
                    a[0],
                    a[1],
                    c[0],
                    c[1],
                )
            bc = find_middle_of_great_circle(
                    b[0],
                    b[1],
                    c[0],
                    c[1],
                )
        case _:
            # Crash ...
            raise ValueError(f"\"splitSpace\" is an unexpected value ({repr(splitSpace)})") from None

    # Return answer ...
    return [
        shapely.geometry.polygon.Polygon(
            shapely.geometry.polygon.LinearRing(
                [
                    a,
                    ab,
                    ac,
                    a,
                ]
            )
        ),
        shapely.geometry.polygon.Polygon(
            shapely.geometry.polygon.LinearRing(
                [
                    b,
                    bc,
                    ab,
                    b,
                ]
            )
        ),
        shapely.geometry.polygon.Polygon(
            shapely.geometry.polygon.LinearRing(
                [
                    c,
                    ac,
                    bc,
                    c,
                ]
            )
        ),
        shapely.geometry.polygon.Polygon(
            shapely.geometry.polygon.LinearRing(
                [
                    ab,
                    bc,
                    ac,
                    ab,
                ]
            )
        ),
    ]
