#!/usr/bin/env python3

# Define function ...
def _set_axis_boundary(
    ax,
    centreLl,
    bufferLl,
    intendedDist,
    /,
    *,
      eps = 1.0e-12,
    nIter = 100,
      tol = 1.0e-10,
):
    # Import standard modules ...
    import math

    # Import special modules ...
    try:
        import matplotlib
        matplotlib.rcParams.update(
            {
                       "axes.xmargin" : 0.01,
                       "axes.ymargin" : 0.01,
                            "backend" : "Agg",                                  # NOTE: See https://matplotlib.org/stable/gallery/user_interfaces/canvasagg.html
                         "figure.dpi" : 300,
                     "figure.figsize" : (9.6, 7.2),                             # NOTE: See https://github.com/Guymer/misc/blob/main/README.md#matplotlib-figure-sizes
                          "font.size" : 8,
                "image.interpolation" : "none",                                 # NOTE: See https://matplotlib.org/stable/gallery/images_contours_and_fields/interpolation_methods.html
                     "image.resample" : False,
            }
        )
        import matplotlib.pyplot
    except:
        raise Exception("\"matplotlib\" is not installed; run \"pip install --user matplotlib\"") from None
    try:
        import shapely
        import shapely.geometry
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Import sub-functions ...
    from .calc_dist_between_two_locs import calc_dist_between_two_locs
    from .extract_polys import extract_polys
    from .._consts import RESOLUTION_OF_EARTH

    # **************************************************************************

    # Create short-hand ...
    centreMpl = ax.projection.project_geometry(centreLl)

    # Initialise lists and minimum/maximum values ...
    bearings = []                                                               # [°]
    distsMpl = []                                                               # [?]
    maxMplX = -9.9e+99                                                          # [?]
    maxMplY = -9.9e+99                                                          # [?]
    minMplX = +9.9e+99                                                          # [?]
    minMplY = +9.9e+99                                                          # [?]
    pointsMpl = []

    # Loop over Polygons in the [Multi]Polygon buffer of the Point ...
    for polyLl in extract_polys(
        bufferLl,
        onlyValid = True,
           repair = False,
    ):
        # Loop over Points in the exterior LinearRing of the Polygon ...
        for pointLl in shapely.geometry.multipoint.MultiPoint(polyLl.exterior.coords).geoms:
            # Skip this exterior Point if both it and the centre Point are at a
            # pole (as they are, therefore, physically at the same place on
            # Earth, regardless as to what the following maths says) ...
            if math.isclose(
                90.0,
                abs(centreLl.y),
                abs_tol = tol,
            ) and math.isclose(
                90.0,
                abs(pointLl.y),
                abs_tol = tol,
            ):
                continue

            # Calculate the distance between, and the bearing from, the centre
            # Point to the exterior Point ...
            actualDist, bearing, _ = calc_dist_between_two_locs(
                centreLl.x,
                centreLl.y,
                pointLl.x,
                pointLl.y,
                  eps = eps,
                nIter = nIter,
            )                                                                   # [m], [°]

            # Skip this exterior Point if it is not on the buffer (i.e., it is
            # an extra exterior Point to make the buffer look good, e.g., a
            # slice along the anti-meridean) ...
            if not math.isclose(
                intendedDist,
                actualDist,
                abs_tol = tol * RESOLUTION_OF_EARTH,
            ):
                continue

            # Project the exterior Point and append it to the list ...
            pointMpl = ax.projection.project_geometry(pointLl)
            pointsMpl.append(pointMpl)

            # Append the bearing from the centre Point to the exterior Point to
            # the list ...
            bearings.append(bearing)                                            # [°]

            # Append the distance between the projected centre Point to the
            # projected exterior Point to the list ...
            distsMpl.append(
                math.hypot(
                    pointMpl.x - centreMpl.x,
                    pointMpl.y - centreMpl.y,
                )
            )                                                                   # [?]

            # Update the minimum/maximum values ...
            maxMplX = max(
                maxMplX,
                pointMpl.x,
            )                                                                   # [?]
            maxMplY = max(
                maxMplY,
                pointMpl.y,
            )                                                                   # [?]
            minMplX = min(
                minMplX,
                pointMpl.x,
            )                                                                   # [?]
            minMplY = min(
                minMplY,
                pointMpl.y,
            )                                                                   # [?]

    # Sort the projected exterior Point list by the bearings and clean up ...
    idxs = sorted(
        range(len(bearings)),
        key = lambda idx: bearings[idx],
    )
    del bearings
    pointsMpl = [pointsMpl[idx] for idx in idxs]
    del idxs

    # Make a Path of the projected exterior Points and clean up ...
    ringMpl = shapely.geometry.polygon.LinearRing(pointsMpl)
    pathMpl = matplotlib.path.Path(ringMpl.coords)
    del pointsMpl

    # Configure axis ...
    ax.set_boundary(pathMpl)
    ax.set_xlim(
        minMplX,
        maxMplX,
    )
    ax.set_ylim(
        minMplY,
        maxMplY,
    )

    # Calculate the average distance between the projected centre Point and the
    # projected exterior Points ...
    avgDistMpl = sum(distsMpl) / float(len(distsMpl))                           # [?]

    # Return the minimum and maximum distances relative to the average ...
    return min(distsMpl) / avgDistMpl, max(distsMpl) / avgDistMpl
