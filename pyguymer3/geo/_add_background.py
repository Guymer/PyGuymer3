def _add_background(axis, kwArgCheck = None, debug = False):
    # NOTE: This function uses CSS4 named colours, see:
    #         * https://matplotlib.org/stable/gallery/color/named_colors.html

    # Import special modules ...
    try:
        import cartopy
    except:
        raise Exception("\"cartopy\" is not installed; run \"pip install --user Cartopy\"") from None
    try:
        import matplotlib
    except:
        raise Exception("\"matplotlib\" is not installed; run \"pip install --user matplotlib\"") from None
    try:
        import shapely
        import shapely.geometry
        import shapely.validation
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Import sub-functions ...
    from ._debug import _debug

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # **************************************************************************

    # Create suitable colour ...
    facecolor = matplotlib.colors.to_rgba(matplotlib.colors.CSS4_COLORS["lightblue"])
    if debug:
        print(f"INFO: \"background\" is ({facecolor[0]:.6f},{facecolor[1]:.6f},{facecolor[2]:.6f},{facecolor[3]:.6f}).")

    # Define Earth (as a correctly oriented Polygon) ...
    earth = shapely.geometry.polygon.Polygon(
        [
            (-180.0,  +90.0),
            (-180.0,  -90.0),
            (+180.0,  -90.0),
            (+180.0,  +90.0),
            (-180.0,  +90.0),
        ]
    )
    if not isinstance(earth, shapely.geometry.polygon.Polygon):
        raise Exception("\"earth\" is not a Polygon") from None
    if not earth.is_valid:
        _debug(earth)
        raise Exception(f"\"earth\" is not a valid Polygon ({shapely.validation.explain_validity(earth)})") from None
    if earth.is_empty:
        raise Exception("\"earth\" is an empty Polygon") from None

    # Plot geometry ...
    axis.add_geometries(
        [earth],
        cartopy.crs.PlateCarree(),
        edgecolor = "none",
        facecolor = facecolor,
    )
