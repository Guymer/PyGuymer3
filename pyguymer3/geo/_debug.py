def _debug(badGeoms, kwArgCheck = None, prefix = "."):
    """Save CSVs for debugging

    Parameters
    ----------
    badGeoms : shapely.geometry.multilinestring.LineString, shapely.geometry.multilinestring.MultiLineString, shapely.geometry.polygon.Polygon, shapely.geometry.multipolygon.MultiPolygon
        the bad [Multi]LineString or [Multi]Polygon

    Notes
    -----
    Copyright 2018 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import sub-functions ...
    from .extract_lines import extract_lines
    from .extract_polys import extract_polys

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Loop over all the bad LineStrings in this [Multi]LineString ...
    for i, badGeom in enumerate(extract_lines(badGeoms)):
        # Open output file ...
        print(f"ERROR: Writing \"debug{prefix}LineString{i:d}.csv\" ...")
        with open(f"debug{prefix}LineString{i:d}.csv", "wt", encoding = "utf-8") as fobj:
            # Loop over bad coordinates of this bad LineString ...
            for badCoord in badGeom.coords:
                # Write output ...
                fobj.write(f"{badCoord[0]:.15e},{badCoord[1]:.15e}\n")

    # Loop over all the bad Polygons in this [Multi]Polygon ...
    for i, badGeom in enumerate(extract_polys(badGeoms)):
        # Open output file ...
        print(f"ERROR: Writing \"debug{prefix}Polygon{i:d}.exterior.csv\" ...")
        with open(f"debug{prefix}Polygon{i:d}.exterior.csv", "wt", encoding = "utf-8") as fobj:
            # Loop over bad coordinates in the exterior ring of this bad Polygon ...
            for badCoord in badGeom.exterior.coords:
                # Write output ...
                fobj.write(f"{badCoord[0]:.15e},{badCoord[1]:.15e}\n")

        # Loop over bad interior rings of this bad Polygon ...
        for j, interior in enumerate(badGeom.interiors):
            # Open output file ...
            print(f"ERROR: Writing \"debug{prefix}Polygon{i:d}.interior{j:d}.csv\" ...")
            with open(f"debug{prefix}Polygon{i:d}.interior{j:d}.csv", "wt", encoding = "utf-8") as fobj:
                # Loop over bad coordinates in this interior ring of this bad
                # Polygon ...
                for badCoord in interior.coords:
                    # Write output ...
                    fobj.write(f"{badCoord[0]:.15e},{badCoord[1]:.15e}\n")
