def make_path_safe(path):
    # Loop over characters ...
    for illegal_char in ["\\", "/", ":", "*", "?", "\"", "<", ">", "|", "%"]:
        path = path.replace(illegal_char, "")

    # Make the file visible ...
    if path[0:1] == ".":
        path = " " + path

    return path
