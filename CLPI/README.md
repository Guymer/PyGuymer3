# PyGuymer3.CLPI

This sub-module is a native Python implementation of a parser for Blu-ray CLPI files. It has used [an excellent CLPI Wiki](https://github.com/lerks/BluRay/wiki/CLPI) and was initially provided by [lonecrane](https://github.com/lonecrane) - thank you very much!

## Usage

The below code will print out the entire dictionary for you.

```python
#!/usr/bin/env python3

# Import standard modules ...
import json

# Import my modules ...
import pyguymer3

# Parse CLPI file ...
info = pyguymer3.parse_CLPI_file("/path/to/blu-ray", 200)

# Print dictionary ...
print(json.dumps(info, ensure_ascii = False, indent = 4, sort_keys = True))
```
