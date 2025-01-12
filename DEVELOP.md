## Development

To test the build locally first, run:

```sh
python3.12 -m build &> build.log
```

To release a new version to both GitHub and PyPI, run:

```sh
git tag -s -m "does this GitHub Action work?" v0.0.1
git push --tags
```
