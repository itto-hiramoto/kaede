# Homebrew packaging

This directory holds a draft Homebrew formula for kaede. The canonical copy
once published lives in a separate tap repo:

- Tap repo: `itto-hiramoto/homebrew-kaede`
- Formula path in tap: `Formula/kaede.rb`

Users install via:

```sh
brew tap itto-hiramoto/kaede
brew install kaede
```

## Cutting a release

1. Tag the release on `main`:

   ```sh
   git tag -a vX.Y.Z -m "Release vX.Y.Z"
   git push origin vX.Y.Z
   ```

2. Compute the source tarball sha256:

   ```sh
   curl -sL "https://github.com/itto-hiramoto/kaede/archive/refs/tags/vX.Y.Z.tar.gz" \
     | shasum -a 256
   ```

3. Update `kaede.rb` in the tap repo:
   - `url` → the new `vX.Y.Z` tag URL
   - `sha256` (top-level) → the value from step 2

4. If the `library/bdwgc` submodule moved since the last release, also update
   the bdwgc resource block:

   ```sh
   git -C library/bdwgc rev-parse HEAD                          # new commit
   curl -sL "https://github.com/ivmai/bdwgc/archive/<commit>.tar.gz" | shasum -a 256
   ```

   Replace the `resource "bdwgc"` URL commit hash and `sha256` accordingly.

5. Once the tap is live, prefer `brew bump-formula-pr` to automate steps 2–3
   from a CI workflow:

   ```sh
   brew bump-formula-pr --strict \
     --url="https://github.com/itto-hiramoto/kaede/archive/refs/tags/vX.Y.Z.tar.gz" \
     itto-hiramoto/kaede/kaede
   ```

## Local testing (no tap repo required)

From a clone of this repo, install the formula in-place:

```sh
brew install --build-from-source ./packaging/homebrew/kaede.rb
brew test kaede
brew audit --new --strict ./packaging/homebrew/kaede.rb
```

To test against the live tap once published:

```sh
brew untap itto-hiramoto/kaede 2>/dev/null
brew tap itto-hiramoto/kaede
brew install --verbose --debug kaede
```

## Why a `resource` for bdwgc?

`library/bdwgc` is a git submodule, and GitHub release tarballs from
`archive/refs/tags/...` do **not** include submodule contents. Without the
resource block, `install.py` fails when cmake tries to build an empty
`library/bdwgc/` directory. The resource stages the pinned bdwgc tarball
into the build tree before `install.py` runs.
