# ekphrasis

Check what a user interface actually renders against a written description of what it should show.

## Requirements

- SBCL 2.x
- ASDF 3.x
- All deps available via `$LISP_WORKSPACE/` or Quicklisp

## Load

```lisp
(asdf:load-system :ekphrasis)
```

## Test

```lisp
(asdf:test-system :ekphrasis)
```

## Run

```bash
# Foreground:
sbcl --eval '(asdf:load-system :ekphrasis)' --eval '(ekphrasis/src/main:main)'

# Build binary:
./build.sh

# Dev image (Slynk listener):
./scripts/dev-boot.sh
```

## License

AGPL-3.0-or-later; see `LICENSE`.

Copyright (c) 2026 Brian O'Reilly.
