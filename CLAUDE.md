# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

stfu-process (Stop Text From Unterse Process) is an Emacs minor-mode that truncates long outputs from sub-processes. It hooks into comint's preoutput filter system to prevent long lines and excessive output from freezing Emacs.

## Build and Test Commands

Run all tests:
```bash
emacs --batch -L . -l stfu-process.el -l test/stfu-process-test.el -f ert-run-tests-batch-and-exit
```

Byte-compile with warnings as errors:
```bash
emacs --batch --eval "(setq byte-compile-error-on-warn t)" -f batch-byte-compile stfu-process.el
```

From Emacs interactively:
```elisp
(ert-run-tests-interactively t)
;; Or specific test:
(ert-run-tests-interactively "stfu-process-test-backspace-reduces-line-length")
```

CI runs on GitHub Actions (Emacs 27.1, 28.1, 29.1, snapshot) with tests, byte-compilation, and package-lint.

## Architecture

The package works by inserting `stfu-process-preoutput-filter` into comint's `comint-preoutput-filter-functions`. Key flow:

1. **Filter placement** (`stfu-process-add-preoutput-filter`): Adds filter to comint's chain. Placement controlled by `stfu-process-add-filter-placement` (prepend if negative, append if non-negative, or use custom function).

2. **Output tracking**: Two buffer-local variables track state:
   - `stfu-process--cur-output-length`: Total output since last "prompt" (reset heuristically based on short outputs)
   - `stfu-process--cur-line-length`: Current line length (accounts for backspaces for in-place updates)

3. **Truncation logic** (`stfu-process-preoutput-filter`): On each output chunk, updates lengths and returns either the original string or a truncation message based on `stfu-process-total-limit` and `stfu-process-line-limit`.

4. **Interactive commands**:
   - `stfu-process-now`: Immediately activates filter and interrupts process (if pty)
   - `stfu-process-ignore`: Activates filter without interrupting

## Key Customization Variables

- `stfu-process-total-limit` (default 100000): Max total output before truncation
- `stfu-process-line-limit` (default 5000): Max line length before line break insertion
- `stfu-process-suppression-string`: Replacement for truncated output
- `stfu-process-add-filter-placement`: Controls filter position in comint chain
