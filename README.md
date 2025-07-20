# cl-win32-types

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Quicklisp](https://img.shields.io/badge/Quicklisp-available-brightgreen.svg)](http://quicklisp.org/)

A comprehensive set of CFFI type definitions for the Windows API, designed for seamless integration with modern Common Lisp development environments on Windows.

## Overview

This package provides an exhaustive set of 45 CFFI type definitions for the Windows C API (e.g., `DWORD`, `HANDLE`, `WCHAR`, `BOOL`). It is built for production use with Common Lisp (i.e., SBCL) on Windows. The system is designed to be modular, well-documented, and includes very thorough test and benchmark suites.

## Features

* **Comprehensive Type Coverage**: Defines a wide range of standard Windows C types.
* **Lisp-Idiomatic Naming**: Type names are lowercased (e.g., `dword` instead of `DWORD`) and use a `win-` prefix for types that clash with Common Lisp symbols (e.g., `win-char`).
* **Platform Detection**: Includes compile-time checks to ensure pointer-sized types are defined correctly for 32-bit and 64-bit platforms, with a clear error for unsupported environments.
* **ASDF System**: Ready for straightforward use via Quicklisp. The library, tests, and benchmarks are defined as separate systems.
* **Comprehensive Test Suite**: Includes a full suite of unit tests using FiveAM to verify type sizes, ranges, and behavior.
* **Performance Benchmarks**: Provides a detailed benchmark suite to measure FFI call overhead, memory allocation, and type conversion performance.

## Installation (for Users)

Once available in Quicklisp, you can load it with:

```lisp
(ql:quickload :cl-win32-types)
```
## Developer Setup (for Contributors)

This project uses **Qlot** to manage and lock dependency versions for reproducible builds.

1.  **Install Qlot** (one-time setup):
    ```bash
    ros install qlot
    ```

2.  **Install Project Dependencies**: This command reads the `qlfile.lock` and installs the exact versions of all dependencies into a local `.qlot/` directory.
    ```bash
    # Make sure you are in the project's root directory
    qlot install
    ```

3.  **Start a REPL**: To work on the project, start your REPL using `qlot exec`. This ensures your Lisp session uses the project's local dependencies.
    ```bash
    qlot exec ros run
    ```
---

## Usage Example

Reference the types in your CFFI definitions. All types are exported from the `cl-win32-types` package.

```lisp
(defpackage #:my-windows-app
  (:use #:cl #:cffi #:cl-win32-types))

(in-package #:my-windows-app)

(cffi:defcfun ("MessageBoxW" message-box-w) int
  (hwnd    hwnd)
  (text    lpcwstr)
  (caption lpcwstr)
  (type    uint))

(defun show-hello-dialog ()
  (message-box-w (cffi:null-pointer) "Hello from Common Lisp!" "My App" 0))
```

## Running Tests and Benchmarks

* **To run the test suite**:
    ```lisp
    (asdf:test-system :cl-win32-types)
    ```
  * **Or with qlot:**
    ```powershell
    qlot exec ros run --eval "(asdf:test-system :cl-win32-types)" --quit
    ```


* **To run the benchmarks**:
    ```lisp
    (ql:quickload :cl-win32-types/benchmarks)
    (cl-win32-types/benchmarks:run-all-benchmarks)
    ```
  * **Or with qlot:**
    ```powershell
    qlot exec ros --eval "(ql:quickload :cl-win32-types/benchmarks)" --eval "(cl-win32-types/benchmarks:run-all-benchmarks)" --quit
    ```
<details>
<summary>Click to see sample benchmark output</summary>

```text
=== CL-WIN32-TYPES BENCHMARK SUITE ===

--- Basic Type Benchmarks ---
GetTickCount (DWORD return):
-          SAMPLES  TOTAL     MINIMUM   MAXIMUM   MEDIAN    AVERAGE    DEVIATION
REAL-TIME        3  0.002202  0.000583  0.000939  0.00068   0.000734   0.00015
...

--- Call Overhead Comparison ---
Native Lisp function call:
-          SAMPLES  TOTAL     MINIMUM   MAXIMUM   MEDIAN    AVERAGE    DEVIATION
REAL-TIME        3  0.001002  0.000288  0.00042   0.000294  0.000334   0.000061

CFFI function call (no args):
-          SAMPLES  TOTAL     MINIMUM   MAXIMUM   MEDIAN    AVERAGE    DEVIATION
REAL-TIME        3  0.022551  0.006851  0.008161  0.007539  0.007517   0.000535

...

=== BENCHMARK SUITE COMPLETE ===
```

</details>

## Contributing

Bug reports and pull requests are welcome on GitHub. Please ensure the test suite passes by running `(asdf:test-system :cl-win32-types)` before submitting a pull request.

## License

This project is licensed under the **MIT License**. See the `LICENSE` file for details.