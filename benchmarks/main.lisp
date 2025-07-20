;;;; benchmarks/main.lisp
#|
Copyright (c) 2025 Andrew D. France

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
 |#
(defpackage #:cl-win32-types/benchmarks
  (:use #:cl #:cffi #:trivial-benchmark #:cl-win32-types)
  (:export #:run-benchmarks
           #:run-all-benchmarks
           #:compare-call-overhead
           #:memory-allocation-analysis))

(in-package #:cl-win32-types/benchmarks)

#+(or :win32 :windows)
(progn
  ;; Basic API calls for testing different return types
  (cffi:defcfun ("GetTickCount" get-tick-count) dword)
  (cffi:defcfun ("GetCurrentProcessId" get-current-process-id) dword)
  (cffi:defcfun ("GetLastError" get-last-error) dword)
  (cffi:defcfun ("GetActiveWindow" get-active-window) hwnd)
  (cffi:defcfun ("GetDC" get-dc) hdc (hwnd hwnd))
  (cffi:defcfun ("ReleaseDC" release-dc) :int (hwnd hwnd) (hdc hdc))
  
  ;; String functions
  (cffi:defcfun ("GetWindowTextA" get-window-text-a) :int
    (hwnd hwnd) (lpstring lpstr) (max-count :int))
  (cffi:defcfun ("GetWindowTextW" get-window-text-w) :int
    (hwnd hwnd) (lpstring lpwstr) (max-count :int))
  
  ;; Memory allocation functions
  (cffi:defcfun ("GlobalAlloc" global-alloc) handle
    (flags :uint) (bytes dword))
  (cffi:defcfun ("GlobalFree" global-free) handle
    (mem handle)))

(defun run-all-benchmarks ()
  "Runs the comprehensive benchmark and analysis suite."
  #-(or :win32 :windows)
  (format t "~&Benchmarks are only available on Windows.~%")
  
  #+(or :win32 :windows)
  (progn
    (format t "~&=== CL-WIN32-TYPES BENCHMARK SUITE ===~%~%")
    (benchmark-basic-types)
    (benchmark-handle-types)
    (benchmark-string-operations)
    (benchmark-memory-operations)
    (benchmark-type-conversions)
    (compare-call-overhead)
    (memory-allocation-analysis)
    (format t "~&=== BENCHMARK SUITE COMPLETE ===~%")))

(defun run-benchmarks ()
  "Runs the original basic CFFI overhead benchmark."
  #-(or :win32 :windows)
  (format t "~&Benchmarks are only available on Windows.~%")

  #+(or :win32 :windows)
  (progn
    (format t "~&--- Running CFFI Call Overhead Benchmark ---~%")
    (format t "Measuring the time to make 1,000,000 calls to GetTickCount.~%~%")
    (time-cffi-call)
    (format t "~&--------------------------------------------~%"))
  (values))

#+(or :win32 :windows)
(defun benchmark-basic-types ()
  "Benchmark basic Windows type operations."
  (format t "~&--- Basic Type Benchmarks ---~%")
  (format t "GetTickCount (DWORD return): ")
  (with-timing (3) (dotimes (i 100000) (get-tick-count)))
  (format t "GetCurrentProcessId (DWORD return): ")
  (with-timing (3) (dotimes (i 100000) (get-current-process-id)))
  (format t "GetLastError (DWORD return): ")
  (with-timing (3) (dotimes (i 100000) (get-last-error)))
  (terpri))

#+(or :win32 :windows)
(defun benchmark-handle-types ()
  "Benchmark handle type operations."
  (format t "~&--- Handle Type Benchmarks ---~%")
  (format t "GetActiveWindow (HWND return): ")
  (with-timing (3) (dotimes (i 50000) (get-active-window)))
  (let ((hwnd (get-active-window)))
    (when (not (cffi:null-pointer-p hwnd))
      (format t "GetDC/ReleaseDC (handle parameters): ")
      (with-timing (3)
        (dotimes (i 10000)
          (let ((hdc (get-dc hwnd)))
            (unless (cffi:null-pointer-p hdc)
              (release-dc hwnd hdc)))))))
  (terpri))

#+(or :win32 :windows)
(defun benchmark-string-operations ()
  "Benchmark string type operations."
  (format t "~&--- String Type Benchmarks ---~%")
  (let ((hwnd (get-active-window)))
    (if (cffi:null-pointer-p hwnd)
        (format t "No active window found, skipping string benchmarks.~%")
        (progn
          (cffi:with-foreign-object (buffer 'win-char 256)
            (format t "GetWindowTextA (LPSTR): ")
            (with-timing (3) (dotimes (i 10000) (get-window-text-a hwnd buffer 256))))
          (cffi:with-foreign-object (buffer 'wchar 256)
            (format t "GetWindowTextW (LPWSTR): ")
            (with-timing (3) (dotimes (i 10000) (get-window-text-w hwnd buffer 256)))))))
  (terpri))

#+(or :win32 :windows)
(defun benchmark-memory-operations ()
  "Benchmark memory allocation with Windows types."
  (format t "~&--- Memory Operation Benchmarks ---~%")
  (format t "GlobalAlloc/GlobalFree (1KB blocks): ")
  (with-timing (3)
    (dotimes (i 5000)
      (let ((mem (global-alloc 0 1024)))
        (unless (cffi:null-pointer-p mem) (global-free mem)))))
  (format t "GlobalAlloc/GlobalFree (64KB blocks): ")
  (with-timing (3)
    (dotimes (i 1000)
      (let ((mem (global-alloc 0 65536)))
        (unless (cffi:null-pointer-p mem) (global-free mem)))))
  (terpri))

#+(or :win32 :windows)
(defun benchmark-type-conversions ()
  "Benchmark CFFI type conversion overhead."
  (format t "~&--- Type Conversion Benchmarks ---~%")
  (format t "DWORD (32-bit) conversions: ")
  (let ((val 0))
    (with-timing (3)
      (dotimes (i 1000000)
        (setf val (cffi:convert-to-foreign i 'dword))
        (cffi:convert-from-foreign val 'dword))))
  (format t "DWORD64 (64-bit) conversions: ")
  (let ((val 0))
    (with-timing (3)
      (dotimes (i 1000000)
        (setf val (cffi:convert-to-foreign i 'dword64))
        (cffi:convert-from-foreign val 'dword64))))
  (format t "WORD (16-bit) conversions: ")
  (let ((val 0))
    (with-timing (3)
      (dotimes (i 1000000)
        (setf val (cffi:convert-to-foreign (mod i 65536) 'word))
        (cffi:convert-from-foreign val 'word))))
  (terpri))

#+(or :win32 :windows)
(defun compare-call-overhead ()
  "Compare overhead of different function call types."
  (format t "~&--- Call Overhead Comparison ---~%")
  (format t "Native Lisp function call: ")
  (with-timing (3) (dotimes (i 1000000) (+ 1 1)))
  (format t "CFFI function call (no args): ")
  (with-timing (3) (dotimes (i 1000000) (get-tick-count)))
  (terpri))

#+(or :win32 :windows)
(defun memory-allocation-analysis ()
  "Analyze memory allocation patterns."
  (format t "~&--- Memory Allocation Analysis ---~%")
  (dolist (size '(64 256 1024 4096 16384 65536))
    (format t "~A byte allocations: " size)
    (with-timing (3)
      (dotimes (i 1000)
        (let ((mem (global-alloc 0 size)))
          (unless (cffi:null-pointer-p mem)
            (global-free mem))))))
  (terpri))

;; Basic
(defun time-cffi-call ()
  "The actual benchmark code that calls the foreign function."
  (with-timing (3) (dotimes (i 1000000) (get-tick-count))))