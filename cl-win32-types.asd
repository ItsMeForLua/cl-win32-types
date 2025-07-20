;;;; cl-win32-types.asd
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

(asdf:defsystem #:cl-win32-types
  :description "A comprehensive collection of CFFI type aliases for the Windows API."
  :author "Andrew D. France andrewforlua@gmail.com"
  :license "MIT"
  :version "1.0.0"
  :depends-on (#:cffi)
  :serial t
  :components ((:file "cl-win32-types")))

(asdf:defsystem #:cl-win32-types/tests
  :description "Test suite for cl-win32-types."
  :author "Andrew D. France andrewforlua@gmail.com"
  :license "MIT"
  :depends-on (#:cl-win32-types #:fiveam)
  :components ((:module "tests"
                :serial t
                :components ((:file "main"))))
  :perform (asdf:test-op (op c) (uiop:symbol-call '#:cl-win32-types/tests '#:run-tests)))

;; System for running benchmarks.
;; You can run it with (asdf:load-system :cl-win32-types/benchmarks)
;; followed by (cl-win32-types/benchmarks:run-benchmarks)
(asdf:defsystem #:cl-win32-types/benchmarks
  :description "Benchmarks for cl-win32-types."
  :author "Andrew D. France andrewforlua@gmail.com"
  :license "MIT"
  :depends-on (#:cl-win32-types #:cffi #:trivial-benchmark)
  :components ((:module "benchmarks"
                :serial t
                :components ((:file "main")))))
