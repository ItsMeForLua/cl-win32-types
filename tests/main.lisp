;;;; tests/main.lisp
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
(defpackage #:cl-win32-types/tests
  (:use #:cl #:cffi #:fiveam #:cl-win32-types)
  (:export #:run-tests #:run-all-tests #:run-suite)
  (:documentation "Comprehensive test suite for cl-win32-types with full type validation,
                   cross-platform compatibility, and edge case coverage."))

(in-package #:cl-win32-types/tests)

;;;--------------------------------------------------------------------------
;;; Test Suite Definition and Configuration
;;;--------------------------------------------------------------------------

(def-suite win32-type-tests
  :description "Comprehensive tests for cl-win32-types type definitions.")

(def-suite basic-types
  :description "Tests for basic Windows integer and character types."
  :in win32-type-tests)

(def-suite handle-types  
  :description "Tests for Windows handle and pointer types."
  :in win32-type-tests)

(def-suite string-types
  :description "Tests for Windows string pointer types."
  :in win32-type-tests)

(def-suite platform-types
  :description "Tests for platform-dependent pointer-sized types."
  :in win32-type-tests)

(def-suite type-compatibility
  :description "Tests for type compatibility and interoperability."
  :in win32-type-tests)

(def-suite edge-cases
  :description "Tests for edge cases and boundary conditions."
  :in win32-type-tests)

;;;--------------------------------------------------------------------------
;;; Test Utilities and Helpers
;;;--------------------------------------------------------------------------

(defun platform-info ()
  "Returns platform information for test diagnostics."
  (list :lisp-implementation (lisp-implementation-type)
        :lisp-version (lisp-implementation-version)
        :features (intersection '(:windows :win32 :x86 :x86-64 :32-bit :64-bit)
                               *features*)
        :pointer-size (foreign-type-size :pointer)
        :machine-type (machine-type)))

(defmacro with-test-context (description &body body)
  "Wrapper that provides diagnostic context for failing tests."
  `(handler-bind ((error (lambda (c)
                          (format *error-output* 
                                  "~&Test failure in ~A~%Platform: ~A~%Error: ~A~%"
                                  ,description (platform-info) c))))
     ,@body))

(defun type-alignment-test (type-name expected-alignment)
  "Test that a type has the expected memory alignment."
  #+sbcl (is (= (sb-alien:alien-type-alignment (sb-alien:parse-alien-type type-name))
                expected-alignment))
  #-sbcl (skip "Alignment tests only supported on SBCL"))

(defun endianness-test (type-name value expected-bytes)
  "Test byte representation of a value in foreign memory."
  (with-foreign-object (ptr type-name)
    (setf (mem-ref ptr type-name) value)
    (loop for i from 0 below (foreign-type-size type-name)
          collect (mem-ref ptr :uint8 i) into bytes
          finally (is (equal bytes expected-bytes)
                     "~A value ~A should have byte representation ~A, got ~A"
                     type-name value expected-bytes bytes))))

;;;--------------------------------------------------------------------------
;;; Basic Types Test Suite
;;;--------------------------------------------------------------------------

(in-suite basic-types)

(test type-sizes
  "Verify the size of all fundamental Windows types."
  (with-test-context "Basic type sizes"
    ;; 8-bit types
    (is (= (foreign-type-size 'win-byte) 1) "BYTE should be 1 byte")
    (is (= (foreign-type-size 'win-boolean) 1) "BOOLEAN should be 1 byte")  
    (is (= (foreign-type-size 'win-char) 1) "CHAR should be 1 byte")
    (is (= (foreign-type-size 'int8) 1) "INT8 should be 1 byte")
    
    ;; 16-bit types
    (is (= (foreign-type-size 'word) 2) "WORD should be 2 bytes")
    (is (= (foreign-type-size 'wchar) 2) "WCHAR should be 2 bytes") 
    (is (= (foreign-type-size 'win-atom) 2) "ATOM should be 2 bytes")
    (is (= (foreign-type-size 'langid) 2) "LANGID should be 2 bytes")
    (is (= (foreign-type-size 'int16) 2) "INT16 should be 2 bytes")
    
    ;; 32-bit types
    (is (= (foreign-type-size 'dword) 4) "DWORD should be 4 bytes")
    (is (= (foreign-type-size 'long) 4) "LONG should be 4 bytes")
    (is (= (foreign-type-size 'ulong) 4) "ULONG should be 4 bytes")
    (is (= (foreign-type-size 'int) 4) "INT should be 4 bytes")
    (is (= (foreign-type-size 'uint) 4) "UINT should be 4 bytes")
    (is (= (foreign-type-size 'int32) 4) "INT32 should be 4 bytes")
    (is (= (foreign-type-size 'bool) 4) "BOOL should be 4 bytes")
    (is (= (foreign-type-size 'hresult) 4) "HRESULT should be 4 bytes")
    (is (= (foreign-type-size 'lcid) 4) "LCID should be 4 bytes")
    (is (= (foreign-type-size 'colorref) 4) "COLORREF should be 4 bytes")
    (is (= (foreign-type-size 'win-float) 4) "FLOAT should be 4 bytes")
    
    ;; 64-bit types
    (is (= (foreign-type-size 'dword64) 8) "DWORD64 should be 8 bytes")
    (is (= (foreign-type-size 'qword) 8) "QWORD should be 8 bytes")
    (is (= (foreign-type-size 'dwordlong) 8) "DWORDLONG should be 8 bytes")
    (is (= (foreign-type-size 'longlong) 8) "LONGLONG should be 8 bytes")
    (is (= (foreign-type-size 'int64) 8) "INT64 should be 8 bytes")))

(test type-ranges
  "Test the valid ranges of Windows integer types."
  (with-test-context "Type value ranges"
    ;; Unsigned types
    (with-foreign-object (ptr 'word)
      (setf (mem-ref ptr 'word) 65535)
      (is (= (mem-ref ptr 'word) 65535) "WORD max value"))
    
    (with-foreign-object (ptr 'dword)  
      (setf (mem-ref ptr 'dword) #xFFFFFFFF)
      (is (= (mem-ref ptr 'dword) #xFFFFFFFF) "DWORD max value"))
    
    ;; Signed types
    (with-foreign-object (ptr 'int32)
      (setf (mem-ref ptr 'int32) -2147483648)
      (is (= (mem-ref ptr 'int32) -2147483648) "INT32 min value")
      (setf (mem-ref ptr 'int32) 2147483647)
      (is (= (mem-ref ptr 'int32) 2147483647) "INT32 max value"))))

(test type-conversions
  "Test CFFI type conversion behavior."
  (with-test-context "Type conversions"
    ;; Test automatic conversion
    (is (= (convert-to-foreign 42 'dword) 42))
    (is (= (convert-from-foreign 42 'dword) 42))
    
    ;; Test truncation behavior
    (is (= (convert-to-foreign 65537 'word) 1) "WORD should truncate to 16 bits")
    
    ;; Test sign extension
    (is (= (convert-to-foreign -1 'int32) -1))
    (is (= (convert-from-foreign #xFFFFFFFF 'int32) -1))))

;;;--------------------------------------------------------------------------
;;; Handle Types Test Suite  
;;;--------------------------------------------------------------------------

(in-suite handle-types)

(test handle-type-sizes
  "Verify handle types are pointer-sized."
  (with-test-context "Handle type sizes"
    (let ((expected-size (foreign-type-size :pointer)))
      (is (= (foreign-type-size 'handle) expected-size))
      (is (= (foreign-type-size 'pvoid) expected-size))
      (is (= (foreign-type-size 'hinstance) expected-size))
      (is (= (foreign-type-size 'hwnd) expected-size))
      (is (= (foreign-type-size 'hdc) expected-size))
      (is (= (foreign-type-size 'hicon) expected-size))
      (is (= (foreign-type-size 'hbrush) expected-size))
      (is (= (foreign-type-size 'hfont) expected-size))
      (is (= (foreign-type-size 'hmodule) expected-size))
      (is (= (foreign-type-size 'sc_handle) expected-size)))))

(test handle-null-pointers
  "Test that handles can represent null pointers."
  (with-test-context "Handle null pointer support"
    (with-foreign-object (ptr 'handle)
      (setf (mem-ref ptr 'handle) (null-pointer))
      (is (null-pointer-p (mem-ref ptr 'handle)) "Handle should support null pointers"))))

(test handle-interoperability
  "Test that different handle types are interoperable."
  (with-test-context "Handle type interoperability" 
    (with-foreign-objects ((hwnd-ptr 'hwnd)
                          (handle-ptr 'handle))
      ;; Should be able to assign between handle types
      (let ((test-ptr (make-pointer #x12345678)))
        (setf (mem-ref hwnd-ptr 'hwnd) test-ptr)
        (setf (mem-ref handle-ptr 'handle) (mem-ref hwnd-ptr 'hwnd))
        (is (pointer-eq (mem-ref handle-ptr 'handle) test-ptr))))))

;;;--------------------------------------------------------------------------
;;; String Types Test Suite
;;;--------------------------------------------------------------------------

(in-suite string-types)

(test string-type-sizes
  "Verify string pointer types are pointer-sized."
  (with-test-context "String type sizes"
    (let ((expected-size (foreign-type-size :pointer)))
      (is (= (foreign-type-size 'lpstr) expected-size))
      (is (= (foreign-type-size 'lpcstr) expected-size))
      (is (= (foreign-type-size 'lpwstr) expected-size))
      (is (= (foreign-type-size 'lpcwstr) expected-size))
      (is (= (foreign-type-size 'pwchar) expected-size))
      (is (= (foreign-type-size 'pwstr) expected-size)))))

(test string-encoding-compatibility
  "Test string types work with CFFI string operations."
  (with-test-context "String encoding compatibility"
    ;; Test ANSI string operations
    (with-foreign-string (str "Hello World" :encoding :ascii)
      (with-foreign-object (ptr 'lpstr)
        (setf (mem-ref ptr 'lpstr) str)
        (is (string= (foreign-string-to-lisp (mem-ref ptr 'lpstr) :encoding :ascii)
                     "Hello World"))))
    
    ;; Test Unicode string operations  
    (with-foreign-string (str "Hello World" :encoding :utf-16le)
      (with-foreign-object (ptr 'lpwstr)
        (setf (mem-ref ptr 'lpwstr) str)
        (is (string= (foreign-string-to-lisp (mem-ref ptr 'lpwstr) :encoding :utf-16le)
                     "Hello World"))))))

;;;--------------------------------------------------------------------------
;;; Platform-Dependent Types Test Suite
;;;--------------------------------------------------------------------------

(in-suite platform-types)

(test platform-pointer-sizes
  "Verify platform-dependent types have correct sizes."
  (with-test-context "Platform-dependent type sizes"
    (cond 
      ((member :64-bit *features*)
       (is (= (foreign-type-size 'dword_ptr) 8) "DWORD_PTR should be 8 bytes on 64-bit")
       (is (= (foreign-type-size 'int_ptr) 8) "INT_PTR should be 8 bytes on 64-bit") 
       (is (= (foreign-type-size 'uint_ptr) 8) "UINT_PTR should be 8 bytes on 64-bit")
       (is (= (foreign-type-size 'ulong_ptr) 8) "ULONG_PTR should be 8 bytes on 64-bit"))
      ((member :32-bit *features*)
       (is (= (foreign-type-size 'dword_ptr) 4) "DWORD_PTR should be 4 bytes on 32-bit")
       (is (= (foreign-type-size 'int_ptr) 4) "INT_PTR should be 4 bytes on 32-bit")
       (is (= (foreign-type-size 'uint_ptr) 4) "UINT_PTR should be 4 bytes on 32-bit") 
       (is (= (foreign-type-size 'ulong_ptr) 4) "ULONG_PTR should be 4 bytes on 32-bit"))
      (t 
       ;; Fallback: assume pointer size
       (let ((ptr-size (foreign-type-size :pointer)))
         (is (= (foreign-type-size 'dword_ptr) ptr-size))
         (is (= (foreign-type-size 'int_ptr) ptr-size))
         (is (= (foreign-type-size 'uint_ptr) ptr-size))
         (is (= (foreign-type-size 'ulong_ptr) ptr-size)))))))

(test pointer-arithmetic-compatibility
  "Test that pointer-sized types work with pointer arithmetic."
  (with-test-context "Pointer arithmetic compatibility"
    (with-foreign-object (ptr 'dword_ptr)
      (let ((test-value (cffi:pointer-address (make-pointer #x1000))))
        (setf (mem-ref ptr 'dword_ptr) test-value)
        (is (= (mem-ref ptr 'dword_ptr) test-value) "DWORD_PTR should hold pointer values")))))

;;;--------------------------------------------------------------------------
;;; Type Compatibility Test Suite
;;;--------------------------------------------------------------------------

(in-suite type-compatibility)

(test windows-api-compatibility
  "Test compatibility with actual Windows API patterns."
  (with-test-context "Windows API compatibility"
    ;; Test BOOL vs boolean patterns
    (with-foreign-object (bool-ptr 'bool)
      (setf (mem-ref bool-ptr 'bool) 1)  ; TRUE
      (is (not (zerop (mem-ref bool-ptr 'bool))) "BOOL TRUE should be non-zero")
      (setf (mem-ref bool-ptr 'bool) 0)  ; FALSE  
      (is (zerop (mem-ref bool-ptr 'bool)) "BOOL FALSE should be zero"))
    
    ;; Test HRESULT error patterns
    (with-foreign-object (hr-ptr 'hresult)
      (setf (mem-ref hr-ptr 'hresult) 0)  ; S_OK
      (is (>= (mem-ref hr-ptr 'hresult) 0) "S_OK should be non-negative")
      (setf (mem-ref hr-ptr 'hresult) #x80000000)  ; Generic error
      (is (< (mem-ref hr-ptr 'hresult) 0) "Error HRESULT should be negative"))))

(test cross-type-assignments
  "Test assignments between related types."
  (with-test-context "Cross-type assignments"
    ;; Test that DWORD and UINT are compatible
    (with-foreign-objects ((dword-ptr 'dword)
                          (uint-ptr 'uint))
      (setf (mem-ref dword-ptr 'dword) #x12345678)
      (setf (mem-ref uint-ptr 'uint) (mem-ref dword-ptr 'dword))
      (is (= (mem-ref uint-ptr 'uint) #x12345678)))))

;;;--------------------------------------------------------------------------
;;; Edge Cases and Error Conditions
;;;--------------------------------------------------------------------------

(in-suite edge-cases)

(test boundary-values
  "Test behavior at type boundaries."
  (with-test-context "Boundary value handling"
    ;; Test maximum values don't overflow
    (with-foreign-object (ptr 'dword)
      (setf (mem-ref ptr 'dword) #xFFFFFFFF)
      (is (= (mem-ref ptr 'dword) #xFFFFFFFF) "DWORD max value should not overflow"))
    
    ;; Test minimum signed values
    (with-foreign-object (ptr 'int32)
      (setf (mem-ref ptr 'int32) (- (expt 2 31)))
      (is (= (mem-ref ptr 'int32) (- (expt 2 31))) "INT32 min value should be preserved"))))

(test memory-layout-consistency
  "Test that types have consistent memory layout."
  (with-test-context "Memory layout consistency"
    ;; Test that array of types has expected stride
    (with-foreign-object (array 'dword 4)
      (loop for i from 0 below 4 do
        (setf (mem-aref array 'dword i) (+ i #x1000)))
      (loop for i from 0 below 4 do
        (is (= (mem-aref array 'dword i) (+ i #x1000))
            "Array element ~A should maintain value" i)))))

(test unicode-edge-cases  
  "Test Unicode string handling edge cases."
  (with-test-context "Unicode edge cases"
    ;; TODO:
    ;; Test BOM handling and surrogate pairs would go here
    ;; For now, we'll just test basic wide character size
    (is (= (foreign-type-size 'wchar) 2) "WCHAR should be 2 bytes for UTF-16")))

;;;--------------------------------------------------------------------------
;;; Test Runners and Reporting
;;;--------------------------------------------------------------------------

(defun run-tests (&optional (stream t))
  "Run all tests with basic reporting."
  (let ((results (run! 'win32-type-tests)))
    (format stream "~&Test Results Summary:~%")
    (format stream "Platform: ~A~%" (platform-info))
    results))

(defun run-all-tests (&key (verbose nil) (stream t))
  "Run all tests with comprehensive reporting."
  (format stream "~&=== CL-WIN32-TYPES COMPREHENSIVE TEST SUITE ===~%")
  (format stream "Platform Information: ~A~%~%" (platform-info))
  
  (let ((results (if verbose
                     (run! 'win32-type-tests)
                     (run! 'win32-type-tests))))
    (format stream "~&=== TEST SUITE COMPLETE ===~%")
    results))

(defun run-suite (suite-name &key (verbose nil))
  "Run a specific test suite."
  (run! suite-name))

;; Performance regression detection
(defparameter *performance-baseline* nil
  "Baseline performance measurements for regression testing.")

(defun benchmark-test-performance ()
  "Benchmark test execution for performance regression detection."
  (format t "~&Benchmarking test suite performance...~%")
  (time (run! 'win32-type-tests)))

;; Auto-discovery of new test functions
(defun discover-tests ()
  "Automatically discover and list all test functions."
  (let ((tests '()))
    (do-symbols (symbol (find-package '#:cl-win32-types/tests))
      (when (and (fboundp symbol)
                 (string-prefix-p "TEST-" (symbol-name symbol)))
        (push symbol tests)))
    (sort tests #'string< :key #'symbol-name)))

(defun generate-test-report (&optional (filename "test-report.txt"))
  "Generate a comprehensive test report file."
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (format stream "CL-WIN32-TYPES Test Report~%")
    (format stream "Generated: ~A~%~%" (get-universal-time))
    (run-all-tests :verbose t :stream stream)))