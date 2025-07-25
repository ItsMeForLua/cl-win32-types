;;;; cl-win32-types.lisp
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

(defpackage #:cl-win32-types
  (:use #:cl #:cffi)
  (:documentation "Provides a comprehensive set of CFFI type aliases for the Windows API.")
  (:export
   ;; Core Types
   #:wchar #:bool #:win-boolean #:win-char #:win-byte #:word #:dword #:dwordlong
   #:dword64 #:qword #:long #:ulong #:longlong #:int #:uint #:win-float
   #:int8 #:int16 #:int32 #:int64

   ;; Pointer & Handle Types
   #:handle #:pvoid #:win-atom #:hinstance #:hwnd #:hdc #:hicon #:hbrush #:hfont
   #:hmodule #:sc_handle #:hresult #:langid #:lcid #:colorref

   ;; Explicit Pointer Types (LP = Long Pointer)
   #:lpdword #:lpwstr #:lpcwstr #:lpstr #:lpcstr #:pwchar #:pwstr
   #:lphandle #:lpvoid

   ;; Platform-Dependent Pointer-Sized Types
   #:int_ptr #:uint_ptr #:dword_ptr #:ulong_ptr))

(in-package #:cl-win32-types)

;;;--------------------------------------------------------------------------
;;; Basic Integer and Character Types
;;;--------------------------------------------------------------------------

(defctype bool        :int32)
(defctype win-boolean :uint8)
(defctype win-byte    :uint8)
(defctype win-char    :int8)
(defctype wchar       :uint16)
(defctype word        :uint16)
(defctype dword       :uint32)
(defctype dword64     :uint64)
(defctype qword       :uint64)
(defctype dwordlong   :uint64)
(defctype int         :int32)
(defctype int8        :int8)
(defctype int16       :int16)
(defctype int32       :int32)
(defctype int64       :int64)
(defctype long        :int32)
(defctype longlong    :int64)
(defctype uint        :uint32)
(defctype ulong       :uint32)
(defctype win-float   :float)

;;;--------------------------------------------------------------------------
;;; Handle and Special-Purpose Types
;;;--------------------------------------------------------------------------

(defctype handle    :pointer)
(defctype pvoid     :pointer)
(defctype win-atom  :uint16)
(defctype hresult   :int32)
(defctype langid    :uint16)
(defctype lcid      :uint32)
(defctype colorref  :uint32)

;; Specific handle types, which are all aliases for HANDLE (:pointer)
(defctype hinstance handle)
(defctype hwnd      handle)
(defctype hdc       handle)
(defctype hicon     handle)
(defctype hbrush    handle)
(defctype hfont     handle)
(defctype hmodule   handle)
(defctype sc_handle handle)

;;;--------------------------------------------------------------------------
;;; Explicit Pointer Types (LP = Long Pointer)
;;;--------------------------------------------------------------------------

(defctype lpdword   :pointer) ; Pointer to a DWORD
(defctype lphandle  :pointer) ; Pointer to a HANDLE
(defctype lpvoid    :pointer) ; Pointer to void (generic pointer)

;; String pointer types
(defctype lpstr     :string)  ; Pointer to an ANSI string
(defctype lpcstr    :string)  ; Pointer to a constant ANSI string
(defctype lpwstr    :pointer) ; Pointer to a wide (Unicode) string. Use :pointer for manual management.
(defctype lpcwstr   :pointer) ; Pointer to a constant wide (Unicode) string. Use :pointer for manual management.
(defctype pwchar    :pointer) ; Pointer to a WCHAR
(defctype pwstr     :pointer) ; Pointer to a wide (Unicode) string

;;;--------------------------------------------------------------------------
;;; Platform-Dependent Pointer-Sized Types
;;;--------------------------------------------------------------------------

#+(and (or :win32 :windows) :64-bit)
(progn
  (defctype dword_ptr :uint64)
  (defctype int_ptr   :int64)
  (defctype uint_ptr  :uint64)
  (defctype ulong_ptr :uint64))

#+(and (or :win32 :windows) :32-bit)
(progn
  (defctype dword_ptr :uint32)
  (defctype int_ptr   :int32)
  (defctype uint_ptr  :uint32)
  (defctype ulong_ptr :uint32))

;; Fallback case to catch unsupported or misconfigured platforms at compile time.
#-(or (and (or :win32 :windows) :64-bit)
      (and (or :win32 :windows) :32-bit))
(error "cl-win32-types: Could not determine platform pointer size.
This library requires the Lisp implementation to have either :64-BIT or :32-BIT in *features* on Windows.")
