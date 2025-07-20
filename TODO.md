# **cl-win32-types Roadmap**

This document outlines the development roadmap for the cl-win32-types library, focusing on tasks that enhance its core mission of providing an exhaustive and accurate CFFI type definitions for the Windows API.

## **Project Mission**

cl-win32-types is a foundational library that provides accurate CFFI type aliases for Windows API development. It does one thing well: giving Lisp developers the correct type definitions, structures, and constants they need to interface with Windows APIs.

### **Priority 1: Core Type Coverage**

- \[ \] **Additional Integer Types**: Add any missing fundamental Windows integer types.
  - \[ \] USHORT
  - \[ \] SHORT
  - \[ \] UCHAR
  - \[ \] SCHAR
- \[ \] **Additional Handle Types**: Add specialized handles for common Windows subsystems.
  - \[ \] HKEY (registry key handle)
  - \[ \] HFILE (file handle)
  - \[ \] HMENU (menu handle)
  - \[ \] HCURSOR (cursor handle)
  - \[ \] HPEN (pen handle)
- \[ \] **Callback Types**: Define function pointer types for common callbacks.
  - \[ \] WNDPROC (window procedure)
  - \[ \] DLGPROC (dialog procedure)
  - \[ \] HOOKPROC (hook procedure)
- \[ \] **Platform Completeness**:
  - \[ \] Add ARM64 Windows platform detection to the pointer-size logic.
  - \[ \] Verify type sizes against the Windows SDK on an ARM64 machine.

### **Priority 2: Common Structures and Unions**

- \[ \] **Basic Geometry**: Add common coordinate and size structures.
  - \[ \] POINT
  - \[ \] SIZE
  - \[ \] RECT
- \[ \] **System Information**: Add basic system data structures.
  - \[ \] SYSTEMTIME
  - \[ \] FILETIME
  - \[ \] LARGE\_INTEGER (union)
  - \[ \] ULARGE\_INTEGER (union)
- \[ \] **Common Message Structures**:
  - \[ \] MSG (window message)
- \[ \] **Security**: Add basic security structures.
  - \[ \] SECURITY\_ATTRIBUTES

### **Priority 3: Common Constants**

- \[ \] **Common Return Values**: Define standard Windows return codes.
  - \[ \] ERROR\_SUCCESS
  - \[ \] INVALID\_HANDLE\_VALUE
  - \[ \] S\_OK, E\_FAIL
- \[ \] **File Attributes**: Define file system constants.
  - \[ \] FILE\_ATTRIBUTE\_\* constants
  - \[ \] GENERIC\_READ, GENERIC\_WRITE access rights
- \[ \] **Registry Constants**: Define registry access and key constants.
  - \[ \] HKEY\_\* predefined keys
  - \[ \] KEY\_\* access rights
- \[ \] **Resource Limits**: Define common Windows buffer size constants.
  - \[ \] MAX\_PATH
  - \[ \] MAX\_COMPUTERNAME\_LENGTH

### **Priority 4: Quality and Usability**

- \[ \] **Documentation Improvements**:
  - \[ \] Add practical examples to README.md for structure access patterns.
  - \[ \] Create a type reference table in the README.md listing all types and their C equivalents.
- \[ \] **Testing Enhancements**:
  - \[ \] Add tests to verify structure layouts and sizes.
  - \[ \] Add tests to verify constant values match Windows headers.
- \[ \] **Code Quality**:
  - \[ \] Ensure consistent code formatting (e.g., using a tool like trivial-formatter).
  - \[ \] Review and improve inline comments for clarity.

### **Priority 5: Distribution and Accessibility**

- \[ \] **Quicklisp Submission**: Submit the library to the Quicklisp distribution.
- \[ \] **Lisp Implementation Testing**: Verify the library works across major Lisp implementations.
  - \[ \] CCL
  - \[ \] LispWorks
  - \[ \] Allegro CL

### **Priority 6: Community and Maintenance**

- \[ \] **Issue Templates**: Create GitHub issue templates for bug reports and feature (e.g., missing type) requests.
- \[ \] **Contributing Guide**: Create a CONTRIBUTING.md file documenting how to add new types and run tests.
- \[ \] **Versioning Strategy**: Decide on a clear versioning strategy (e.g., Semantic Versioning) for future releases.
