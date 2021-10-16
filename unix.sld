(define-library (distill unix)
  (export
    adduser
    addgroup
    groups+users->artifacts

    make-user
    user-name
    user-uid
    user-gid
    user-home
    user-login
    make-group
    group-name
    group-gid
    group-users
    user?
    group?)
  (import
    scheme
    (srfi 26)
    (srfi 69)
    (distill text)
    (distill plan)
    (distill kvector)
    (distill contract))
  (cond-expand
    (chicken (import
               (chicken type)
               (only (chicken base) include error fixnum?))))
  (include "unix.scm"))
