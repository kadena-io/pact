(define-keyset 'module-keyset (read-keyset "module-keyset"))

(module analyze-tests 'module-keyset
  (defun layup:bool (x: integer)
    (if (< x 10) true false)
  )
)
