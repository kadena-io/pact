(define-keyset 'analyze-admin-keyset (read-keyset "analyze-admin-keyset"))

(module analyze-tests 'analyze-admin-keyset
  (defun gt-ten (a:integer)
    (if (> a 10) "more than ten" "less than ten")
  )
  (defun complicated-cond (a:integer b:string)
    (if (or (> a 10) (= b "foo")) "more than ten" "less than ten")
  )
)