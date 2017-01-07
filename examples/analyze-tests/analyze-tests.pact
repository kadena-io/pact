(define-keyset 'analyze-admin-keyset (read-keyset "analyze-admin-keyset"))

(module analyze-tests 'analyze-admin-keyset
  (defun gt-ten (a:integer)
    (if (> a 10) "more than ten" "less than ten")
  )
  (defun complicated-cond (a:integer b:string)
    (if (or (> a 10) (= b "foo")) "more than ten" "less than ten")
  )
  (defun simple-let (a:integer)
    (enforce (> a 0) "less than 0")
    (let* ((b (+ a 10)))
      (enforce (> a 10) "less than 10")
      (enforce (< b 20) "greater than 20")
      b
    )
  )
)