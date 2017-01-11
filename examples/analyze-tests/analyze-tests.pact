(define-keyset 'module-keyset (read-keyset "module-keyset"))

(module analyze-tests 'module-keyset

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

  (defun create-account (id:string initial-balance:integer)
    "Create a new account for ID with INITIAL-BALANCE funds"
    (enforce-keyset 'module-keyset)
    (enforce (> initial-balance 0) "Initial balance must be > 0")
    (insert 'payments-table id { "balance": initial-balance })
  )

  (defun get-balance (id:string) (read 'payments-table id 'balance))

  (defun pay-with-let (from:string to:string amount:integer)
    (with-read 'payments-table from { "balance":= from-bal }
      (with-read 'payments-table to { "balance":= to-bal }
        (enforce (>= from-bal amount) "Insufficient Funds")
        (let* ((new-from-bal (- from-bal amount))
               (new-to-bal (+ to-bal amount))
              )
          (update 'payments-table from
                  { "balance": new-from-bal})
          (update 'payments-table to
                  { "balance": new-to-bal })
         )
      )
    )
  )

  (defun pay (from:string to:string amount:integer)
    (with-read 'payments-table from { "balance":= from-bal }
      (with-read 'payments-table to { "balance":= to-bal }
        (enforce (>= from-bal amount) "Insufficient Funds")
        (update 'payments-table from
                { "balance": (- from-bal amount) })
        (update 'payments-table to
                { "balance": (+ to-bal amount) })
      )
    )
  )

  (defun pay-with-read (id:string)
    (with-read 'payments-table id { "balance":= from-bal }
      (enforce (>= from-bal 0) "bal too low")
    )
  )

  (defun pay-update (id:string amount:integer)
    (update 'payments-table id
            { "balance": amount })
  )
)
