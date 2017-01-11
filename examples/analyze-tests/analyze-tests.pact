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

  (defschema account
    "Row type for accounts table."
     balance:integer
     data
     )
  (deftable accounts:{account}
    "Main table for accounts module.")

  (defun create-account (id:string initial-balance:integer)
    "Create a new account for ID with INITIAL-BALANCE funds"
    (enforce-keyset 'module-keyset)
    (enforce (> initial-balance 0) "Initial balance must be > 0")
    (insert accounts id { "balance": initial-balance })
  )

  (defun get-balance (id:string) (read accounts id 'balance))

  (defun pay-with-let (from:string to:string amount:integer)
    (with-read accounts from { "balance":= from-bal }
      (with-read accounts to { "balance":= to-bal }
        (enforce (>= from-bal amount) "Insufficient Funds")
        (let* ((new-from-bal (- from-bal amount))
               (new-to-bal (+ to-bal amount))
              )
          (update accounts from
                  { "balance": new-from-bal})
          (update accounts to
                  { "balance": new-to-bal })
         )
      )
    )
  )

  (defun pay (from:string to:string amount:integer)
    (with-read accounts from { "balance":= from-bal }
      (with-read accounts to { "balance":= to-bal }
        (enforce (>= from-bal amount) "Insufficient Funds")
        (update accounts from
                { "balance": (- from-bal amount) })
        (update accounts to
                { "balance": (+ to-bal amount) })
      )
    )
  )

  (defun pay-with-read (id:string)
    (with-read accounts id { "balance":= from-bal }
      (enforce (>= from-bal 0) "bal too low")
    )
  )

  (defun pay-update (id:string amount:integer)
    (update accounts id
            { "balance": amount })
  )
)
