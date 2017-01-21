(define-keyset 'module-keyset (read-keyset "module-keyset"))

(module analyze-tests 'module-keyset
  (defschema account
    "Row type for accounts table."
     balance:integer
     data
     )
  (deftable accounts:{account}
    "Main table for accounts module.")

  (defun create-account (id initial-balance)
    "Create a new account for ID with INITIAL-BALANCE funds"
    (enforce-keyset 'module-keyset)
    (enforce (>= initial-balance 0) "Initial balance must be > 0")
    (insert accounts id { "balance": initial-balance })
  )

  (defun get-balance (id) (read accounts id 'balance))

  (defun pay (from to amount)
    "Transfer money between accounts \
    \{-# PROVE 'analyze-tests.accounts.balance' [ConservesMass, Column >= 0] #-}"
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
)
