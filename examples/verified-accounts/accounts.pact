;; accounts module, admin keyset, and table
; (load "examples/verified-accounts/accounts.repl")

(enforce-pact-version "2.4.1")

(define-keyset 'accounts-admin-keyset
  (read-keyset "accounts-admin-keyset"))

(module accounts 'accounts-admin-keyset
  @doc
    "Accounts module demonstrating row-level keysets, private pacts, and escrow. \
\    Version: 0.2                                                                \
\    Author: Stuart Popejoy"
  @model
    [(defproperty conserves-mass
       (= (column-delta accounts 'balance) 0.0))
     (defproperty auth-required
       (authorized-by 'accounts-admin-keyset))

     ; we have two admin functions
     (property auth-required
       {'only: [read-account-admin, fund-account]})

     ; every function should conserve mass except for the admin fund-account,
     ; and debit / credit which should be private
     (property conserves-mass
       {'except: [fund-account, debit, credit]})

     ; reading functions do not write
     (property (not (table-written accounts))
       {'only: [read-account-user, read-account-admin, check-balance]})

     (property (not (table-read accounts))
       {'only: [create-account]})
    ]

  (defschema account
    @doc   "Row type for accounts table."
    @model [(invariant (>= balance 0.0))]
    balance:decimal
    amount:decimal
    ccy:string
    auth:string     ;; AUTH_KEYSET for keysets, pact id for pacts
    )

  (deftable accounts:{account}
    "Main table for accounts module.")

  (defconst AUTH_KEYSET 'K
    "Indicates keyset-governed account")

  (defconst PACT_REF "ref")

  (defun create-account (address:string ccy)
    (insert accounts address
      { "balance": 0.0
      , "amount": 0.0
      , "ccy": ccy
      , "auth": AUTH_KEYSET
      }
    ))

  (defun transfer (src:string dest:string amount:decimal)
    "transfer AMOUNT from SRC to DEST"
    (debit src amount)
    (credit dest amount))

  (defun read-account-user (id)
    "Read data for account ID"
    (with-read accounts id
              { "balance":= b
              , "ccy":= c
              , "auth" := auth }
      ; TODO: we can't handle this object yet
      ; { "balance": b, "ccy": c }
      b
      ))

  (defun read-account-admin (id)
    "Read data for account ID, admin version"
    (enforce-keyset 'accounts-admin-keyset)
    (read accounts id ['balance 'ccy 'amount]))

  (defun check-balance (balance:decimal amount:decimal)
    (enforce (<= amount balance) "Insufficient funds"))

  (defun fund-account (address amount)
    (enforce-keyset 'accounts-admin-keyset)
    (enforce (>= amount 0.0) "amount must be non-negative")
    (update accounts address
            { "balance": amount
            , "amount": amount }
      ))

  (defun debit (acct amount)
    "Debit AMOUNT from ACCT balance"
    (with-read accounts acct
              { "balance":= balance
              , "auth" := auth
              }
      (check-balance balance amount)
      (update accounts acct
                { "balance": (- balance amount)
                , "amount": (- amount)
                }
          )))

 (defun credit (acct amount)
   "Credit AMOUNT to ACCT balance"
   (enforce (>= amount 0.0) "amount must be non-negative")
   (with-read accounts acct
              { "balance":= balance }
     (update accounts acct
            { "balance": (+ balance amount)
            , "amount": amount
            }
      )))
)

(create-table accounts)
;done
