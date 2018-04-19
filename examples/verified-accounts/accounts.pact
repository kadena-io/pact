;; accounts module, admin keyset, and table
; (load "examples/verified-accounts/accounts.repl")

(enforce-pact-version "2.3")

(define-keyset 'accounts-admin-keyset
  (read-keyset "accounts-admin-keyset"))

(module accounts 'accounts-admin-keyset
  "Accounts module demonstrating row-level keysets, private pacts, and escrow. \
\  Version: 0.2                                                                \
\  Author: Stuart Popejoy"

  (defschema account
    "Row type for accounts table."
     balance:decimal
     amount:decimal
     ccy:string
     auth:string     ;; AUTH_KEYSET for keysets, pact id for pacts
     data
     )

  (deftable accounts:{account}
    "Main table for accounts module.")

  (defconst AUTH_KEYSET 'K
    "Indicates keyset-governed account")

  (defconst PACT_REF "ref")

  (defun create-account (address ccy date)
    (insert accounts address
      { "balance": 0.0
      , "amount": 0.0
      , "ccy": ccy
      , "auth": AUTH_KEYSET
      , "date": date
      , "data": "Created account"
      }
    ))

  ; TODO: defproperty?
  ; (@property-of transfer conserves-mass)
  (defun transfer (src dest amount date)
    "transfer AMOUNT from SRC to DEST"
    (debit src amount date { "transfer-to": dest })
    (credit dest amount date { "transfer-from": src }))

  (defun read-account-user (id)
    "Read data for account ID"
    (with-read accounts id
              { "balance":= b
              , "ccy":= c
              , "auth" := auth }
      { "balance": b, "ccy": c }
      ))

  (@property-of read-account-admin
    (when
        (not (authorized-by 'accounts-admin-keyset))
        abort))
  (defun read-account-admin (id)
    "Read data for account ID, admin version"
    (enforce-keyset 'accounts-admin-keyset)
    (read accounts id ['balance 'ccy 'data 'date 'amount]))

  (defun check-balance (balance amount)
    (enforce (<= amount balance) "Insufficient funds"))

  (defun fund-account (address amount date)
    (enforce-keyset 'accounts-admin-keyset)
    (update accounts address
            { "balance": amount
            , "amount": amount
            , "date": date
            , "data": "Admin account funding" }
      ))

  (defun read-all ()
    (map (read-account-admin) (keys accounts)))

  ; (@property-of debit
  ;   (let
  ;     (with-read 'initial accounts acct
  ;       { "balance" := initial-balance
  ;       , "keyset" := ks
  ;       , "auth" := auth
  ;       })
  ;     (with-read 'final accounts acct
  ;       { "balance" := final-balance })

  ;     (if
  ;       (and
  ;         (>= initial-balance amount)
  ;         (= ks auth))
  ;       (= final-balance (- initial-balance amount))
  ;       (= final-balance initial-balance)))
  ;   )


  ; (@property
  ;   (let
  ;     (with-read 'initial accounts acct
  ;       { "balance" := initial-balance })
  ;     (with-read 'final accounts acct
  ;       { "balance" := final-balance })

  ;     (= final-balance (+ initial-balance amount))
  ;   ))

  ; ; alternately
  ; (@property
  ;   (with-read 'delta accounts acct
  ;     { "balance" := amount }))
  (defun debit (acct amount date data)
    "Debit AMOUNT from ACCT balance recording DATE and DATA"
    (with-read accounts acct
              { "balance":= balance
              , "auth" := auth
              }
      (check-balance balance amount)
      (update accounts acct
                { "balance": (- balance amount)
                , "amount": (- amount)
                , "date": date
                , "data": data
                }
          )))

 (defun credit (acct amount date data)
   "Credit AMOUNT to ACCT balance recording DATE and DATA"
   (with-read accounts acct
              { "balance":= balance }
     (update accounts acct
            { "balance": (+ balance amount)
            , "amount": amount
            , "date": date
            , "data": data
            }
      )))
)

(create-table accounts)
;done
