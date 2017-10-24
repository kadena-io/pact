;; accounts module, admin keyset, and table

(define-keyset 'accounts-admin-keyset
  (read-keyset "accounts-admin-keyset"))

(module accounts 'accounts-admin-keyset
  "Simple account functionality. \
\ Tables used: 'accounts        \
\ Version: 0.1                  \
\ Author: Stuart Popejoy"

  (use system)

  (defschema account
    "Row type for accounts table."
     balance:decimal
     amount:decimal
     ccy:string
     keyset:keyset
     date:time
     data
     )

  (deftable accounts:{account}
    "Main table for accounts module.")

  (defun create-account (address keyset ccy date)
    (insert accounts address
      { "balance": 0.0
      , "amount": 0.0
      , "ccy": ccy
      , "keyset": keyset
      , "date": date
      , "data": "Created account"
      }
    ))

  (defun transfer (src dest amount date)
    "transfer AMOUNT from SRC to DEST"
    ;read balance and row-level keyset from src
    (with-read accounts src { "balance":= src-balance
                            , "keyset" := src-ks }
      (check-balance src-balance amount)
      (enforce-keyset src-ks)
      (with-read accounts dest
                 { "balance":= dest-balance }
        (update accounts src
                { "balance": (- src-balance amount)
                , "amount": (- amount)
                , "date": date
                , "data": { "transfer-to": dest }
                }
        )
        (update accounts dest
                { "balance": (+ dest-balance amount)
                , "amount": amount
                , "date": date
                , "data": { "transfer-from": src }
                }
        ))))

  (defun read-account-user (id)
    "Read data for account ID"
    (with-read accounts id
              { "balance":= b
              , "ccy":= c
              , "keyset" := ks }
      (enforce-keyset ks)
      { "balance": b, "ccy": c }
      ))

  (defun read-account-admin (id)
    "Read data for account ID, admin version"
    (enforce-keyset 'accounts-admin-keyset)
    (read accounts id ['balance 'ccy 'keyset 'data 'date 'amount]))


  (defun account-keys ()
    "Get all account keys"
    (enforce-keyset 'accounts-admin-keyset)
    (keys accounts))

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

  (defpact payment (payer payer-entity payee payee-entity amount date)
    "Debit PAYER at PAYER-ENTITY then credit PAYEE at PAYEE-ENTITY for AMOUNT on DATE"
    (step-with-rollback payer-entity
      (debit payer amount date
            { "payee": payee
            , "payee-entity": payee-entity
            , PACT_REF: (pact-txid)
            })
      (credit payer amount date
           { PACT_REF: (pact-txid), "note": "rollback" }))

    (step payee-entity
      (credit payee amount date
            { "payer": payer
            , "payer-entity": payer-entity
            , PACT_REF: (pact-txid)
            }
      )))

  (defun debit (acct amount date data)
    "Debit AMOUNT from ACCT balance recording DATE and DATA"
    (with-read accounts acct
              { "balance":= balance
              , "keyset" := ks
              }
      (check-balance balance amount)
      (enforce-keyset ks)
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

  (defconst PACT_REF "ref")





  (defconst ESCROW_ACCT "escrow-account")

  (defpact two-party-escrow (deb-acct deb-keyset:string
                             cred-acct cred-keyset:string
                             escrow-amount:decimal timeout)
    "Simple two-party escrow pact"
    (step-with-rollback
      (init-escrow deb-acct escrow-amount)
      (cancel-escrow timeout deb-acct deb-keyset cred-keyset escrow-amount))
    (step
      (finish-escrow deb-acct deb-keyset cred-acct
                         cred-keyset escrow-amount)))

  (defun init-escrow (deb-acct amount)
    ;; transfer will handle deb-keyset enforce
    (transfer deb-acct (new-pact-account ESCROW_ACCT) amount (get-system-time)))

  (defun cancel-escrow (timeout deb-acct deb-keyset:string cred-keyset:string amount)
    (enforce-one "Cancel can only be effected by creditor, or debitor after timeout"
                 [(enforce-keyset cred-keyset)
                  (and (enforce (> (get-system-time) timeout) "Timeout expired")
                       (enforce-keyset deb-keyset))])
    (transfer (get-pact-account ESCROW_ACCT) deb-acct amount (get-system-time)))


  (defun finish-escrow (deb-acct deb-keyset:string
                        cred-acct cred-keyset:string
                        escrow-amount:decimal)
    (enforce-keyset deb-keyset)
    (enforce-keyset cred-keyset)
    (let* ((price (read-decimal "agreed-upon-price"))
           (delta (- escrow-amount price))
           (escrow-acct (get-pact-account ESCROW_ACCT)))
      (enforce (>= escrow-amount price) "Price cannot negotiate up")
      (transfer escrow-acct cred-acct price (get-system-time))
      (if (> delta 0)
        (transfer escrow-acct deb-acct delta))))





)

(create-table accounts)
;done
