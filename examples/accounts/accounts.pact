;; accounts module, admin keyset, and table

(enforce-pact-version "2.3")

(define-keyset 'accounts-admin-keyset
  (read-keyset "accounts-admin-keyset"))

(module accounts 'accounts-admin-keyset
  "Accounts module demonstrating row-level keysets, private pacts, and escrow. \
\  Version: 0.2                                                                \
\  Author: Stuart Popejoy"

  ;; mock preview of system library with get-system-time
  (use system)

  (defschema account
    "Row type for accounts table."
     balance:decimal
     amount:decimal
     ccy:string
     keyset:keyset
     auth:string     ;; AUTH_KEYSET for keysets, pact id for pacts
     date:time
     data
     )

  (deftable accounts:{account}
    "Main table for accounts module.")

  (defconst AUTH_KEYSET 'K
    "Indicates keyset-governed account")

  (defconst ADMIN_KEYSET (read-keyset "accounts-admin-keyset"))


  (defun create-account (address keyset ccy date)
    (insert accounts address
      { "balance": 0.0
      , "amount": 0.0
      , "ccy": ccy
      , "keyset": keyset
      , "auth": AUTH_KEYSET
      , "date": date
      , "data": "Created account"
      }
    ))

  (defun transfer (src dest amount date)
    "transfer AMOUNT from SRC to DEST"
    (debit src amount date { "transfer-to": dest })
    (credit dest amount date { "transfer-from": src }))

  (defun read-account-user (id)
    "Read data for account ID"
    (with-read accounts id
              { "balance":= b
              , "ccy":= c
              , "keyset" := ks
              , "auth" := auth }
      (enforce-auth ks auth)
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
            , PACT_REF: (pact-id)
            })
      (credit payer amount date
           { PACT_REF: (pact-id), "note": "rollback" }))

    (step payee-entity
      (credit payee amount date
            { "payer": payer
            , "payer-entity": payer-entity
            , PACT_REF: (pact-id)
            }
      )))


  (defun enforce-auth (keyset:keyset auth)
    (if (= auth AUTH_KEYSET)
      (enforce-keyset keyset)
      (enforce (= auth (format "%s" [(pact-id)]))
        "Invalid access of pact account")))


  (defun debit (acct amount date data)
    "Debit AMOUNT from ACCT balance recording DATE and DATA"
    (with-read accounts acct
              { "balance":= balance
              , "keyset" := ks
              , "auth" := auth
              }
      (check-balance balance amount)
      (enforce-auth ks auth)
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

  (defpact two-party-escrow (deb-acct cred-acct
                             escrow-amount:decimal timeout)
    "Simple two-party escrow pact"
    (step-with-rollback
      (init-escrow deb-acct escrow-amount)
      (cancel-escrow timeout deb-acct cred-acct escrow-amount))
    (step
      (finish-escrow deb-acct cred-acct
                     escrow-amount)))

  (defun get-acct-keyset (acct)
    ( with-read accounts acct { 'keyset := k } k))

  (defun enforce-acct-keyset (acct)
    (let ((k (get-acct-keyset acct)))
      (enforce-keyset k)))

  (defun init-escrow (deb-acct amount)
    ;; transfer will handle deb-keyset enforce
    (with-read accounts deb-acct { 'ccy:= ccy, 'keyset:= k }
      (let ((pact-acct (new-pact-account ESCROW_ACCT ccy k)))
        (transfer deb-acct pact-acct amount (get-system-time)))))

  (defun cancel-escrow (timeout deb-acct cred-acct amount)
    (let ((dk (get-acct-keyset deb-acct))
          (ck (get-acct-keyset cred-acct))
          (systime (get-system-time)))
      (enforce-one
        "Cancel can only be effected by creditor, or debitor after timeout"
        [(enforce-keyset ck)
         (and (enforce (>= systime timeout) "Timeout expired")
              (enforce-keyset dk))])
      (transfer (get-pact-account ESCROW_ACCT) deb-acct amount (get-system-time))))


  (defun finish-escrow (deb-acct cred-acct
                        escrow-amount:decimal)
    (enforce-acct-keyset deb-acct)
    (enforce-acct-keyset cred-acct)
    (let* ((price (read-decimal "final-price"))
           (delta (- escrow-amount price))
           (escrow-acct (get-pact-account ESCROW_ACCT)))
      (enforce (>= escrow-amount price) "Price cannot negotiate up")
      (transfer escrow-acct cred-acct price (get-system-time))
      (if (> delta 0.0)
        (transfer escrow-acct deb-acct delta (get-system-time))
        "noop")
      (format "Escrow completed with {} paid and {} refunded" [price delta])))


  (defun get-pact-account (pfx:string) (format "{}-{}" [pfx (pact-id)]))

  (defun new-pact-account (pfx ccy)
    (let ((a (get-pact-account pfx)))
      (insert accounts a
        { "balance": 0.0
        , "amount": 0.0
        , "ccy": ccy
        , "keyset": ADMIN_KEYSET
        , "auth": (format "%s" [(pact-id)])
        , "date": (get-system-time)
        , "data": "Created pact account"
        }
      )
      a))

)

(create-table accounts)
;done
