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
     rowguard:guard
     date:time
     data:object
     )

  (deftable accounts:{account}
    "Main table for accounts module.")

  (defcap USER_GUARD (id)
    (with-read accounts id { "rowguard" := g }
               (enforce-guard g)))

  (defcap TRANSFER () true)

  (defcap ADMIN ()
    (enforce-keyset 'accounts-admin-keyset))

  (defun create-account (address guard ccy date)
    (with-capability (ADMIN)
      (insert accounts address
      { "balance": 0.0
      , "amount": 0.0
      , "ccy": ccy
      , "rowguard": guard
      , "date": date
      , "data": { "note": "Created account" }
      }
    )))

  (defun transfer (src dest amount date)
    "transfer AMOUNT from SRC to DEST"
    (with-capability (TRANSFER)
      (debit src amount date { "transfer-to": dest })
      (credit dest amount date { "transfer-from": src })))

  (defun read-account-user (id)
    "Read data for account ID"
    (with-capability (USER_GUARD id)
      (with-read accounts id
                { "balance":= b
                , "ccy":= c }
        { "balance": b, "ccy": c })))


  (defun read-account-admin (id)
    "Read data for account ID, admin version"
    (with-capability (ADMIN)
      (read accounts id ['balance 'ccy 'rowguard 'data 'date 'amount])))


  (defun account-keys ()
    "Get all account keys"
    (with-capability (ADMIN)
      (keys accounts)))

  (defun check-balance (balance amount)
    (enforce (<= amount balance) "Insufficient funds"))

  (defun fund-account (address amount date)
    (with-capability (ADMIN)
      (update accounts address
              { "balance": amount
              , "amount": amount
              , "date": date
              , "data": { "note": "Admin account funding" } }
      )))

  (defun read-all ()
    (with-capability (ADMIN)
      (map (read-account-admin) (keys accounts))))

  (defpact payment (payer payer-entity payee payee-entity amount date)
    "Debit PAYER at PAYER-ENTITY then credit PAYEE at PAYEE-ENTITY for AMOUNT on DATE"
    (step-with-rollback payer-entity
      (with-capability (TRANSFER)
        (debit payer amount date
          { "payee": payee
          , "payee-entity": payee-entity
          , "ref": (pact-id)
          }))
      (with-capability (TRANSFER)
        (credit payer amount date
          { "ref": (pact-id), "note": "rollback" })))

    (step payee-entity
      (with-capability (TRANSFER)
        (credit payee amount date
          { "payer": payer
          , "payer-entity": payer-entity
          , "ref": (pact-id)
          })))
  )


  (defun debit (acct amount date data)
    "Debit AMOUNT from ACCT balance recording DATE and DATA"
    (require-capability (TRANSFER))
    (with-capability (USER_GUARD acct)
      (with-read accounts acct
                { "balance":= balance
                }
        (check-balance balance amount)
        (update accounts acct
                { "balance": (- balance amount)
                , "amount": (- amount)
                , "date": date
                , "data": data
                }
          ))))

 (defun credit (acct amount date data)
   "Credit AMOUNT to ACCT balance recording DATE and DATA"
   (require-capability (TRANSFER))
     (with-read accounts acct
                { "balance":= balance }
       (update accounts acct
              { "balance": (+ balance amount)
              , "amount": amount
              , "date": date
              , "data": data
              }
      )))


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


  (defun init-escrow (deb-acct amount)
    ;; transfer will handle deb-keyset enforce
    (with-capability (USER_GUARD deb-acct)
      (with-read accounts deb-acct { 'ccy:= ccy }
        (let ((pact-acct (new-pact-account ESCROW_ACCT ccy)))
          (transfer deb-acct pact-acct amount (get-system-time))))))

  (defun cancel-escrow (timeout deb-acct cred-acct amount)
    (with-capability (CANCEL-ESCROW deb-acct cred-acct timeout)
      (transfer (get-pact-account ESCROW_ACCT) deb-acct amount (get-system-time))))


  (defun finish-escrow (deb-acct cred-acct
                        escrow-amount:decimal)
    (with-capability (USER_GUARD deb-acct)
      (with-capability (USER_GUARD cred-acct)
        (let* ((price (read-decimal "final-price"))
               (delta (- escrow-amount price))
               (escrow-acct (get-pact-account ESCROW_ACCT)))
          (enforce (>= escrow-amount price) "Price cannot negotiate up")
          (transfer escrow-acct cred-acct price (get-system-time))
          (if (> delta 0.0)
            (transfer escrow-acct deb-acct delta (get-system-time))
            "noop")
          (format "Escrow completed with {} paid and {} refunded" [price delta])
          ))))


  (defcap CANCEL-ESCROW (deb-acct cred-acct timeout)
    "Capability to cancel an escrow between DEB-ACCT and CRED-ACCT within TIMEOUT"
    (let ((systime (get-system-time)))
      (enforce-one
        "Cancel can only be effected by creditor, or debitor after timeout"
        [(compose-capability (USER_GUARD cred-acct))
         (and (compose-capability (USER_GUARD deb-acct))
              (enforce (>= systime timeout) "Cancel timeout not expired for debitor cancel"))
        ])))


  (defun get-pact-account (pfx:string) (format "{}-{}" [pfx (pact-id)]))

  (defun new-pact-account (pfx ccy)
    (let ((a (get-pact-account pfx)))
      (insert accounts a
        { "balance": 0.0
        , "amount": 0.0
        , "ccy": ccy
        , "rowguard": (create-pact-guard pfx)
        , "date": (get-system-time)
        , "data": { "note": "Created pact account" }
        }
      )
      a))

)

(create-table accounts)
;done
