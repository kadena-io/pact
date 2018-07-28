(module cash 'module-admin

  (defschema entry
    ccy:string
    balance:decimal
    change:decimal
    date:time
    keyset:keyset)

  (deftable cash:{entry})

  (defun debit (id amount date)
    "Debit ID for AMOUNT, checking balance for available funds"
    (with-read cash id { "balance":= balance, "keyset" := ks}
      (enforce-keyset ks)
      (enforce (>= balance amount) "Insufficient funds")
      (update cash id {
        "balance": (- balance amount),
        "change": (- amount),
        "date": date
        }))
  )

  (defun credit (id amount date)
    "Credit ID with AMOUNT"
    (with-read cash id { "balance" := balance}
      (update cash id {
        "balance": (+ balance amount),
        "change": amount,
        "date": date
        })
    )
  )

  (defun make-payment (payor payee amount date)
    "Debit PAYOR and credit PAYEE AMOUNT"
    (debit payor amount date)
    (credit payee amount date))

  (defun create-account (id ccy amount date ks)
    "Create account ID for CCY and fund with AMOUNT"
    (enforce-keyset 'operate-admin)
    (insert cash id {
      "ccy": ccy,
      "balance": amount,
      "change": amount,
      "date": date,
      "keyset": ks })
  )

  (defun read-account (id) (read cash id))

)


(create-table cash)
