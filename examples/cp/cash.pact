(module cash 'cp-module-admin

  (defschema entry
    ccy:string
    balance:decimal
    change:decimal
    date:time)

  (deftable cash:{entry})

  (defun debit (id:string amount:decimal date:time)
    "Debit ID for AMOUNT, checking balance for available funds"
    (with-read cash id { "balance":= balance }
      (enforce (>= balance amount) "Insufficient funds")
      (update cash id {
        "balance": (- balance amount),
        "change": (- amount),
        "date": date
        }))
  )

  (defun credit (id:string amount:decimal date:time)
    "Credit ID with AMOUNT"
    (with-read cash id { "balance" := balance}
      (update cash id {
        "balance": (+ balance amount),
        "change": amount,
        "date": date
        })
    )
  )

  (defun make-payment (payor:string payee:string amount:decimal date:time)
    "Debit PAYOR and credit PAYEE AMOUNT"
    (debit payor amount date)
    (credit payee amount date))

  (defun create-account (id:string ccy:string amount:decimal date:time)
    "Create account ID for CCY and fund with AMOUNT"
    (insert cash id {
      "ccy": ccy,
      "balance": amount,
      "change": amount,
      "date": date })
  )

  (defun read-account (id:string) (read cash id))

)


(create-table cash)
