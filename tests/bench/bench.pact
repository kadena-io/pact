;;benchmarking module

(define-keyset 'bench-admin (read-keyset "keyset"))

(module bench 'bench-admin

  (defschema account-schema
    balance:decimal
    guard:guard
    )
  (deftable bench-accounts:{account-schema})

 (defun keys-all-1 (count matched) (= count matched))

 (defun create-account (address:string guard:guard)
   ; write instead of insert here to make mocking easier
   (write bench-accounts address
         { "balance": 0.0, "guard": guard }))

  (defcap TRANSFER (src dest amount)
    (with-read bench-accounts src {"guard":=g}
      (enforce-guard g)
    )
  )

  (defcap MTRANSFER (src dest amount)
    @managed MTRANSFER-mgr
    (with-read bench-accounts src {"guard":=g}
      (enforce-guard g)
    )
  )

  (defun MTRANSFER-mgr (m r)
    (enforce (= (at 'src m) (at 'src r)) "m")
    (enforce (= (at 'dest m) (at 'dest r)) "r")
    (let ((bal (at 'amount m))
          (amt (at 'amount r)))
      (enforce (>= bal amt) "Exhausted")
      (+ { 'amount: (- bal amt)} m))
  )

  (defun transfer (src dest amount)
    "transfer AMOUNT from SRC to DEST"
    (with-capability (TRANSFER src dest amount)
      (with-read bench-accounts src { "balance":= src-balance }
        (check-balance src-balance amount)
          (with-read bench-accounts dest { "balance":= dest-balance }
            (update bench-accounts src
              { "balance": (- src-balance amount)})
            (update bench-accounts dest
              { "balance": (+ dest-balance amount)}))))
  )
  (defun mtransfer (src dest amount)
    "managed transfer AMOUNT from SRC to DEST"
    (with-capability (MTRANSFER src dest amount)
      (with-read bench-accounts src { "balance":= src-balance }
        (check-balance src-balance amount)
          (with-read bench-accounts dest { "balance":= dest-balance }
            (update bench-accounts src
              { "balance": (- src-balance amount)})
            (update bench-accounts dest
              { "balance": (+ dest-balance amount)}))))
  )
 (defun read-account (id)
   "Read data for account ID"
   (read bench-accounts id 'balance 'amount 'data))

 (defun check-balance (balance amount)
   (enforce (<= amount balance) "Insufficient funds"))

 (defun fund-account (address amount)
   (update bench-accounts address
           { "balance": amount }))

 (defun read-all ()
   { "Acct1": (read-account "Acct1")
   , "Acct2": (read-account "Acct2")})

  (defun bench () (transfer "Acct1" "Acct2" 1.0))
  (defun mbench () (mtransfer "Acct1" "Acct2" 1.0))


 (defun upd (a) (update bench-accounts "Acct1"
            { "balance": 1000.0, "amount": 1000.0
            , "data": { "transfer-from": "src" } }) a)


 (defun add (a b)
   (+ a b))

 (defun mul (a b)
   (* a b))

 (defun wrap10 (a) (id (id (id (id (id (id (id (id (id (id a)))))))))))

 (defun rep10 (a) (id a) (id a) (id a) (id a) (id a) (id a) (id a) (id a) (id a) (id a))

 (defun withr () (with-read bench-accounts "Acct1" { "balance":= b } b))

 (defun fst (a b) a)
 (defun snd (a b) b)
 (defun id (a) a)

)

(create-table bench-accounts)

(create-account "Acct1" (read-keyset 'acct))
(fund-account "Acct1" 10000000.0)
(create-account "Acct2" (read-keyset 'acct))
