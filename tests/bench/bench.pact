;;benchmarking module

(define-keyset 'bench-admin (read-keyset "keyset"))

(module bench 'bench-admin

  (defschema accounts balance:decimal amount:decimal data)
  (deftable bench-accounts:{accounts})

 (defun keys-all (count matched) (= count matched))

 (defun create-account (address)
   ; write instead of insert here to make mocking easier
   (write bench-accounts address
         { "balance": 0.0, "amount": 0.0, "data": "Created account" }))

 (defun transfer (src dest amount)
   "transfer AMOUNT from SRC to DEST"
  (with-read bench-accounts src { "balance":= src-balance }
   (check-balance src-balance amount)
    (with-read bench-accounts dest { "balance":= dest-balance }
     (update bench-accounts src
            { "balance": (- src-balance amount), "amount": (- amount)
            , "data": { "transfer-to": dest } })
     (update bench-accounts dest
            { "balance": (+ dest-balance amount), "amount": amount
            , "data": { "transfer-from": src } }))))

 (defun read-account (id)
   "Read data for account ID"
   (read bench-accounts id 'balance 'amount 'data))

 (defun check-balance (balance amount)
   (enforce (<= amount balance) "Insufficient funds"))

 (defun fund-account (address amount)
   (update bench-accounts address
           { "balance": amount, "amount": amount
           , "data": "Admin account funding" }))

 (defun read-all ()
   { "Acct1": (read-account "Acct1")
   , "Acct2": (read-account "Acct2")})

 (defun bench () (transfer "Acct1" "Acct2" 1.0))

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

 (defun mk-test-acct (i unused)
   (let ((acct (format "test%d" [i])))
     (create-account acct)
     (fund-account acct (+ 1000.0 i))
     (+ 1 i)))

 (defun testquery ()
   (length (select bench-accounts (where 'balance (< 99.0)))))

)

(create-table bench-accounts)

(create-account "Acct1")
(fund-account "Acct1" 10000000.0)
(create-account "Acct2")

(fold (mk-test-acct) 0 (make-list 20000 1))
