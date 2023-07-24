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
    @managed amount MTRANSFER-mgr
    (with-read bench-accounts src {"guard":=g}
      (enforce-guard g)
    )
  )

  (defun MTRANSFER-mgr (m r)
      (enforce (>= m r) "Exhausted")
      (- m r)
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

 (defun wrap10_integer:integer (a:integer) (id_integer (id_integer (id_integer (id_integer (id_integer (id_integer (id_integer (id_integer (id_integer (id_integer a)))))))))))

 (defun rep10 (a) (id a) (id a) (id a) (id a) (id a) (id a) (id a) (id a) (id a) (id a))

 (defun withr () (with-read bench-accounts "Acct1" { "balance":= b } b))

 (defun accum:integer (xs) (fold (+) 0 xs))

 (defun fst (a b) a)
 (defun snd (a b) b)
 (defun id (a) a)
 (defun id_integer:integer (a:integer) a)

 (defun arity_tc_0:integer
    () 1)

 (defun arity_tc_1:integer
    (a:integer) 1)

 (defun arity_tc_10:integer
    (a:integer b:integer c:integer d:integer e:integer f:integer g:integer h:integer i:integer j:integer) 1)

 (defun arity_tc_40:integer
    (a1:integer b1:integer c1:integer d1:integer e1:integer f1:integer g1:integer h1:integer i1:integer j1:integer a2:integer b2:integer c2:integer d2:integer e2:integer f2:integer g2:integer h2:integer i2:integer j2:integer a3:integer b3:integer c3:integer d3:integer e3:integer f3:integer g3:integer h3:integer i3:integer j3:integer a4:integer b4:integer c4:integer d4:integer e4:integer f4:integer g4:integer h4:integer i4:integer j4:integer) 1)


 (defschema small_object
     a:integer)
 (defun arity_small_obj:integer (arg:object{small_object}) 1)

 (defschema medium_object
     a:integer b:bool c:integer d:object{small_object}
     e:integer f:bool g:integer h:object{small_object}
   )
 (defun arity_medium_obj:integer (arg:object{medium_object}) 1)

 (defschema large_object
     a:integer b:bool c:integer d:object{small_object}
     e:integer f:bool g:integer h:object{small_object}
     i:integer j:bool k:integer l:object{small_object}
     m:integer n:bool o:integer p:object{small_object}
     )
 (defun arity_large_obj:integer (arg:object{large_object}) 1)
)

(create-table bench-accounts)

(create-account "Acct1" (read-keyset 'acct))
(fund-account "Acct1" 10000000.0)
(create-account "Acct2" (read-keyset 'acct))
