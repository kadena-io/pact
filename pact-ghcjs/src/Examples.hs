{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Examples where

-- We use the string-qq package because file-embed doesn't build with GHCJS

import Data.String.QQ
import Data.Text

exampleData :: [(Text, Text)]
exampleData =
  [ ("Hello World", hello)
  , ("Simple Payment", simplePayment)
  , ("International Payment", internationalPayment)
  , ("Commercial Paper", commercialPaper)
  ]

hello :: Text
hello = [s|
;;
;; "Hello, world!" smart contract/module
;;

;; Simulate message data specifying an administrator keyset.
;; In production use 'mockAdminKey' would be an ED25519 hex-encoded public key.
(env-data { "admin-keyset": ["mockAdminKey"] })

;; Simulate that we've signed this transaction with the keyset.
;; In pact, signatures are pre-validated and represented in the
;; environment as a list of public keys.
(env-keys ["mockAdminKey"])

;; Keysets cannot be created in code, thus we read them in
;; from the load message data.
(define-keyset 'admin-keyset (read-keyset "admin-keyset"))

;; Define the module.
(module helloWorld 'admin-keyset
  "A smart contract to greet the world."
  (defun hello (name)
    "Do the hello-world dance"
    (format "Hello {}!" [name]))
)

;; and say hello!
(hello "world")
|]

simplePayment :: Text
simplePayment = [s|
;;
;; Simple accounts model.
;;


;; setup environment with administrator and user keysets
(env-data { "admin-keyset": { "keys": ["ADMIN"] },
            "sarah-keyset": { "keys": ["SARAH"] },
            "james-keyset": { "keys": ["JAMES"] }
            } )
;... and is signed by ADMIN key
(env-keys ["ADMIN"])

;define keyset to guard module
(define-keyset 'admin-keyset (read-keyset "admin-keyset"))

;define smart-contract code
(module payments 'admin-keyset

  (defschema payments
    balance:decimal
    keyset:keyset)

  (deftable payments-table:{payments})

  (defun create-account (id initial-balance keyset)
    "Create a new account for ID with INITIAL-BALANCE funds, must be administrator."
    (enforce-keyset 'admin-keyset)
    (enforce (>= initial-balance 0.0) "Initial balances must be >= 0.")
    (insert payments-table id
            { "balance": initial-balance,
              "keyset": keyset }))

  (defun get-balance (id)
    "Only users or admin can read balance."
    (with-read payments-table id
      { "balance":= balance, "keyset":= keyset }
      (enforce-one "Access denied"
        [(enforce-keyset keyset)
         (enforce-keyset 'admin-keyset)])
      balance))

  (defun pay (from to amount)
    (with-read payments-table from { "balance":= from-bal, "keyset":= keyset }
      (enforce-keyset keyset)
      (with-read payments-table to { "balance":= to-bal }
        (enforce (> amount 0.0) "Negative Transaction Amount")
        (enforce (>= from-bal amount) "Insufficient Funds")
        (update payments-table from
                { "balance": (- from-bal amount) })
        (update payments-table to
                { "balance": (+ to-bal amount) })
        (format "{} paid {} {}" [from to amount]))))

)

;define table
(create-table payments-table)

;create accounts
(create-account "Sarah" 100.25 (read-keyset "sarah-keyset"))
(create-account "James" 250.0 (read-keyset "james-keyset"))


;; do payment, simluating SARAH keyset.
(env-keys ["SARAH"])
(pay "Sarah" "James" 25.0)
(format "Sarah's balance is {}" [(get-balance "Sarah")])

;; read James' balance as JAMES
(env-keys ["JAMES"])
(format "James's balance is {}" [(get-balance "James")])
|]

internationalPayment :: Text
internationalPayment = [s|
(env-data { "keyset": { "keys": ["ABCD"] , "pred": "keys-all" } })
(env-keys ["ABCD"])

(define-keyset 'module-keyset (read-keyset "keyset"))

;USD Ledger
(module paymentsUSD 'module-keyset

  (deftable payments-tableUSD)

  (defun create-accountUSD (id initial-balance)
    "Create a new account for ID with INITIAL-BALANCE funds"
    (insert payments-tableUSD id { "balance": initial-balance }))

  (defun debitUSD (from amount)
    (with-read payments-tableUSD from { "balance":= from-bal }
        (enforce (>= from-bal amount) "Insufficient Funds")
        (update payments-tableUSD from
                { "balance": (- from-bal amount) })
        (format "{} debited {}" [from amount])))
)

;create USD Ledger table
(create-table payments-tableUSD)
(create-accountUSD "Sarah" 100.00)

;JPY Ledger
(module paymentsJPY 'module-keyset

  (deftable payments-tableJPY)

  (defun create-accountJPY (id initial-balance)
    "Create a new account for ID with INITIAL-BALANCE funds"
    (insert payments-tableJPY id { "balance": initial-balance }))

  (defun creditJPY (to amount)
    (with-read payments-tableJPY to { "balance":= to-bal }
      (update payments-tableJPY to
              { "balance": (+ to-bal amount) })
      (format "{} credited {}" [to amount])))
)

;create JPY Ledger
(create-table payments-tableJPY)
(create-accountJPY "James" 0.0)

;Cross Border Transfer Specification
(module cross-border 'module-keyset
  (defun transfer-usd-to-jpy-accts (from to amountUSD fx-rate)
    (let* ((amountJPY (* amountUSD fx-rate)))
      (debitUSD from amountUSD)
      (creditJPY to amountJPY)
      (format "{} transfered ${} USD to {} at the rate of {}" [from amountUSD to fx-rate])))
)

;do a transfer
(transfer-usd-to-jpy-accts "Sarah" "James" 50.0 104.57)
|]

commercialPaper :: Text
commercialPaper = [s|
(env-data { "module-admin-keyset": { "keys": ["admin"], "pred": "keys-all"}})
(env-keys ["admin"])

;---------------------------------
;      Admin keyset
;---------------------------------
(begin-tx "keyset definition")
(define-keyset 'cp-module-admin
  (read-keyset "module-admin-keyset"))
(commit-tx)

;---------------------------------
;      Cash Workflow Logic
;---------------------------------
(begin-tx "cash module")
(module cash 'cp-module-admin

  (defschema entry
    ccy:string
    balance:decimal
    change:decimal
    date:time)

  (deftable cash:{entry})

  (defun debit (id amount date)
    "Debit ID for AMOUNT, checking balance for available funds"
    (with-read cash id { "balance":= balance }
      (enforce (>= balance amount) "Insufficient funds")
      (update cash id {
        "balance": (- balance amount),
        "change": (- amount),
        "date": date
        })))

  (defun credit (id amount date)
    "Credit ID with AMOUNT"
    (with-read cash id { "balance" := balance}
      (update cash id {
        "balance": (+ balance amount),
        "change": amount,
        "date": date
        })))

  (defun make-payment (payor payee amount date)
    "Debit PAYOR and credit PAYEE AMOUNT"
    (debit payor amount date)
    (credit payee amount date))

  (defun create-account (id ccy amount date)
    "Create account ID for CCY and fund with AMOUNT"
    (insert cash id {
      "ccy": ccy,
      "balance": amount,
      "change": amount,
      "date": date }))

  (defun read-account (id) (read cash id))
)

(create-table cash)
(commit-tx)

;---------------------------------
;      Orders Workflow Logic
;---------------------------------
(begin-tx "orders module")
(module orders 'cp-module-admin
  (defschema order
     cusip:string
     buyer:string
     seller:string
     price:decimal
     qty:integer
     ccy:string
     order-date:time
     status:string
     modify-date:time)

  (deftable cp-orders:{order})


  (defconst ORDER_NEW "NEW")
  (defconst ORDER_FILLED "FILLED")
  (defconst ORDER_CANCELED "CANCELED")
  (defconst ORDER_PAID "PAID")

  (defun new-order (order-id cusip buyer seller qty price ccy date)
    "Create new order ORDER-ID"
    (insert cp-orders order-id {
        "cusip": cusip,
        "buyer": buyer,
        "seller": seller,
        "price": price,
        "qty": qty,
        "ccy": ccy,
        "order-date": date,
        "status": ORDER_NEW,
        "modify-date": date
        }))

  (defun read-order (order-id) (read cp-orders order-id))

  (defun update-order-status (order-id status date)
    (enforce (or (= ORDER_NEW status)
              (or (= ORDER_FILLED status)
               (or (= ORDER_CANCELED status)
                   (= ORDER_PAID status))))
             "Invalid status")
    (update cp-orders order-id
      { "status": status , "modify-date": date }))

  (defun with-order-status (order-id status)
    "Check that order status is correct, returning details"
    (with-read cp-orders order-id {
      "cusip" := cusip,
      "status" := ostatus,
      "qty" := qty,
      "price" := price,
      "seller" := seller,
      "buyer" := buyer
      }
      (enforce (= ostatus status) (format "order must be {}" [status]))
      {"cusip": cusip,
       "qty": qty,
       "price": price,
       "buyer": buyer,
       "seller": seller }))

  (defun with-order (order-id)
    "Get order details"
    (read cp-orders order-id))

  (defun cancel-order (order-id date)
    (with-read cp-orders order-id {"status" := status }
      (enforce (= ORDER_NEW status) "only NEW orders can be canceled")
      (update-order-status order-id ORDER_CANCELED)))
)

(create-table cp-orders)
(commit-tx)

;---------------------------------
; Commercial Paper Workflow Logic
;---------------------------------
(begin-tx "cp module")
(module cp 'cp-module-admin

  (defschema cp-asset
    ticker:string
    issuer:string
    future-value:decimal
    discount-rate:decimal
    maturity:integer
    par:decimal
    discount:decimal
    cost:decimal
    trade-date:time
    settlement-date:time)

  (deftable cp-master:{cp-asset})

  (defschema inventory
    qty:integer
    price:decimal
    date:time)

  (deftable cp-inventory:{inventory})

  (defun issue (issuer cusip ticker future-value discount-rate
                days-to-maturity par date)
    "ISSUER issues CUSIP with specified values, computing discount, cost, settlement date"
    (enforce (> future-value 0.0) "Valid future-value")
    (enforce (and (>= discount-rate 0.0)
                (< discount-rate 100.0))
              "Valid discount-rate")
    (enforce (> days-to-maturity 0) "Valid days-to-maturity")
    (let* ((discount (calculate-discount future-value
                        discount-rate days-to-maturity))
           (cost (- future-value discount))
           (settlement-date (add-time date (days days-to-maturity))))
        (insert cp-master cusip
          {
            "ticker": ticker,
            "issuer": issuer,
            "future-value": future-value,
            "discount-rate": discount-rate,
            "maturity": days-to-maturity,
            "par": par,
            "discount": discount,
            "cost": cost,
            "trade-date": date,
            "settlement-date": settlement-date
          })
        (issue-inventory issuer cusip 1 cost date)
        (format "Issued {}/{} with discount {}, cost {}, settlement date {}"
            [ticker cusip discount cost settlement-date])))

  (defun inventory-key (owner cusip)
    "Make composite key from OWNER and CUSIP"
    (format "{}:{}" [owner cusip]))

  (defun issue-inventory (owner cusip qty price date)
    "Issue inventory for CUSIP recording QTY and PRICE held by OWNER"
    (insert cp-inventory (inventory-key owner cusip)
      {
        "qty": qty,
        "price": price,
        "date": date
      }))

  (defun transfer-inventory (owner cusip transferee qty price date)
    "Transfer CUSIP QTY from OWNER to TRANSFEREE, confirming PRICE"
    (let ((owner-key (inventory-key owner cusip))
          (transferee-key (inventory-key transferee cusip)))
      (with-read cp-inventory owner-key
        { "qty" := owner-owned,
          "price" := owner-price
        }
        (enforce (>= owner-owned qty) "Owner has inventory")
        (enforce (= owner-price price) "Price matches inventory")
        (with-default-read cp-inventory transferee-key
          { "qty": 0 }
          { "qty" := transferee-owned }
          (update cp-inventory owner-key
            { "qty": (- owner-owned qty),
              "date": date })
          (write cp-inventory transferee-key
            { "qty": (+ transferee-owned qty),
              "date": date,
              "price": price })))))

  (defun calculate-discount (future-value discount-rate days-to-maturity)
    (* future-value
      (* (/ discount-rate 100.0)
         (/ days-to-maturity 360.0))))

  (defun fill-order-transfer (order-id seller date)
    "Fill new order ORDER-ID"
    (bind (with-order-status order-id ORDER_NEW) {
      "cusip" := cusip,
      "qty" := qty,
      "price" := price,
      "buyer" := buyer,
      "seller" := order-seller
      }
      (enforce (= order-seller seller) "Seller must match order seller")
      (transfer-inventory seller cusip buyer qty price date)
      (update-order-status order-id ORDER_FILLED date)))

  (defun settle-order (order-id cusip buyer seller date)
    "Open settlement, with payment and status update"
    (bind (with-order-status order-id ORDER_FILLED) {
      "price" := price
      }
      (make-payment buyer seller price date)
      (update-order-status order-id ORDER_PAID date)))

  (defun settle-order-buyer (order-id buyer date)
    "Private settlement, debit step"
    (bind (with-order-status order-id ORDER_FILLED) {
      "price" := price
      }
      (debit buyer price date)))

  (defun refund-order (order-id buyer date)
    "Private settlement, rollback debit step"
    (bind (with-order order-id) { "price" := price }
      (credit buyer price date)))

  (defun settle-order-seller (order-id seller date)
    "Private settlement, credit and status update"
    (bind (with-order order-id) { "price" := price }
      (credit seller price date)
      (update-order-status order-id ORDER_PAID date)))


  (defun read-cp-master (cusip) (read cp-master cusip))

  (defun read-inventory (owner cusip)
    (read cp-inventory (inventory-key owner cusip)))

  ;; scenario1
  (defpact issue-order-fill-settle (agent-entity trader-entity
                      agent trader cusip ticker future-value discount-rate
                      days-to-maturity par order-id date)
    ;; 0: issuance
    (step agent-entity
      (issue agent cusip ticker future-value discount-rate
             days-to-maturity par date))
    ;; 1: new order
    (step-with-rollback trader-entity
      (new-order order-id cusip trader agent 1
          (at "cost" (read-cp-master cusip)) "USD" date)
      ;;rollback
      (cancel-order order-id date))

    ;; 2: fill
    (step agent-entity
      (fill-order-transfer order-id agent date))

    ;; 3: pay
    (step-with-rollback trader-entity
      (settle-order-buyer order-id trader date)
      ;;rollback
      (refund-order order-id trader date))

    ;; 4: settle
    (step agent-entity
      (settle-order-seller order-id agent date))
  )
)

(create-table cp-master)
(create-table cp-inventory)
(commit-tx)

;---------------------------------
;      REPL Code portion
;---------------------------------
(module cp-test 'cp-module-admin
  (defconst t1 (time "2016-09-01T11:00:00Z"))
  (defconst t2 (add-time t1 30))
  (defconst t3 (add-time t2 30))
  (defun runpact-scenario-1 (order-id cusip)
    (issue-order-fill-settle
      "AGENT" "TRADER" "agent" "trader" cusip "GE CP"
      100000.0 7.35 30 1000.0 order-id t1)
  )
)


(create-account "agent" "USD" 0.0 t1)
(create-account "trader" "USD" 100000.0 t1)

(issue "agent" "cusip1" "GE CP" 100000.0 7.35 30 1000.0 t1)
(expect "agent has inventory" 1 (at "qty" (read-inventory "agent" "cusip1")))


(new-order "order1" "cusip1" "trader" "agent" 1 99387.5 "USD" t1)

(fill-order-transfer "order1" "agent" t2)

(expect "order record correct"
  {"cusip": "cusip1", "buyer": "trader", "seller": "agent", "price": 99387.5,
  "qty": 1, "ccy": "USD", "order-date": t1,
  "status": ORDER_FILLED, "modify-date": t2}
  (read-order "order1"))

(expect "agent inventory" 0 (at "qty" (read-inventory "agent" "cusip1")))
(expect "trader inventory" 1 (at "qty" (read-inventory "trader" "cusip1")))

(settle-order "order1" "cusip1" "trader" "agent" t3)
(expect "trader balance" 612.5 (at "balance" (read-account "trader")))
(expect "agent balance" 99387.5 (at "balance" (read-account "agent")))
(expect "order paid" ORDER_PAID (at "status" (read-order "order1")))

(read-cp-master "cusip1")
|]
