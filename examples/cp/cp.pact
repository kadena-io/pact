(use 'cash)
(use 'orders)

(module cp 'cp-module-admin

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
        (insert 'cp-master (is-string cusip)
          {
            "ticker": (is-string ticker),
            "issuer": issuer,
            "future-value": future-value,
            "discount-rate": discount-rate,
            "maturity": days-to-maturity,
            "par": (is-decimal par),
            "discount": discount,
            "cost": cost,
            "trade-date": date,
            "settlement-date": settlement-date
          })
        (issue-inventory issuer cusip 1 cost date)
        (format "Issued {}/{} with discount {}, cost {}, settlement date {}"
            ticker cusip discount cost settlement-date)
        ))

  (defun inventory-key (owner:string cusip:string)
    "Make composite key from OWNER and CUSIP"
    (format "{}:{}" owner cusip)
  )

  (defun issue-inventory (owner cusip qty price date)
    "Issue inventory for CUSIP recording QTY and PRICE held by OWNER"
    (insert 'cp-inventory (inventory-key owner cusip)
      {
        "qty": qty,
        "price": price,
        "date": date
      })
  )

  (defun transfer-inventory: string (owner:string cusip transferee qty price date)
    "Transfer CUSIP QTY from OWNER to TRANSFEREE, confirming PRICE"
    (let ((owner-key (inventory-key owner cusip))
          (transferee-key (inventory-key transferee cusip)))
      (with-read 'cp-inventory owner-key
        { "qty" := owner-owned,
          "price" := owner-price
        }
        (enforce (>= owner-owned (is-integer qty)) "Owner has inventory")
        (enforce (= owner-price (is-decimal price)) "Price matches inventory")
        (with-default-read 'cp-inventory transferee-key
          { "qty": 0 }
          { "qty" := transferee-owned }
          (update 'cp-inventory owner-key
            { "qty": (- owner-owned qty),
              "date": (is-time date) })
          (write 'cp-inventory transferee-key
            { "qty": (+ transferee-owned qty),
              "date": date }))))
  )


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
      (update-order-status order-id ORDER_FILLED date))
  )

  (defun settle-order (order-id cusip buyer seller date)
    "Open settlement, with payment and status update"
    (bind (with-order-status order-id ORDER_FILLED) {
      "price" := price
      }
      (make-payment buyer seller price date)
      (update-order-status order-id ORDER_PAID date))
  )


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


  (defun read-cp-master (cusip) (read 'cp-master cusip))

  (defun read-inventory (owner cusip)
    (read 'cp-inventory (inventory-key owner cusip)))

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

(create-table 'cp-master 'cp)
(create-table 'cp-inventory 'cp)
