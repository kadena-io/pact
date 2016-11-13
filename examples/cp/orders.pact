(module orders 'cp-module-admin


  (defconst ORDER_NEW "NEW")
  (defconst ORDER_FILLED "FILLED")
  (defconst ORDER_CANCELED "CANCELED")
  (defconst ORDER_PAID "PAID")

  (defun new-order (order-id cusip buyer seller qty price ccy date)
    "Create new order ORDER-ID"
    (insert 'cp-orders order-id {
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

  (defun read-order (order-id) (read 'cp-orders order-id))

  (defun update-order-status (order-id status date)
    (enforce (or (= ORDER_NEW status)
              (or (= ORDER_FILLED status)
               (or (= ORDER_CANCELED status)
                   (= ORDER_PAID status))))
             "Invalid status")
    (update 'cp-orders order-id
      { "status": status , "modify-date": date })

  )

  (defun with-order-status (order-id status)
    "Check that order status is correct, returning details"
    (with-read 'cp-orders order-id {
      "cusip" := cusip,
      "status" := ostatus,
      "qty" := qty,
      "price" := price,
      "seller" := seller,
      "buyer" := buyer
      }
      (enforce (= ostatus status) (format "order must be {}" status))
      {"cusip": cusip,
       "qty": qty,
       "price": price,
       "buyer": buyer,
       "seller": seller })
  )

  (defun with-order (order-id)
    "Get order details"
    (read 'cp-orders order-id)
  )

  (defun cancel-order (order-id date)
    (with-read 'cp-orders order-id {"status" := status }
      (enforce (= ORDER_NEW status) "only NEW orders can be canceled")
      (update-order-status order-id ORDER_CANCELED))
  )

)


(create-table 'cp-orders 'orders)
