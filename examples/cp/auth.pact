(define-keyset 'module-admin
  (read-keyset "module-admin-keyset"))
(define-keyset 'operate-admin
  (read-keyset "module-operate-keyset"))

(module mpid 'module-admin

  (defschema mpid
    keyset:keyset
    )
  (deftable mpids:{mpid})


  (defun create-mpid (id keyset)
    (enforce-keyset 'operate-admin)
    (insert mpids id {
      "keyset": keyset
      })
  )

  (defun enforce-mpid-auth (id)
    "Enforce keyset for market participant ID, and return keyset"
    (with-read mpids id { "keyset":= k }
      (enforce-keyset k)
      k)
  )

)

(create-table mpids)
