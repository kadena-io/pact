
(module covtest GOVERNANCE

  (defcap GOVERNANCE:bool () true)

  (defconst CONST:decimal 1.0)

  (defschema sch
      guard:guard
      val:decimal
      )

  (deftable tbl:{sch})

  (defcap CAP:bool (k:string)
    (enforce-guard
     (at 'guard
         (read tbl k))))

  (defun create:string
    ( k:string
      g:guard
      val:decimal
    )
    (insert tbl k
            { 'guard: g
            , 'val: val
              }))

  (defun update-val:string
    ( k:string
      val:decimal
    )
    (with-capability (CAP k)
        (update tbl k { 'val: val })))

  (defun increase:string
    ( k:string
      d:decimal
    )
    (let ((curr (at 'val (read tbl k))))
      (enforce (> d curr) "must increase")
      (update-val k d)) ;; deliberately skipping this
    )
)

(create-table tbl)
