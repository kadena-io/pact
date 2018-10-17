
(define-keyset 'auction-admin-keyset
  (read-keyset "auction-admin-keyset"))

(module auction 'auction-admin-keyset
  @doc "Vickrey auction with partial verification"

  @model
    [
      ; TODO: it would be convenient to have the equivalent of haskell's and:
      ; (defproperty and' (bs:[bool]) (fold (and) true bs))

      (defproperty implies (a:bool b:bool) (not (and a (not b))))

      ; since the state table is a singleton (ie has only one row), this
      ; property asserts that we only ever use that row.
      (defproperty state-table-safety
        (and
          ; the state table is a singleton. we only read / write to STATE_ROW.
          (and
            ; TODO: should be able to use STATE_ROW variable
            (forall (row:string)
              (implies (row-read    state-table row) (= row 'STATE_ROW)))
            (forall (row:string)
              (implies (row-written state-table row) (= row 'STATE_ROW))))

          ; additionally, STATE_ROW always exists, but no other row ever does
          ; (and
            (implies
              (row-exists state-table 'STATE_ROW 'before)
              (row-exists state-table 'STATE_ROW 'after))

            ; TODO:
            ; (forall (row:string)
            ;   (implies
            ;     (row-exists state-table row 'after)
            ;     (= row 'STATE_ROW)))

            ; TODO:
            ; (not (exists (row:string)
            ;   (and
            ;     (!= row 'STATE_ROW)
            ;     (row-exists state-table row 'after))))
                ))

      ; TODO: let-binding
      ; (defproperty transition-safety
      ;   (let* (
      ;     ; TODO: destructuring read
      ;     (pre  (at 'step (read state-table 'STATE_ROW 'before)))
      ;     (post (at 'step (read state-table 'STATE_ROW 'after))))

      ;   (and
      ;     ; NOT_STARTED -> NOT_STARTED + RUNNING
      ;     (implies
      ;       (= pre 'NOT_STARTED)
      ;       (or
      ;         (= post 'NOT_STARTED)
      ;         (= post 'RUNNING)))

      ;     ; RUNNING -> RUNNING + ENDED
      ;     (and
      ;       (implies
      ;         (= pre RUNNING)
      ;         (or
      ;           (= post 'RUNNING)
      ;           (= post 'ENDED)))

      ;     ; ENDED is terminal
      ;       (implies
      ;         (= pre 'ENDED)
      ;         (= post 'ENDED))))
      ;   ))


      ; temporary stand-in. this checks the first step transitions safely.
      (defproperty transition-safety
        ; NOT_STARTED -> RUNNING
        (implies
          (= step-before 'NOT_STARTED)
          (or
            (= step-after 'NOT_STARTED)
            (= step-after 'RUNNING))))

      (defproperty step-before
        (at 'step (read state-table 'STATE_ROW 'before)))

      (defproperty step-after
        (at 'step (read state-table 'STATE_ROW 'after)))

      (defproperty step-transition (from:string to:string)
        (and
          (= step-before from)
          (= step-after  to)))

      (defproperty maintains-step
        (forall (step:string)
          (implies
            (= step step-before)
            (= step step-after))))

      ; every function in the module is safe re the state table
      (property state-table-safety)

      ; every function in the module transitions safely
      (property transition-safety)

      ; only start-auction and end-auction change the step
      (property maintains-step {'except: [init, start-auction, end-auction]})
      (property (step-transition 'NOT_STARTED 'RUNNING)
        { 'only: [start-auction] })
      (property (step-transition 'RUNNING 'ENDED)
        { 'only: [end-auction] })
    ]

  (defschema state
    @doc   "Auction state"
    @model (invariant
      (or   (= step 'NOT_STARTED)
        (or (= step 'RUNNING)
            (= step 'ENDED))))
    step:string
    )

  (defconst STATE_ROW 'STATE_ROW)

  (deftable state-table:{state}
    "Main table for auction state.")

  (defschema concealed-bid-contents
    ; hash of bid contents
    ; TODO: how to do user?
    amount:decimal)

  (defschema concealed-bid
    ; hash of bid contents
    bid:string)

  (defun init:string ()
    (insert state-table STATE_ROW { 'step: 'NOT_STARTED }))

  (defun noop:string () "" "")

  (defun start-auction:string ()
    "TODO"
    (let* ((step:string (at 'step (read state-table STATE_ROW)))
           (okay:bool (= step 'NOT_STARTED)))
      (enforce okay "must not have already started"))
    (write state-table STATE_ROW { 'step: 'RUNNING }))

  (defun end-auction:string ()
    "TODO"
    (let ((step:string (at 'step (read state-table STATE_ROW))))
      (enforce (= step 'RUNNING) "must be running"))
    (write state-table STATE_ROW { 'step: 'ENDED }))

  ; TODO:
  ; bid
  ; reveal
  ; choose winner / refund
  ; etc
)
