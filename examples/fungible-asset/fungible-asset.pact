(interface ERC20
  @doc "ERC Token Standard #20 Interface \
  \ For more information, see the following: \
  \ https://github.com/ethereum/EIPs/blob/master/EIPS/eip-20-token-standard.md"
  @model
   [ (defproperty implies (a:bool b:bool) (when a b))
      ; since the token table is a singleton (ie has only one row), this
      ; property asserts that we only ever use that row.
      (defproperty token-table-safety
        (and
          ; the token table is a singleton. we only read / write to TOKEN_ROW.
          (and
            (forall (row:string)
              (implies (row-read    state-table row) (= row 'TOKEN_ROW)))
            (forall (row:string)
              (implies (row-written state-table row) (= row 'TOKEN_ROW))))
          ; additionally, TOKEN_ROW always exists, but no other row ever does
          (implies
            (row-exists state-table 'TOKEN_ROW 'before)
            (row-exists state-table 'TOKEN_ROW 'after)
          )
        ))
      (property (conserves-mass account 'balance))
      ; issuance monotonically increasing
      (property
        (>=
          (at 'issued (read accounts TOKEN_ROW 'after))
          (at 'issued (read accounts TOKEN_ROW 'before))
        ))
      ; reserve monotonically decreasing
      (property
        (<=
          (at 'reserve (read accounts TOKEN_ROW 'after))
          (at 'reserve (read accounts TOKEN_ROW 'before))
        ))
    ]

    (defconst TOKEN_ROW 'TOKEN_ROW)

  ; --------------------------------------------------------------

  (defun totalSupply:integer ()
    "totalSupply should be non-negative"
    @model [(property (>= result 0))])

  (defun balanceOf:integer (tokenOwner:keyset)
    "balance should be non-negative"
    @model [(property (>= result 0))])

  (defun allowance:integer (tokenOwner:keyset address:string)
    "token allowance should be non-negative"
    @model [(property (>= result 0))])

  (defun approve:bool (spender:keyset tokens:integer)
    "transferred token amounts should always be positive"
    @model [(property (> tokens 0))])

  (defun transferFrom:bool (from:keyset to:keyset tokens:integer)
    "sender and reciever should be different accounts \
    \ and all token transfers should be nontrivial"
    @model
      [ (property (> tokens 0))
        (property (not (= from to)))
      ]
    )
)
