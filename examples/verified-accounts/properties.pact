(enforce-pact-version "2.4.1")

(define-keyset 'accounts-admin-keyset
  (read-keyset "accounts-admin-keyset"))

(module properties 'accounts-admin-keyset
  "Reusable properties"

  ; TODO: abstract over table / column
  (defproperty conserves-mass
    (= (column-delta 'accounts 'balance) 0.0))

  (defproperty auth-required
    (authorized-by 'accounts-admin-keyset)
    ))
