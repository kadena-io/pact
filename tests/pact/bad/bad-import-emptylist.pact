;; Bad import - empty import lists should fail if no module hash
;; is supplied.
(module f F
  (defcap F () true)
  (defun f () true)
)

(module g G
  (use f [])
  (defcap G () true)
  (defun g () (f))
)
