;; Bad import - empty import lists should fail
(module f F
  (defcap F () true)
  (defun f () true)
)

(module g G
  (use f [])
  (defcap G () true)
  (defun g () (f))
)
