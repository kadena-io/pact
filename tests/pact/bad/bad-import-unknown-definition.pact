(module f F
  (defcap F () true)
  (defun f () true)
)

(module g G
  (use f [g])
  (defcap G () true)
  (defun g () (f))
)
