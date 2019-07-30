(module f F
  (defcap F () true)
  (defun f () true)
)

(module g G
  (use f [F])
  (defcap G () true)
  (defun g () (f))
)
