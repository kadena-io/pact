(module f F
  (defcap F () true)
  (defun f () true)
)

(module g G
  (defcap G () true)
  (defun f () (F))
)
