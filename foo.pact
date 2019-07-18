(module f G

  (defcap G () true)

  (defun f () true)
)

(module g G

  (use f ['f])

  (defcap G () true)

  (defun g () (f))
)
