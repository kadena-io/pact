;; Bad import - name overlap between definitions and imports
(module f F
  (defcap F () true)
  (defun f () true)
)

(module g G
  (use f [f])
  (defcap G () true)
  (defun f () true)
)
