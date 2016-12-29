(define-keyset 'pact-admin (read-keyset "pact-admin-keyset"))

(module keysets 'pact-admin

 (defun keys-some (count matched)
   "Match at least one key in keyset"
   (> matched 0))

 (defun keys-all (count matched)
   "Match all keys"
   (= count matched))

 (defun keys-n (n count matched)
   "Match at least N keys"
   (matched >= n))

 (defun keys-2 (c m) "Match at least 2 keys" (keys-n 2 c m))
 (defun keys-3 (c m) "Match at least 3 keys" (keys-n 3 c m))

 (defun update-keyset (keyset-name new-keyset)
   (enforce-keyset keyset-name)
   (define-keyset keyset-name new-keyset))

)
