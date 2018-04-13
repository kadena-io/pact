
(declare-const a Real)
(declare-const b String)
(define-fun-rec real.show.frac ((n Real)) String
  ; print out the digits after the decimal point
  (
   ite (= (to_real (to_int n)) n)
       (let ((s (int.to.str (to_int n))))
         (str.substr s 1 (- (str.len s) 1))
       )
       (real.show.frac (* 10 n))
  )
)
(define-fun-rec real.show.int ((n Real)) String
  ; print out the digits before the decimal point
  (int.to.str (to_int n))
)

(define-fun-rec real.to.str ((n Real)) String
  (ite (= n (to_real (to_int n)))
       (int.to.str (to_int n))
       (str.++ (str.++ (real.show.int n) ".") (real.show.frac n))
  )
)

(declare-const foo Int)
(declare-const resS String)
(declare-const fmtS String)
(declare-const derivedFmtS String)
(declare-const args (List String))

(define-fun-rec prep-format ((fmtS String)) (List String)
  (ite (or (= fmtS "{}") (= fmtS ""))
       (ite (= fmtS "{}") (insert "" nil) nil)
    (ite (not (str.contains fmtS "{}"))
        (insert fmtS nil)
        (let ((loc (str.indexof fmtS "{}")))
          (let ((initS (str.substr fmtS 0 loc))
                (restS (str.substr fmtS (+ loc 2) (- (str.len fmtS) (+ loc 2))))
                )
            (ite (= initS "")
              (insert "" (prep-format restS))
              (insert initS (insert "" (prep-format restS)))
            )
          )
        )
    )
  )
)

;(assert (= args (prep-format "{}{}f{}:{}")))
;(assert (= resS (str.prefixof "{}" "{}")))

(define-fun-rec zip-format ((fmt (List String)) (args (List String))) String
  (ite (= nil fmt)
       ""
       (ite (= nil args)
         (str.++ (head fmt) (zip-format (tail fmt) args))
         (ite (= "" (head fmt))
           (str.++ (head args) (zip-format (tail fmt) (tail args)))
           (str.++ (head fmt) (zip-format (tail fmt) args))
         )
       )
  )
)


(define-fun-rec format ((fmt String) (args (List String))) String
  (zip-format (prep-format fmt) args)
)

(assert (= args (insert "{}" (insert "bar" (insert "baz" nil)))))
(assert (= resS (format "{}:{}:{}:[]" args)))
(assert (= "{}:bar:baz:[]" (format derivedFmtS args)))
(assert (not (= "{}:bar:baz:[]" derivedFmtS)))
(check-sat)
(get-model)