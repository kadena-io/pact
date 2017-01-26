
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


(declare-const resS String)
(declare-const fmtS String)
(declare-const derivedFmtS String)
(declare-const args (List String))

(define-fun-rec prep-format ((fmtS String)) (List String)
  (ite (= fmtS "{}")
       (insert "a" nil)
    (ite (str.prefixof "{}" fmtS)
         (insert "b" (prep-format (str.substr fmtS 2 (- (str.len fmtS) 2))))
       (ite (not (str.contains fmtS "{}"))
            (insert fmtS nil)
            (let ((loc (str.indexof "{}" fmtS)))
              (let ((initS (str.substr fmtS 0 loc))
                    (restS (str.substr fmtS (+ loc 1) (- (str.len fmtS) (+ loc 2))))
                    )
                (insert initS (insert "c" (ite (= "" restS) nil (prep-format restS))))
              )
            )
       )
    )
  )
)

(assert (= args (prep-format "foo{}")))
;(define-fun-rec str.replace-after ((loc Int) (src String) (target String) (dest String)) String
;  (let ((rest (str.substr src loc (- (str.len src) loc)))
;        (init (str.substr src 0 loc))
;        )
;    (str.++ init (str.replace rest target dest))
;  )
;)
;
;(define-fun-rec safe-format ((after Int) (fmtS String) (args (List String))) String
;  (ite (or (= nil args) (not (str.contains fmtS "{}")))
;       fmtS
;       (let ((loc (str.indexof "{}" fmtS after)))
;         (let ((nextLoc (+ (- loc 2) (str.len (head args)))))
;           (safe-format nextLoc (str.replace-after loc fmtS "{}" (head args)) (tail args))
;         )
;       )
;  )
;)
;(define-fun-rec format ((fmtS String) (args (List String))) String (safe-format 0 fmtS args))
;
;(assert (= args (insert "{}" (insert "bar" (insert "baz" nil)))))
;(assert (= resS (format "{}:{}:{}:[]" args)))
;(assert (= "foo:bar:baz:[]" (format derivedFmtS args)))
;(assert (not (= "foo:bar:baz:[]" derivedFmtS)))
(check-sat)
(get-model)
