; <native> = (vec tag ...)
(defun native-tag (x)
  (vec-ref x 0))

(defun native-fun (env pattern code) (vec 'fun env pattern code))
(defun native-builtin (run) (vec 'builtin run)) ; run :: (state values) => ()
(defun native-macro (env pattern code) (vec 'macro env pattern code))
(defun native-syntax (expand compile) (vec 'syntax expand compile)) ; expand :: (expand-child values) => values, compile :: (compile-child values) => code
(defun native-port (inp outp) (vec 'port inp outp))
(defun native-vec (v) (vec 'vec v))

(defun native-of? (t x)
  (= t (native-tag x)))

(defun native-inspect (x)
  (cond
    [(native-of? 'fun x) "<fun>"]
    [(native-of? 'builtin x) "<builtin>"]
    [(native-of? 'macro x) "<macro>"]
    [(native-of? 'syntax x) "<syntax>"]
    [(native-of? 'port x) "<port>"]
    [(native-of? 'vec x) (sexp-inspect (list->sexp (cons (sexp-sym "vec")
                                                         (vec->list (vec-ref x 1)))))]
    [else (error (str-concat "Unknown native tag: " (sym->str (native-tag x))))]))
