(defrecord pattern pattern pattern?
  ([fixed pattern-fixed *pattern-set-fixed!]
   [rest pattern-rest]))

(defun pattern->str (p)
  (let ([fixed (pattern-fixed p)]
        [rest (pattern-rest p)])
    (sexp-inspect (foldr sexp-cons (if rest (sexp-sym rest) sexp-nil) (map sexp-sym fixed)))))

(defun pattern-build (s)
  (let/cc return
    (success
      (let loop ([s s])
        (cond
          [(sexp-of? 'sym s) (pattern () (sexp-repr s))]
          [(sexp-of? 'nil s) (pattern () #f)]
          [(sexp-of? 'cons s)
            (let ([a (car (sexp-repr s))]
                  [p (loop (cdr (sexp-repr s)))])
              (unless (= 'sym (sexp-tag a))
                (return (failure-evaluation "Unsupported pattern: " (sexp-inspect a))))
              (*pattern-set-fixed! p (cons (sexp-repr a) (pattern-fixed p)))
              p)]
          [else
            (return (failure-evaluation "Unsupported pattern: " (sexp-inspect s)))])))))

(defun pattern-bind (p args env)
  (let loop ([args args]
             [params (pattern-fixed p)])
    (cond
      [(and (nil? args) (nil? params))
        (if (pattern-rest p)
          (success (env-def! (pattern-rest p) sexp-nil env))
          (success ()))]
      [(nil? args)
        (failure-evaluation "This function takes at least " (num->str (list-count (pattern-fixed p))) " arguments")]
      [(nil? params)
        (if (pattern-rest p)
          (success (env-def! (pattern-rest p) (list->sexp args) env))
          (failure-evaluation "Unnecessary arguments passed (expected " (num->str (list-count (pattern-fixed p))) " arguments)"))]
      [else
        (env-def! (car params) (car args) env)
        (loop (cdr args) (cdr params))])))
