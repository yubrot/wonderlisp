(defun syntax-env ()
  (let1 e (env #f)
    (env-def! "def" syntax-def e)
    (env-def! "set!" syntax-set! e)
    (env-def! "begin" syntax-begin e)
    (env-def! "if" syntax-if e)
    (env-def! "fun" syntax-fun e)
    (env-def! "macro" syntax-macro e)
    (env-def! "builtin" syntax-builtin e)
    (env-def! "quote" syntax-quote e)
    e))

(defun syntax-expand (expand-child args)
  (map expand-child args))

(defun syntax-expand-except-first (expand-child args)
  (if (nil? args)
    args
    (let ([a (car args)]
          [b (cdr args)])
      (cons a (map expand-child b)))))

(defmacro def-lisp-syntax (name expand args . body)
  `(def ,name (sexp-native (native-syntax ,expand (fun ,args ,@body)))))

(def-lisp-syntax syntax-def syntax-expand-except-first (compile-child args)
  (unless (and (= (list-count args) 2) (sexp-of? 'sym (car args)))
    (vm-reflect (failure-evaluation "Syntax error: expacted (def sym x)")))
  (append (compile-child (cadr args))
          (list (inst-def (sexp-repr (car args)))
                (inst-ldc sexp-nil))))

(def-lisp-syntax syntax-set! syntax-expand-except-first (compile-child args)
  (unless (and (= (list-count args) 2) (sexp-of? 'sym (car args)))
    (vm-reflect (failure-evaluation "Syntax error: expacted (set! sym x)")))
  (append (compile-child (cadr args))
          (list (inst-set (sexp-repr (car args)))
                (inst-ldc sexp-nil))))

(defun syntax-seq (compile-child args)
  (let loop ([args args])
    (cond
      [(nil? args)
        (list (inst-ldc sexp-nil))]
      [(nil? (cdr args))
        (compile-child (car args))]
      [else
        (append (compile-child (car args))
                (list inst-pop)
                (loop (cdr args)))])))

(def-lisp-syntax syntax-begin syntax-expand (compile-child args)
  (syntax-seq compile-child args))

(def-lisp-syntax syntax-if syntax-expand (compile-child args)
  (unless (= (list-count args) 3)
    (vm-reflect (failure-evaluation "Syntax error: expacted (if cond then else)")))
  (let ([c (compile-child (car args))]
        [t (compile-child (cadr args))]
        [e (compile-child (caddr args))])
    (append c
            (list (inst-sel (append t (list inst-leave))
                            (append e (list inst-leave)))))))

(def-lisp-syntax syntax-fun syntax-expand-except-first (compile-child args)
  (when (nil? args)
    (vm-reflect (failure-evaluation "Syntax error: expected (fun pattern body...)")))
  (let ([pattern (vm-reflect (pattern-build (car args)))]
        [body (syntax-seq compile-child (cdr args))])
    (list (inst-ldf pattern (append body (list inst-leave))))))

(def-lisp-syntax syntax-macro syntax-expand-except-first (compile-child args)
  (when (nil? args)
    (vm-reflect (failure-evaluation "Syntax error: expected (macro pattern body...)")))
  (let ([pattern (vm-reflect (pattern-build (car args)))]
        [body (syntax-seq compile-child (cdr args))])
    (list (inst-ldm pattern body))))

(def-lisp-syntax syntax-builtin syntax-expand-except-first (compile-child args)
  (unless (and (= (list-count args) 1) (sexp-of? 'sym (car args)))
    (vm-reflect (failure-evaluation "Syntax error: expected (builtin sym)")))
  (list (inst-ldb (sexp-repr (car args)))))

(def-lisp-syntax syntax-quote syntax-expand-except-first (compile-child args)
  (unless (= (list-count args) 1)
    (vm-reflect (failure-evaluation "Syntax error: expected (quote expr)")))
  (list (inst-ldc (car args))))
