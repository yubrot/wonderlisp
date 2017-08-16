(defun failure-evaluation msg
  (failure (str-concat "Evaluation error: " (apply str-concat msg))))

(defun failure-internal msg
  (failure (str-concat "Internal error: " (apply str-concat msg))))

(def vm-reflect result-reflect)

(defrecord vm-context *vm-create vm-context?
  ([toplevel *vm-toplevel]
   [builtins vm-builtins]))

(defrecord vm-state *vm-state vm-state?
  ([stack *vm-stack *vm-set-stack!]
   [env *vm-env *vm-set-env!]
   [code *vm-code *vm-set-code!]
   [dump *vm-dump *vm-set-dump!]
   [context vm-context]))

(defun vm-create ()
  (*vm-create (env (syntax-env)) (tbl)))

(defun vm-compile (ctx value)
  (*vm-compile (*vm-toplevel ctx) value))

(defun vm-macroexpand (recurse ctx value)
  (*vm-macroexpand recurse ctx value))

(defun vm-eval (ctx s)
  (result-reify
    (let ([s (result-reflect (vm-macroexpand #t ctx s))]
          [code (result-reflect (vm-compile ctx s))])
      (result-reflect (*vm-exec ctx (*vm-toplevel ctx) code)))))

(defun *vm-compile (compile-env s)
  (result-reify
    (let1 compile (fun (s) (result-reflect (*vm-compile compile-env s)))
      (cond
        [(sexp-of? 'sym s)
          (list (inst-ldv (sexp-repr s)))]
        [(sexp-of? 'cons s)
          (let1 ls (sexp->list s)
            ; (... foo . bar)
            (when (failure? ls)
              (result-reflect (failure-evaluation "Improper list: " (sexp-inspect s))))
            (let ([f (car (force-success ls))]
                  [args (cdr (force-success ls))]
                  [syntax-or-f (env-refer f compile-env)])
              (cond
                ; (syntax ...)
                [(and syntax-or-f (sexp-of-native? 'syntax syntax-or-f))
                  (let1 syntax-compile (vec-ref (sexp-repr syntax-or-f) 2)
                    (syntax-compile compile args))]
                ; (fun|builtin ...)
                [else
                  (let ([f (compile f)]
                        [args (map compile args)]
                        [app (inst-app (list-count args))])
                    (append f (apply append args) (list app)))])))]
        [else
          (list (inst-ldc s))]))))

(defun vm-push (state v)
  (*vm-set-stack! state (cons v (*vm-stack state))))

(defun vm-pop (state)
  (if (nil? (*vm-stack state))
    (failure-internal "Inconsistent stack")
    (let1 hd (car (*vm-stack state))
      (*vm-set-stack! state (cdr (*vm-stack state)))
      (success hd))))

(defun *vm-push-dump (state env code)
  (*vm-set-dump! state (cons (cons env code) (*vm-dump state))))

(defun *vm-pop-dump (state)
  (if (nil? (*vm-dump state))
    (failure-internal "Inconsistent dump")
    (let1 hd (car (*vm-dump state))
      (*vm-set-dump! state (cdr (*vm-dump state)))
      (success hd))))

(defun vm-enter (state env code)
  (let1 ip (*vm-code state)
    (unless (and (cons? ip) (inst-of? op-leave (car ip)) (nil? (cdr ip)))
      (*vm-push-dump state (*vm-env state) (*vm-code state)))
    (*vm-set-env! state env)
    (*vm-set-code! state code)))

(defun vm-leave (state)
  (result-reify
    (let1 dump (result-reflect (*vm-pop-dump state))
      (*vm-set-env! state (car dump))
      (*vm-set-code! state (cdr dump)))))

(defun vm-apply (state f args)
  (result-reify
    (cond
      [(sexp-of-native? 'fun f)
        (let1 at (fun (i) (vec-ref (sexp-repr f) i))
          (vm-enter state (env (at 1)) (at 3))
          (result-reflect (pattern-bind (at 2) args (*vm-env state))))]
      [(sexp-of-native? 'builtin f)
        (let1 builtin-run (vec-ref (sexp-repr f) 1)
          (builtin-run state args))]
      [else
        (result-reflect (failure-evaluation "Cannot call: " (sexp-inspect f)))])))

(defun vm-apply-never (state f args)
  (*vm-set-stack! state ())
  (*vm-set-code! state (list inst-leave))
  (*vm-set-dump! state ())
  (vm-apply state f args))

(defun vm-capture-cont (state)
  (let ([stack (*vm-stack state)]
        [env (*vm-env state)]
        [code (*vm-code state)]
        [dump (*vm-dump state)]
        [ret (fun (state v)
               (*vm-set-stack! state stack)
               (*vm-set-env! state env)
               (*vm-set-code! state code)
               (*vm-set-dump! state dump)
               (vm-push state v))])
    (sexp-native
      (native-builtin
        (fun (state args)
          (cond
            [(nil? args) (ret state sexp-nil)]
            [(nil? (cdr args)) (ret state (car args))]
            [else (vm-reflect (failure-evaluation "Multiple values are not implemented"))]))))))

(defun *vm-exec (ctx env code)
  (*vm-run (*vm-state () env code () ctx)))

(defun *vm-run (state)
  (if (nil? (*vm-code state))
    (vm-pop state)
    (let1 inst (car (*vm-code state))
      (*vm-set-code! state (cdr (*vm-code state)))
      (let1 r (*vm-run-inst state inst)
        (if (success? r)
          (*vm-run state)
          r)))))

(defun *vm-run-inst (state i)
  (let ([at (fun (index) (vec-ref i index))]
        [current-env (*vm-env state)]
        [push (fun (v) (vm-push state v))]
        [pop (fun () (result-reflect (vm-pop state)))])
    (result-reify
      (cond
        [(inst-of? op-ldc i)
          (push (at 1))]
        [(inst-of? op-ldv i)
          (push (result-reflect (env-get (at 1) current-env)))]
        [(inst-of? op-ldf i)
          (push (sexp-native (native-fun current-env (at 1) (at 2))))]
        [(inst-of? op-ldm i)
          (push (sexp-native (native-macro current-env (at 1) (at 2))))]
        [(inst-of? op-ldb i)
          (let1 builtin (tbl-find (vm-builtins (vm-context state)) (at 1))
            (when (nil? builtin)
              (result-reflect (failure-evaluation "Unsupported builtin: " (at 1))))
            (push (sexp-native (native-builtin builtin))))]
        [(inst-of? op-sel i)
          (let ([bool (pop)]
                [branch-code (if (sexp-test bool) (at 1) (at 2))])
            (vm-enter state (env current-env) branch-code))]
        [(inst-of? op-app i)
          (let ([args (foldl (fun (args _) (cons (pop) args)) () (iota 0 (at 1)))]
                [f (pop)])
            (result-reflect (vm-apply state f args)))]
        [(inst-of? op-leave i)
          (result-reflect (vm-leave state))]
        [(inst-of? op-pop i)
          (pop)]
        [(inst-of? op-def i)
          (let1 v (pop)
            (env-def! (at 1) v current-env))]
        [(inst-of? op-set i)
          (let1 v (pop)
            (result-reflect (env-set! (at 1) v current-env)))]))))

(defun *vm-macroexpand (recurse ctx s)
  (result-reify
    (let ([ls (sexp->list s)]
          [expand (fun (s) (result-reflect (*vm-macroexpand recurse ctx s)))]
          [expand-children (if recurse
                             (fun (s) (result-reflect (*vm-macroexpand-children ctx s)))
                             id)]
          [macroexpand-env (*vm-toplevel ctx)])
      (if (and (success? ls) (cons? (force-success ls)))
        (let ([m (car (force-success ls))]
              [args (cdr (force-success ls))]
              [macro-or-syntax (env-refer m macroexpand-env)])
          (cond
            ; (macro ...)
            [(and macro-or-syntax (sexp-of-native? 'macro macro-or-syntax))
              (let ([at (fun (i) (vec-ref (sexp-repr macro-or-syntax) i))]
                    [env (env (at 1))]
                    [_ (result-reflect (pattern-bind (at 2) args env))]
                    [s (result-reflect (*vm-exec ctx env (at 3)))])
                (if recurse (expand s) s))]
            ; (syntax ...)
            [(and macro-or-syntax (sexp-of-native? 'syntax macro-or-syntax))
              (if recurse
                (let1 syntax-expand (vec-ref (sexp-repr macro-or-syntax) 1)
                  (list->sexp (cons m (syntax-expand expand args))))
                s)]
            [else
              (expand-children s)]))
        (expand-children s)))))

(defun *vm-macroexpand-children (ctx s)
  (result-reify
    (let loop ([s s])
      (if (sexp-of? 'cons s)
        (let ([a (result-reflect (*vm-macroexpand #t ctx (car (sexp-repr s))))]
              [b (loop (cdr (sexp-repr s)))])
          (sexp-cons a b))
        s))))
