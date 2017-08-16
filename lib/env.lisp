(defrecord env *env env?
  ([parent *env-parent]
   [current *env-current]))

(defun env (parent)
  (*env parent (tbl)))

(defun env-def! (k v env)
  (tbl-insert! (*env-current env) k v))

(defun env-set! (k v env)
  (cond
    [(tbl-contains? (*env-current env) k)
      (success (tbl-insert! (*env-current env) k v))]
    [(*env-parent env)
      (env-set! k v (*env-parent env))]
    [else
      (failure-evaluation "Undefined variable: " k)]))

(defun env-find (k env)
  (let1 x (tbl-find (*env-current env) k)
    (cond
      [(not (nil? x)) x]
      [(*env-parent env) (env-find k (*env-parent env))]
      [else #f])))

(defun env-get (k env)
  (let1 x (env-find k env)
    (if x
      (success x)
      (failure-evaluation "Undefined variable: " k))))

(defun env-refer (s env)
  (if (sexp-of? 'sym s)
    (env-find (sexp-repr s) env)
    #f))
