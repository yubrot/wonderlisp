(defun register-builtins (args tbl)
  (tbl-insert! tbl "cons" builtin-cons)

  (tbl-insert! tbl "exit" builtin-exit)
  (tbl-insert! tbl "error" builtin-error)

  (tbl-insert! tbl "gensym" (builtin-gensym-gen))

  (tbl-insert! tbl "car" builtin-car)
  (tbl-insert! tbl "cdr" builtin-cdr)

  (tbl-insert! tbl "apply" builtin-apply)

  (tbl-insert! tbl "num?" (builtin-test-gen "num?" 'num))
  (tbl-insert! tbl "sym?" (builtin-test-gen "sym?" 'sym))
  (tbl-insert! tbl "str?" (builtin-test-gen "str?" 'str))
  (tbl-insert! tbl "cons?" (builtin-test-gen "cons?" 'cons))
  (tbl-insert! tbl "nil?" (builtin-test-gen "nil?" 'nil))
  (tbl-insert! tbl "bool?" (builtin-test-gen "bool?" 'bool))
  (tbl-insert! tbl "proc?" (builtin-test-native-gen "proc?" '(fun builtin)))
  (tbl-insert! tbl "meta?" (builtin-test-native-gen "meta?" '(macro syntax)))
  (tbl-insert! tbl "port?" (builtin-test-native-gen "port?" '(port)))
  (tbl-insert! tbl "vec?" (builtin-test-native-gen "vec?" '(vec)))

  (tbl-insert! tbl "+" (builtin-arithmetic-gen "+" + 0))
  (tbl-insert! tbl "-" (builtin-arithmetic-gen "-" - #f))
  (tbl-insert! tbl "*" (builtin-arithmetic-gen "*" * 1))
  (tbl-insert! tbl "/" (builtin-arithmetic-gen "*" / #f))
  (tbl-insert! tbl "%" (builtin-arithmetic-gen "%" % #f))

  (tbl-insert! tbl "=" builtin-eq)
  (tbl-insert! tbl "<" (builtin-compare-gen "<" <))
  (tbl-insert! tbl ">" (builtin-compare-gen ">" >))
  (tbl-insert! tbl "<=" (builtin-compare-gen "<=" <=))
  (tbl-insert! tbl ">=" (builtin-compare-gen ">=" >=))

  (tbl-insert! tbl "call/cc" builtin-call/cc)
  (tbl-insert! tbl "never" builtin-never)

  (tbl-insert! tbl "str" builtin-str)
  (tbl-insert! tbl "str-ref" builtin-str-ref)
  (tbl-insert! tbl "str-bytesize" builtin-str-bytesize)
  (tbl-insert! tbl "str-concat" builtin-str-concat)
  (tbl-insert! tbl "substr" builtin-substr)
  (tbl-insert! tbl "sym->str" builtin-sym->str)
  (tbl-insert! tbl "num->str" builtin-num->str)
  (tbl-insert! tbl "str->num" builtin-str->num)

  (tbl-insert! tbl "vec" builtin-vec)
  (tbl-insert! tbl "vec-make" builtin-vec-make)
  (tbl-insert! tbl "vec-ref" builtin-vec-ref)
  (tbl-insert! tbl "vec-length" builtin-vec-length)
  (tbl-insert! tbl "vec-set!" builtin-vec-set!)
  (tbl-insert! tbl "vec-copy!" builtin-vec-copy!)

  (tbl-insert! tbl "open" builtin-open)
  (tbl-insert! tbl "close" builtin-close)

  (tbl-insert! tbl "stdin" builtin-stdin)
  (tbl-insert! tbl "stdout" builtin-stdout)
  (tbl-insert! tbl "stderr" builtin-stderr)

  (tbl-insert! tbl "read-byte" builtin-read-byte)
  (tbl-insert! tbl "read-str" builtin-read-str)
  (tbl-insert! tbl "read-line" builtin-read-line)

  (tbl-insert! tbl "write-byte" builtin-write-byte)
  (tbl-insert! tbl "write-str" builtin-write-str)
  (tbl-insert! tbl "write-line" builtin-write-line)
  (tbl-insert! tbl "flush" builtin-flush)

  (tbl-insert! tbl "args" (builtin-args-gen args))

  (tbl-insert! tbl "eval" builtin-eval)
  (tbl-insert! tbl "macroexpand" (builtin-macroexpand-gen "macroexpand" #t))
  (tbl-insert! tbl "macroexpand-1" (builtin-macroexpand-gen "macroexpand-1" #f)))

(defmacro builtin-let (name params . body)
  `(begin
     (unless (= (list-count args) ,(list-count params))
       (vm-reflect (failure-evaluation ,name " takes " ,(num->str (list-count params)) " arguments")))
     (let ,(list-zip-with (partial builtin-take-arg name) params (iota 0 (list-count params)))
       ,@body)))

(defun builtin-take-arg (builtin-name param index)
  (cond
    [(sym? param)
      `(,param (nth ,index args))]
    [(and (cons? param) (= 3 (list-count param)))
      (let ([name (car param)]
            [type (cadr param)]
            [description (caddr param)])
        `(,name (let1 v (nth ,index args)
                  (if (sexp-of? ',type v)
                    (sexp-repr v)
                    (vm-reflect (failure-evaluation ,builtin-name ": Expected " ,description " but got " (sexp-inspect v)))))))]
    [(and (cons? param) (= 4 (list-count param)))
      (let ([name (car param)]
            [native-type (cadr param)]
            [native-ref (caddr param)]
            [description (car (cdddr param))])
        `(,name (let1 v (nth ,index args)
                  (if (sexp-of-native? ',native-type v)
                    (,native-ref (sexp-repr v))
                    (vm-reflect (failure-evaluation ,builtin-name ": Expected " ,description " but got " (sexp-inspect v)))))))]
    [else
      (error "Syntax error: builtin-take-arg expects 'name or '(name type description) or '(name native-type native-ref description)")]))

(defun *vec-payload (v) (vec-ref v 1))

(defun *port-in (v) (or (vec-ref v 1) (vm-reflect (failure-evaluation "port is not available for reading"))))
(defun *port-out (v) (or (vec-ref v 2) (vm-reflect (failure-evaluation "port is not available for writing"))))
(defun *port-any (v) v)

(defun builtin-cons (state args)
  (builtin-let "cons" (a b)
    (vm-push state (sexp-cons a b))))

(defun builtin-exit (state args)
  (if (nil? args)
    (exit 0)
    (builtin-let "exit" ([exitcode num "exitcode"])
      (exit exitcode))))

(defun builtin-error (state args)
  (if (nil? args)
    (vm-reflect (failure-evaluation "error called"))
    (builtin-let "error" ([msg str "error message"])
      (vm-reflect (failure-evaluation msg)))))

(defun builtin-gensym-gen ()
  (let1 id 0
    (fun (state args)
      (builtin-let "gensym" ()
        (set! id (+ id 1))
        (vm-push state (sexp-sym (str-concat "#sym." (num->str id))))))))

(defun builtin-car (state args)
  (builtin-let "car" ([a cons "cons"])
    (vm-push state (car a))))

(defun builtin-cdr (state args)
  (builtin-let "cdr" ([a cons "cons"])
    (vm-push state (cdr a))))

(defun builtin-apply (state args)
  (builtin-let "apply" (f args)
    (let1 ls (sexp->list args)
      (if (success? ls)
        (vm-apply state f (force-success ls))
        (vm-reflect (failure-evaluation "apply: Expected argument list but got " (sexp-inspect args)))))))

(defun builtin-test-gen (name type)
  (fun (state args)
    (builtin-let name (a)
      (vm-push state (sexp-bool (sexp-of? type a))))))

(defun builtin-test-native-gen (name types)
  (fun (state args)
    (builtin-let name (a)
      (vm-push state (sexp-bool (any (fun (t) (sexp-of-native? t a)) types))))))

(defun builtin-arithmetic-gen (name op id)
  (fun (state args)
    (if (nil? args)
      (if id
        (vm-push state (sexp-num id))
        (vm-reflect (failure-evaluation name " takes at least one argument")))
      (if (all (partial sexp-of? 'num) args)
        (vm-push state (sexp-num (apply op (map sexp-repr args))))
        (vm-reflect (failure-evaluation name " takes number arguments"))))))

(defun builtin-eq (state args)
  (if (nil? args)
    (vm-push state sexp-true)
    (vm-push state (sexp-bool (all (partial *builtin-eq (car args)) (cdr args))))))

(defun *builtin-eq (a b)
  (if (= (sexp-tag a) (sexp-tag b))
    (cond
      [(or (= (sexp-tag a) 'num)
           (= (sexp-tag a) 'sym)
           (= (sexp-tag a) 'str)
           (= (sexp-tag a) 'bool)) (= (sexp-repr a) (sexp-repr b))]
      [(= (sexp-tag a) 'cons) (and (*builtin-eq (car (sexp-repr a)) (car (sexp-repr b)))
                                   (*builtin-eq (cdr (sexp-repr a)) (cdr (sexp-repr b))))]
      [(= (sexp-tag a) 'nil) #t]
      [else #f])
    #f))

(defun builtin-compare-gen (name op)
  (fun (state args)
    (if (nil? args)
      (vm-push state sexp-true)
      (let ([a (car args)]
            [b (cdr args)])
        (cond
          [(sexp-of? 'num a)
            (unless (all (partial sexp-of? 'num) b)
              (vm-reflect (failure-evaluation name ": Expected number arguments")))]
          [(sexp-of? 'str a)
            (unless (all (partial sexp-of? 'str) b)
              (vm-reflect (failure-evaluation name ": Expected string arguments")))]
          [else
            (vm-reflect (failure-evaluation name " is only defined for strings or numbers"))])
        (let loop ([a (sexp-repr a)]
                   [b (map sexp-repr b)])
          (cond
            [(nil? b) (vm-push state sexp-true)]
            [(op a (car b)) (loop (car b) (cdr b))]
            [else (vm-push state sexp-false)]))))))

(defun builtin-call/cc (state args)
  (builtin-let "call/cc" (f)
    (let1 cont (vm-capture-cont state)
      (vm-apply state f (list cont)))))

(defun builtin-never (state args)
  (builtin-let "never" (f)
    (vm-apply-never state f ())))

(defun builtin-str (state args)
  (unless (all (partial sexp-of? 'num) args)
    (vm-reflect (failure-evaluation "str: Expected number arguments")))
  (let1 bytes (map sexp-repr args)
    (unless (all (fun (s) (<= 0 s 255)) bytes)
      (vm-reflect (failure-evaluation "str: Each byte of string must be inside the range 0-255")))
    (vm-push state (sexp-str (apply str bytes)))))

(defun builtin-str-ref (state args)
  (builtin-let "str-ref" ([str str "string"]
                          [index num "index"])
    (vm-push state (sexp-option sexp-num (str-ref str index)))))

(defun builtin-str-bytesize (state args)
  (builtin-let "str-bytesize" ([str str "string"])
    (vm-push state (sexp-num (str-bytesize str)))))

(defun builtin-str-concat (state args)
  (unless (all (partial sexp-of? 'str) args)
    (vm-reflect (failure-evaluation "str-concat: Expected string arguments")))
  (vm-push state (sexp-str (apply str-concat (map sexp-repr args)))))

(defun builtin-substr (state args)
  (builtin-let "substr" ([s str "string"]
                         [i num "index"]
                         [l num "size"])
    (unless (<= 0 i (+ i l) (str-bytesize s))
      (vm-reflect (failure-evaluation "substr: Index out of range")))
    (vm-push state (sexp-str (substr s i l)))))

(defun builtin-sym->str (state args)
  (builtin-let "sym->str" ([s sym "symbol"])
    (vm-push state (sexp-str s))))

(defun builtin-num->str (state args)
  (builtin-let "num->str" ([n num "number"])
    (vm-push state (sexp-str (num->str n)))))

(defun builtin-str->num (state args)
  (builtin-let "str->num" ([s str "string"])
    (vm-push state (sexp-option sexp-num (str->num s)))))

(defun builtin-vec (state args)
  (vm-push state (sexp-native (native-vec (apply vec args)))))

(defun builtin-vec-make (state args)
  (builtin-let "vec-make" ([l num "length"]
                           init)
    (vm-push state (sexp-native (native-vec (vec-make l init))))))

(defun builtin-vec-ref (state args)
  (builtin-let "vec-ref" ([v vec *vec-payload "vector"]
                          [i num "index"])
    (vm-push state (sexp-option id (vec-ref v i)))))

(defun builtin-vec-length (state args)
  (builtin-let "vec-length" ([v vec *vec-payload "vector"])
    (vm-push state (sexp-num (vec-length v)))))

(defun builtin-vec-set! (state args)
  (builtin-let "vec-set!" ([v vec *vec-payload "vector"]
                           [i num "index"]
                           item)
    (unless (<= 0 i (vec-length v))
      (vm-reflect (failure-evaluation "vec-set!: Index out of range")))
    (vec-set! v i item)
    (vm-push state sexp-nil)))

(defun builtin-vec-copy! (state args)
  (builtin-let "vec-copy!" ([dest vec *vec-payload "destination vector"]
                            [dest-start num "destination index"]
                            [src vec *vec-payload "source vector"]
                            [src-start num "source index"]
                            [length num "length"])
    (unless (and (<= 0 src-start (+ src-start length) (vec-length src))
                 (<= 0 dest-start (+ dest-start length) (vec-length dest)))
      (vm-reflect (failure-evaluation "vec-copy! Index out of range")))
    (vec-copy! dest dest-start src src-start length)
    (vm-push state sexp-nil)))

(defun builtin-open (state args)
  (builtin-let "open" ([filepath str "filepath"]
                       [mode str "mode"])
    (unless (or (= mode "r") (= mode "w"))
      (vm-reflect (failure-evaluation "open: Unsupported mode for open: " mode)))
    (let ([port (open filepath mode)]
          [native-port (fun (p)
                         (if (= mode "r")
                           (sexp-native (native-port p #f))
                           (sexp-native (native-port #f p))))])
      (vm-push state (sexp-result native-port port)))))

(defun builtin-close (state args)
  (builtin-let "close" ([p port *port-any "port"])
    (cond
      [(vec-ref p 1)
        (vm-push state (sexp-result sexp (close (vec-ref p 1))))
        (vec-set! p 1 #f)]
      [(vec-ref p 2)
        (vm-push state (sexp-result sexp (close (vec-ref p 2))))
        (vec-set! p 2 #f)]
      [else
        (vm-push state (sexp-result sexp (success ())))])))

(defun builtin-stdin (state args)
  (builtin-let "stdin" ()
    (vm-push state (sexp-native (native-port stdin #f)))))

(defun builtin-stdout (state args)
  (builtin-let "stdout" ()
    (vm-push state (sexp-native (native-port #f stdout)))))

(defun builtin-stderr (state args)
  (builtin-let "stderr" ()
    (vm-push state (sexp-native (native-port #f stderr)))))

(defun builtin-read-byte (state args)
  (builtin-let "read-byte" ([p port *port-in "port"])
    (vm-push state (sexp-result sexp (read-byte p)))))

(defun builtin-read-str (state args)
  (builtin-let "read-str" ([size num "size"]
                            [p port *port-in "port"])
    (vm-push state (sexp-result sexp (read-str size p)))))

(defun builtin-read-line (state args)
  (builtin-let "read-line" ([p port *port-in "port"])
    (vm-push state (sexp-result sexp (read-line p)))))

(defun builtin-write-byte (state args)
  (builtin-let "write-byte" ([byte num "byte"]
                             [p port *port-out "port"])
    (vm-push state (sexp-result sexp (write-byte byte p)))))

(defun builtin-write-str (state args)
  (builtin-let "write-str" ([str str "string"]
                            [p port *port-out "port"])
    (vm-push state (sexp-result sexp (write-str str p)))))

(defun builtin-write-line (state args)
  (builtin-let "write-line" ([str str "string"]
                             [p port *port-out "port"])
    (vm-push state (sexp-result sexp (write-line str p)))))

(defun builtin-flush (state args)
  (builtin-let "flush" ([p port *port-out "port"])
    (vm-push state (sexp-result sexp (flush p)))))

(defun builtin-args-gen (env-args)
  (fun (state args)
    (builtin-let "args" ()
      (vm-push state (list->sexp (map sexp-str env-args))))))

(defun builtin-eval (state args)
  (builtin-let "eval" (s)
    (vm-push state (sexp-result id (vm-eval (vm-context state) s)))))

(defun builtin-macroexpand-gen (name recurse)
  (fun (state args)
    (builtin-let name (s)
      (vm-push state (sexp-result id (vm-macroexpand recurse (vm-context state) s))))))
