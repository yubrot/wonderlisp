(defun main (args)
  (let1 ctx (vm-create)
    (cond
      [(nil? args)
        (init ctx #t ())
        (repl ctx)]
      [(= (car args) "-test")
        (init ctx #f ())
        (map (partial run-test ctx) (cdr args))]
      [else
        (let ([files ()]
              [lisp-args ()]
              [args-started #f])
          (for args (fun (arg)
            (cond
              [args-started (set! lisp-args (cons arg lisp-args))]
              [(= arg "--") (set! args-started #t)]
              [else (set! files (cons arg files))])))
          (init ctx #t (reverse lisp-args))
          (map (partial exec-file ctx) (reverse files)))])))

(defun init (ctx boot args)
  (register-builtins args (vm-builtins ctx))
  (when boot
    (let1 bootcode (str->stream (force-success (open-read "rosetta-lisp/boot.lisp")))
      (force-success (exec ctx bootcode)))))

(defun repl (ctx)
  (write-line "[ocalisp REPL]" stderr)
  (let loop ([stream (port->stream stdin)])
    (write-str "> " stderr)
    (flush stderr)
    (let1 r (parse <sexp-repl> stream)
      (if (success? r)
        (let ([s (car (result r))]
              [next-stream (cdr (result r))]
              [r (vm-eval ctx s)])
          (if (success? r)
            (write-line (sexp-inspect (result r)) stdout)
            (write-line (result r) stderr))
          (loop next-stream))
        (begin
          (write-line (result r) stderr)
          (loop (port->stream stdin)))))))

(defun exec (ctx stream)
  (result-reify
    (let1 code (result-reflect (parse-just <program> stream))
      (for code (fun (s)
        (result-reflect (vm-eval ctx s)))))
    ()))

(defun exec-file (ctx file)
  (let1 r (exec ctx (str->stream (force-success (open-read file))))
    (or (success? r) (error (str-concat file ": " (result r))))))

(main args)
