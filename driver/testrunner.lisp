(defun run-test (ctx test)
  (map (partial run-testcase ctx) (parse-testcases test)))

; => (((header . command) inputs...) ...)
(defun parse-testcases (file)
  (let1 port (force-success (open file "r"))
    (let loop ([testcases ()])
      (let1 header (force-success (read-line port))
        (cond
          [(= 'eof header)
            (force-success (close port))
            (reverse testcases)]
          [else
            (let ([command-str (force-success (read-line port))]
                  [command (force-success (parse-just <test-command> (str->stream command-str)))]
                  [command-type (car command)]
                  [command-inputs (cdr command)]
                  [testcase (list header
                                  command-type
                                  (map (fun (n) (read-lines n port)) command-inputs))])
              (loop (cons testcase testcases)))])))))

(defun run-testcase (ctx testcase)
  (let ([header (car testcase)]
        [command (cadr testcase)]
        [inputs (caddr testcase)]
        [_ (println "Running test " header "...")]
        [result (run-command ctx command inputs)])
    (when (failure? result)
      (println "Test failed at " header ": " (force-failure result))
      (exit 1))))

(defun run-command (ctx command inputs)
  (result-reify
    (cond
      [(= command 'parse-success)
        (let ([i (car inputs)]
              [o (cadr inputs)]
              [r (result-reflect (parse-just <sexp> (str->stream i)))])
          (result-reflect (check-diff (sexp-inspect r) o)))]
      [(= command 'parse-failure)
        (let ([i (car inputs)]
              [r (parse-just <s> (str->stream i))])
          (unless (failure? r)
            (result-reflect (failure (str-concat "Parse succeeded: " (sexp-inspect (result r)))))))]
      [(= command 'compile-success)
        (let ([i (car inputs)]
              [o (cadr inputs)]
              [s (result-reflect (parse-just <sexp> (str->stream i)))]
              [r (result-reflect (vm-compile ctx s))])
          (result-reflect (check-diff (code->str r) o)))]
      [(= command 'compile-failure)
        (let ([i (car inputs)]
              [s (result-reflect (parse-just <sexp> (str->stream i)))]
              [r (vm-compile ctx s)])
          (unless (failure? r)
            (result-reflect (failure (str-concat "Compile succeeded:\n" (code->str (result r)))))))]
      [(= command 'eval-success)
        (let ([i (car inputs)]
              [o (cadr inputs)]
              [s (result-reflect (parse-just <sexp> (str->stream i)))]
              [r (result-reflect (vm-eval ctx s))])
          (result-reflect (check-diff (sexp-inspect r) o)))]
      [(= command 'eval-failure)
        (let ([i (car inputs)]
              [s (result-reflect (parse-just <sexp> (str->stream i)))]
              [r (vm-eval ctx s)])
          (unless (failure? r)
            (result-reflect (failure (str-concat "Eval succeeded: " (sexp-inspect (result r)))))))]
      [(= command 'eval-all)
        (let ([i (car inputs)]
              [program (result-reflect (parse-just <program> (str->stream i)))])
          (for program
            (fun (s) (result-reflect (vm-eval ctx s)))))]
      [else
        (error (str-concat "Unknown command: " (sym->str command)))])))

(def <test-command>
  (let ([one (fun (name sym) (p-reduce (fun (_ _ a) (list sym a)) (ps-str name) <> <number>))]
        [two (fun (name sym) (p-reduce (fun (_ _ a _ b) (list sym a b)) (ps-str name) <> <number> <> <number>))])
  (p-choice (two "PARSE_SUCCESS" 'parse-success)
            (one "PARSE_FAILURE" 'parse-failure)
            (two "COMPILE_SUCCESS" 'compile-success)
            (one "COMPILE_FAILURE" 'compile-failure)
            (two "EVAL_SUCCESS" 'eval-success)
            (one "EVAL_FAILURE" 'eval-failure)
            (one "EVAL_ALL" 'eval-all))))

(defun read-lines (n port)
  (let loop ([buf ""]
             [n n])
    (if (= 0 n)
      buf
      (let1 line (force-success (read-line port))
        (when (= 'eof line)
          (error (str-concat "Failed to read line: " (num->str n) " more lines are required")))
        (loop (str-concat buf line "\n") (- n 1))))))

(defun check-diff (i o)
  (if (= (str-trim i) (str-trim o))
    (success ())
    (failure i)))
