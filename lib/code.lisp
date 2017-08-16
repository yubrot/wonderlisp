; <code> = (inst ...)

(defun code->str (code)
  (letrec ([block-id 0]
           [blocks ()]
           [inst->str (fun (i) (*inst->str add-code i))]
           [add-code (fun (header code)
                       (let ([id (str-concat "[" (num->str block-id) " " header "]")]
                             [buf (vec #f)])
                         (set! block-id (+ block-id 1))
                         (set! blocks (cons buf blocks))
                         (let ([a (str-concat id "\n")]
                               [b (map (fun (i) (str-concat "  " (inst->str i) "\n")) code)])
                           (vec-set! buf 0 (apply str-concat (cons a b)))
                           id)))])
    (add-code "entry" code)
    (apply str-concat (map (fun (block) (vec-ref block 0)) (reverse blocks)))))

(defun *inst->str (add-code inst)
  (let ([at (fun (i) (vec-ref inst i))]
        [c str-concat])
    (cond
      [(inst-of? op-ldc inst) (c "ldc " (sexp-inspect (at 1)))]
      [(inst-of? op-ldv inst) (c "ldv " (at 1))]
      [(inst-of? op-ldf inst) (c "ldf " (add-code (c "fun " (pattern->str (at 1))) (at 2)))]
      [(inst-of? op-ldm inst) (c "ldm " (add-code (c "macro " (pattern->str (at 1))) (at 2)))]
      [(inst-of? op-ldb inst) (c "ldb " (at 1))]
      [(inst-of? op-sel inst) (let ([a (add-code "then" (at 1))]
                                    [b (add-code "else" (at 2))])
                                (c "sel " a " " b))]
      [(inst-of? op-app inst) (c "app " (num->str (at 1)))]
      [(inst-of? op-leave inst) "leave"]
      [(inst-of? op-pop inst) "pop"]
      [(inst-of? op-def inst) (c "def " (at 1))]
      [(inst-of? op-set inst) (c "set " (at 1))]
      [else (error (c "Unknown opcode " (num->str (at 0))))])))
