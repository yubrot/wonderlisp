; <sexp> = (tag . repr)
(def sexp-tag car)
(def sexp-repr cdr)

(defun sexp-num (x) (cons 'num x))
(defun sexp-sym (x) (cons 'sym x))
(defun sexp-str (x) (cons 'str x))
(defun sexp-cons (x y) (cons 'cons (cons x y)))
(defun sexp-bool (x) (cons 'bool x))
(defun sexp-native (x) (cons 'native x))

(def sexp-nil (cons 'nil ()))
(def sexp-true (sexp-bool #t))
(def sexp-false (sexp-bool #f))

(defun sexp-option (constructor x)
  (if (nil? x)
    sexp-nil
    (constructor x)))

(defun sexp-result (constructor x)
  (if (failure? x)
    (sexp-cons sexp-false (sexp-str (result x)))
    (sexp-cons sexp-true (constructor (result x)))))

(defun sexp-of? (t s)
  (= t (sexp-tag s)))

(defun sexp-of-native? (t s)
  (and (sexp-of? 'native s)
       (native-of? t (sexp-repr s))))

; Convert host S expression into VM sexp representation
(defun sexp (x)
  (cond
    [(num? x) (sexp-num x)]
    [(sym? x) (sexp-sym (sym->str x))]
    [(str? x) (sexp-str x)]
    [(cons? x) (sexp-cons (sexp (car x))
                          (sexp (cdr x)))]
    [(nil? x) sexp-nil]
    [(bool? x) (sexp-bool x)]
    [else (error)]))

(defun list->sexp (ls)
  (foldr sexp-cons sexp-nil ls))

(defun sexp->list (s)
  (let/cc return
    (success
      (let loop ([s s])
        (cond
          [(sexp-of? 'nil s) ()]
          [(sexp-of? 'cons s) (cons (car (sexp-repr s))
                                    (loop (cdr (sexp-repr s))))]
          [else (return (failure ()))])))))

(defun sexp-test (s)
  (if (sexp-of? 'bool s) (sexp-repr s) #t))

(defun sexp-quote (x)
  (list->sexp (list (sexp 'quote) x)))
(defun sexp-quasiquote (x)
  (list->sexp (list (sexp 'quasiquote) x)))
(defun sexp-unquote (x)
  (list->sexp (list (sexp 'unquote) x)))
(defun sexp-unquote-splicing (x)
  (list->sexp (list (sexp 'unquote-splicing) x)))

(def sexp-syntax-sugar
  '(("quote" . "'")
    ("quasiquote" . "`")
    ("unquote" . ",")
    ("unquote-splicing" . ",@")))

; (sexp) => str
(defun sexp-inspect (s)
  (cond
    [(sexp-of? 'num s) (num->str (sexp-repr s))]
    [(sexp-of? 'sym s) (sexp-repr s)]
    [(sexp-of? 'str s) (str-concat "\"" (str-escape (sexp-repr s)) "\"")]
    [(sexp-of? 'cons s) (let ([l (car (sexp-repr s))]
                              [r (cdr (sexp-repr s))]
                              [a (list-lookup (sexp-repr l) sexp-syntax-sugar)])
                          (if (and (= 'sym (sexp-tag l))
                                   (not (nil? a))
                                   (= 'cons (sexp-tag r))
                                   (= 'nil (sexp-tag (cdr (sexp-repr r)))))
                            (str-concat a (sexp-inspect (car (sexp-repr r))))
                            (str-concat "(" (cons-inspect (sexp-repr s)) ")")))]
    [(sexp-of? 'nil s) "()"]
    [(sexp-of? 'bool s) (if (sexp-repr s) "#t" "#f")]
    [(sexp-of? 'native s) (native-inspect (sexp-repr s))]
    [else (error (str-concat "Unknown sexp tag: " (sym->str (sexp-tag s))))]))

; ((sexp . sexp)) => str
(defun cons-inspect (c)
  (let ([a (car c)]
        [b (cdr c)])
    (cond
      [(= 'nil (sexp-tag b)) (sexp-inspect a)]
      [(= 'cons (sexp-tag b)) (str-concat (sexp-inspect a) " " (cons-inspect (sexp-repr b)))]
      [else (str-concat (sexp-inspect a) " . " (sexp-inspect b))])))
