(def <>
  (let ([<spaces> (ps-str-while (char-class " \t\n"))]
        [<comment> (p-seq (ps-str ";") (ps-str-while (char-class "^\n")))])
    (p-many (p-or <spaces> <comment>))))

(defun *entry (p)
  (p-reduce (fun (_ x) x) <> p))

(defun *lex (p)
  (p-reduce (fun (x _) x) p <>))

(def <program>
  (p-lazy (*entry (p-many (*lex <s>)))))

(def <sexp>
  (p-lazy (*entry (*lex <s>))))

(def <sexp-repl>
  (p-lazy (*entry <s>)))

(def <s>
  (p-lazy
    (let ([<cons> (p-map caddr (p-or (p-seq (ps-str "(") <> <s-inner> <> (ps-str ")"))
                                     (p-seq (ps-str "[") <> <s-inner> <> (ps-str "]"))))]
          [<num> (p-map sexp-num <number>)]
          [<sym> (p-map sexp-sym <symbol>)]
          [<str> (p-map sexp-str <string>)]
          [<t> (p-map (const sexp-true)  (ps-str "#t"))]
          [<f> (p-map (const sexp-false) (ps-str "#f"))])
      (p-choice <cons> <s-quoted> <num> <sym> <str> <t> <f>))))

(def <s-quoted>
  (p-lazy
    (let ([gen (fun (f s) (p-map (compose f caddr) (p-seq (ps-str s) <> <s>)))]
          [<quote> (gen sexp-quote "'")]
          [<quasiquote> (gen sexp-quasiquote "`")]
          [<unquote> (gen sexp-unquote ",")]
          [<unquote-splicing> (gen sexp-unquote-splicing ",@")])
      (p-choice <quote> <quasiquote> <unquote-splicing> <unquote>))))

(def <s-inner>
  (p-lazy
    (let ([<term> (p-or (p-map caddr (p-seq (ps-str ".") <> <s>))
                        (p-unit sexp-nil))]
          [<ls> (p-some (*lex <s>))]
          [list-like (fun (xs s) (foldr sexp-cons s xs))]
          [<one-or-more> (p-reduce list-like <ls> <term>)]
          [<zero> (*lex (p-unit sexp-nil))])
      (p-or <one-or-more> <zero>))))

(def *letter "A-Za-z")
(def *digit "0-9")
(def *special "-!$%&*+/:<=>?@^_~")

(def <number>
  (p-lazy
    (let ([? (fun (p) (p-or p (p-unit "")))]
          [ch (fun (s) (ps-char-if (char-class s)))]
          [<digits> (ps-str-while (char-class *digit))]
          [<frac> (p-reduce str-concat (ps-str ".") (? <digits>))]
          [<exp> (p-reduce str-concat (ch "eE") (? (ch "-+")) <digits>)])
      (p-map str->num (p-reduce str-concat (? (ch "-+")) <digits> (? <frac>) (? <exp>))))))

(def <symbol>
  (p-lazy
    (let ([<first> (ps-str-while (char-class (str-concat *letter *special)))]
          [<rest> (ps-str-while (char-class (str-concat *letter *digit *special)))]
          [not-dot? (fun (s) (not (= s ".")))])
      (p-where not-dot? (p-reduce str-concat <first> (p-or <rest> (p-unit "")))))))

(def <string>
  (p-lazy
    (let ([<a> (ps-str-while (char-class "^\\\""))]
          [<escape-sequence> (p-reduce str-concat (ps-str "\\") (ps-char-if (char-class "\\tn\"")))]
          [<text> (p-map (fun (ls) (apply str-concat ls)) (p-many (p-or <a> <escape-sequence>)))])
      (p-map (compose str-unescape cadr) (p-seq (ps-str "\"") <text> (ps-str "\""))))))
