;;; SchAPL - simple APL interpreter written in Scheme

; todo
; * redo lexer, define better what is valid syntax
;   (e.g. '# plus' and '3 a b c' is a valid now)
; * implement more monadic and dyadic operators
; * implement reduce and scan operators

(define pi (* 4 (atan 1)))
(define (error str) (begin (display str) '()))
(define (dot f g) (lambda (x) (f (g x))))
(define (swap op) (lambda (a b) (op b a)))
(define (curry op a) (lambda (b) (op a b)))
(define (string-car str) (string-ref str 0))
(define (string-cdr str) (string-tail str 1))
(define (dot-product x y)
  (cond
    ((not (= (length x) (length y))) (error "dot-product: vectors must be of same length"))
    ((null? x) 0)
    (else (+ (* (car x) (car y)) (dot-product (cdr x) (cdr y))))))
(define (first-digit nustr) (char->digit (string-car nustr)))
(define (liftify f) (lambda (mx) (f (first mx))))
(define (liftify2 f) (lambda (mx my) (f (first mx) (first my))))
(define (string-take-until str charset)
  (string-head str
    (let ((nextchar (string-find-next-char-in-set str charset)))
      (if nextchar nextchar (string-length str)))))
(define (logt base x) (/ (log x) (log base)))
(define (to-predicate op) (lambda (a b) (to-our-boolean (op a b))))
(define (from-our-boolean x)
  (cond
    ((= x 1) #t)
    ((= x 0) #f)
    (else (error "invalid boolean value, must be 0 or 1"))))
(define (to-our-boolean x)
  (cond
    ((eq? x #t) 1)
    ((eq? x #f) 0)
    (else (error "invalid native boolean value, must be #f or #t"))))

(define (string-take-until-including str charset)
  (string-head str
    (let ((nextchar (string-find-next-char-in-set str charset)))
      (if nextchar (1+ nextchar) (string-length str)))))

(define (operator? str) (char=? (string-car str) #\#))
(define (left-parenthesis? str) (char=? (string-car str) #\())
(define (strip-parentheses parstr) (substring parstr 1 (-1+ (string-length parstr))))

; all dyadic functions must take two lists
(define (find-index lst elems)
  (map
    (lambda (elem)
      (let loop ((index 0))
        (cond
          ((>= index (length lst)) (error "element not in list"))
          ((eq? (list-ref lst index) elem)  index)
          (else (loop (1+ index))))))
    elems))

(define (reader str)
  ((cond
    ((left-parenthesis? str) read-parentheses)
    ((operator? str)         read-op)
    (else                    read-value)) str))

(define (read-value str)
  (string-take-until str (string->char-set "#")))

(define (read-parentheses str)
  (string-take-until-including str (string->char-set ")")))

(define (read-op str)
  (if (char=? (string-car str) #\#)
    (string-append "#" (string-take-until (string-cdr str) (char-set-union char-set:numeric (string->char-set " #"))))
    (error "operator must be begin with hash sign (#)")))

(define (lex expr)
  (if (string-null? expr)
    '()
    (let* ((tok (reader expr)))
      (cons (string-trim tok) (lex (string-trim (string-tail expr (string-length tok))))))))

(define (parse-number nustr)
  ((char=? (string-car nustr) #\_) (* -1 (parse-number (string-cdr nustr)))))

; both functions expect a string of digits
(define (parse-integer nustr)
  (cond
    ((string-null? nustr) 0)
    (else
      (+ (* (expt 10 (-1+ (string-length nustr))) (first-digit nustr))
         (parse-number (string-cdr nustr))))))

(define (parse-fraction nustr)
  (let loop ((str (reverse nustr)))
    (cond
      ((string-null? nustr) 0)
      (else
        (+ (* (expt 10 (* -1 (string-length str))) (first-digit str))
           (loop (string-cdr nustr)))))))


(define (tokenize-array arrstr)
  (if (string-null? arrstr)
    '()
    (let* ((elem (string-take-until arrstr char-set:whitespace)))
      (cons (string-trim elem) (tokenize-array (string-trim (string-tail arrstr (string-length elem))))))))

(define (parse-array arrstr)
; (map parse-number (tokenize-array arrstr)))
  (map string->number (tokenize-array arrstr)))

(define (parse-value vars valstr)
  (cond
    ((char-in-set? (string-car valstr) char-set:alphabetic) (substitute vars (intern valstr)))
    (else (parse-array valstr))))

(define (apply-binary op arg1 arg2)
  (cond
    ((and (null? arg1) (null? arg2)) '())
    ((= (length arg1) (length arg2))
      (cons (op (car arg1) (car arg2)) (apply-binary op (cdr arg1) (cdr arg2))))
    ((= (length arg1) 1)
      (map (curry op (car arg1)) arg2))
    ((= (length arg2) 1)
      (map (curry (swap op) (car arg2)) arg1))
    (else
      (error "vectors must be the same length or at least one a singlet"))))

(define (parse-op opstr)
  (symbol-append 'op- (intern (string-cdr opstr))))
    ;  (if (eqv? (string-car opstr) #\#)
            ;    (symbol-append 'op- (intern (string-cdr opstr))))
    ;    (error (string-append "invalid operator: \"" opstr "\"" )))

(define (signum a)
  (cond
    ((< a 0) -1)
    ((= a 0) 0)
    ((> a 0) 1)))

(define (fac n)
  (if (< n 2) 1 (* n (fac (- n 1)))))

(define (nCr n k) (/ (fac n) (* (fac k) (fac (- n k)))))

(define (circle type theta)
  (case type
    ((1) (sin theta))
    ((2) (cos theta))
    ((3) (tan theta))
    ((4) (sinh theta))
    ((5) (cosh theta))
    ((6) (tanh theta))
    ((-1) (asin theta))
    ((-2) (acos theta))
    ((-3) (atan theta))
    ((-4) (asinh theta))
    ((-5) (acosh theta))
    ((-6) (atanh theta))
    (else (error "circle error"))))

(define (deal count range) (list-tabulate count (lambda (i) (random-integer range))))

(define (membership lst1 lst2)
  (map (lambda (elem) (if (member elem lst2) 1 0))
    lst1))

(define (our-take x li)
(let ((i (car li)))
  (cond
    ((> (abs i) (length x)) (error "our-take error"))
    ((positive? i) (take x i))
    ((negative? i) (drop x (+ (length x) i))))))

(define (our-drop x i)
(let ((i (car li)))
  (cond
    ((> (abs i) (length x)) (error "our-drop error"))
    ((> i 0) (drop x i))
    ((<= i 0) (take x (+ (length x) i))))))

(define (decode base coefficients)
  (dot-product
    coefficients
    (map (lambda (x) (expt base x)) (reverse (iota (length coefficients))))))

(define (encode base value)
  (reverse
    (let loop ((v value))
      (cond
        ((= v 0) '())
        (else (cons (remainder v base) (loop (quotient v base))))))))

(define (our-or x y)
  (let
    ((a (from-our-boolean x))
     (b (from-our-boolean y)))
    (to-our-boolean (boolean/or a b))))

(define (our-and x y)
  (let
    ((a (from-our-boolean x))
     (b (from-our-boolean y)))
    (to-our-boolean (boolean/and a b))))

(define (our-reduce op arg)
  (fold op org))

(define (operator-template name monadic dyadic args)
  (cond
    ((and (= (length args) 1) monadic) (monadic (first args)))
    ((and (= (length args) 2) dyadic) (dyadic (first args) (second args)))
    (else (error (string-append name " error")))))

(define (arithmetic-template name monadic dyadic args)
  (operator-template name (lambda (x) (map monadic x)) (lambda (x y) (apply-binary dyadic x y)) args))

(define (op-mul . args) (arithmetic-template "mul" signum * args))
(define (op-add . args) (arithmetic-template "add" car + args))
(define (op-sub . args) (arithmetic-template "sub" (curry * -1) - args))
(define (op-div . args) (arithmetic-template "div" (curry / 1) / args))
(define (op-ban . args) (arithmetic-template "ban" fac nCr args))
(define (op-iot . args) (operator-template "iot"   (liftify iota) find-index args))
(define (op-cir . args) (arithmetic-template "cir" (curry * pi) circle args))
(define (op-dea . args) (arithmetic-template "dea" random-integer deal args))
(define (op-abs . args) (arithmetic-template "abs" abs modulo args))
(define (op-exp . args) (arithmetic-template "exp" exp expt args))
(define (op-exp . args) (arithmetic-template "exp" log logt args))
(define (op-cei . args) (arithmetic-template "cei" ceiling max args))
(define (op-flo . args) (arithmetic-template "flo" floor min args))

(define (op-le . args) (arithmetic-template "le" #f (to-predicate <) args))
(define (op-leq . args) (arithmetic-template "le" #f (to-predicate <=) args))
(define (op-eq . args) (arithmetic-template "eq" #f (to-predicate =) args))
(define (op-geq . args) (arithmetic-template "geq" #f (to-predicate >=) args))
(define (op-ge . args) (arithmetic-template "ge" #f (to-predicate >) args))
(define (op-neq . args) (arithmetic-template "neq" #f (to-predicate (lambda (x y) (not (= x y)))) args))

(define (op-or . args) (arithmetic-template "or" #f our-or args))
(define (op-and . args) (arithmetic-template "and" #f our-and args))

(define (op-mem . args) (operator-template "mem"   #f membership args))
(define (op-tak . args) (operator-template "tak"   #f our-take args))
(define (op-dro . args) (operator-template "dro"   #f our-drop args))
(define (op-enc . args) (operator-template "enc"   #f encode args))
(define (op-dec . args) (operator-template "dec"   #f decode args))
(define (op-cat . args) (operator-template "cat"   #f append args))
(define (op-red . args) (operator-template "red"   #f op-reduce args))
(define (op-lef . args) (first args))
(define (op-rig . args) (second args))
(define (op-rho . args) (list (length (first args))))

(define (op-reduce op xs)
  (list (fold (lambda (x y) (first (op (list x) (list y)))) (car xs) (cdr xs))))

(define (print-array array)
  (if (null? array)
    ""
    (begin
      (display (car array))
      (display " ")
      (print-array (cdr array)))))

(define (parse vars lexemes)
  (cond
    ((null? lexemes) (list vars '()))
    ((= (length lexemes) 1)
      (if (left-parenthesis? (first lexemes))
        (list vars (list 'quote (eval (second (parse vars (lex (strip-parentheses (first lexemes))))) environ)))
        (list vars (list 'quote (parse-value vars (first lexemes))))))
    ((equal? (second lexemes) "#assign")
      (list (cons (list (intern (first lexemes)) (eval (second (parse vars (cddr lexemes))) environ)) vars) '()))
    ((operator? (second lexemes)) ; binary
      (list vars (list (parse-op (second lexemes)) (second (parse vars (take lexemes 1))) (second (parse vars (cddr lexemes))))))
    ((operator? (first lexemes)) ; unary
      (list vars (list (parse-op (first lexemes)) (second (parse vars (cdr lexemes))))))

    (else (error "malformed expression"))))

(define (substitute vars variable)
  (second (false-check (assoc variable vars))))

(define (false-check e) (if e e (error "undefined variable")))

(define environ (the-environment))

(define ov '((a (5)) (b (3))))

(define (apl-repl vars)
    (display "& ")
    (let* ((parsed (parse vars (lex (read-line))))
          (new-vars (first parsed))
          (result (eval (second parsed) environ)))
            (print-array result)
            (newline)
            (apl-repl new-vars)))


