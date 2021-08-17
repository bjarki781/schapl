;;; SchAPL - simple APL interpreter written in Scheme

; todo
; * redo lexer, define better what is valid syntax
;   (e.g. '# plus' and '3 a b c' is a valid now)
; * implement more monadic and dyadic operators
; * implement reduce and scan operators

(define pi (* 4 (atan 1)))
(define (dot f g) (lambda (x) (f (g x))))
(define (swap op) (lambda (a b) (op b a)))
(define (curry op a) (lambda (b) (op a b)))
(define (liftify f) (lambda (mx) (f (first mx))))
(define (liftify2 f) (lambda (mx my) (f (first mx) (first my))))
(define (dot-product x y)
  (cond
    ((not (= (length x) (length y))) (error "dot-product: vectors must be of same length"))
    ((null? x) 0)
    (else (+ (* (car x) (car y)) (dot-product (cdr x) (cdr y))))))
(define (to-predicate op) (lambda (a b) (to-our-boolean (op (from-our-boolean a) (from-our-boolean b)))))
(define (logt base x) (/ (log x) (log base)))
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

(define (our-take li x)
(let ((i (car li)))
  (cond
    ((> (abs i) (length x)) (error "our-take error"))
    ((positive? i) (take x i))
    ((negative? i) (drop x (+ (length x) i))))))

(define (our-drop li x)
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

(define (reduce-expand a b)
    (list (fold (arithmetic-ambi-dyadic (eval (first a) user-initial-environment)) (car b) (cdr b))))

(define (ambi-template monadic-name dyadic-name monadic dyadic args)
  (cond
    ((and (= (length args) 1) monadic) (monadic (first args)))
    ((and (= (length args) 2) dyadic) (dyadic (first args) (second args)))
    (else (error (string-append (if monadic monadic-name dyadic-name) " error" (number->string (length args)))))))

(define (arithmetic-ambi-template monadic-name dyadic-name monadic dyadic args)
  (ambi-template monadic-name dyadic-name 
    (lambda (x) (map monadic x)) 
    (lambda (x y) (apply-binary dyadic x y)) 
    args))

(define (monadic-template name monadic args)
  (ambi-template name #f monadic #f args))

(define (arithmetic-monadic-template name monadic args)
  (ambi-template name #f (lambda (x) (map monadic x)) #f args))

(define (dyadic-template name dyadic args)
  (ambi-template #f name #f dyadic args))

(define (arithmetic-dyadic-template name dyadic args)
  (ambi-template #f name #f (lambda (x y) (apply-binary dyadic x y)) args))

(define-structure ambi monadic-name dyadic-name monadic dyadic)
(define-structure arithmetic-ambi monadic-name dyadic-name monadic dyadic)
(define-structure monadic name operation)
(define-structure arithmetic-monadic name operation)
(define-structure dyadic name operation)
(define-structure arithmetic-dyadic name operation)

; this can be made much shorter with a macro, no?
(define (apply-operator op . args) 
  (cond 
   ((ambi? op) 
     (ambi-template (ambi-monadic-name op) (ambi-dyadic-name op) 
                    (ambi-monadic op) (ambi-dyadic op) args))
   ((arithmetic-ambi? op) 
     (arithmetic-ambi-template 
       (arithmetic-ambi-monadic-name op) (arithmetic-ambi-dyadic-name op) 
       (arithmetic-ambi-monadic op) (arithmetic-ambi-dyadic op) args))
   ((monadic? op) 
     (monadic-template (monadic-name op) (monadic-operation op) args))
   ((arithmetic-monadic? op) 
     (arithmetic-monadic-template (arithmetic-monadic-name op) (arithmetic-monadic-operation op) args))
   ((dyadic? op) 
     (dyadic-template (dyadic-name op) (dyadic-operation op) args))
   ((arithmetic-dyadic? op) 
     (arithmetic-dyadic-template (arithmetic-dyadic-name op) (arithmetic-dyadic-opertion op) args))
   (else (error "error"))))

(define op-mul (make-arithmetic-ambi "signum"            "multiply"       signum         *))
(define op-add (make-arithmetic-ambi "conjugate"         "addition"       (lambda (x) x) +))
(define op-sub (make-arithmetic-ambi "negation"          "subtraction"    (curry * -1)   -))
(define op-div (make-arithmetic-ambi "reciprocal"        "division"       (curry /  1)   /))
(define op-ban (make-arithmetic-ambi "factorial"         "combinations"   fac            nCr))
(define op-dea (make-arithmetic-ambi "roll "             "deal"           random-integer deal))
(define op-abs (make-arithmetic-ambi "magnitude"         "residue"        abs            remainder))
(define op-cei (make-arithmetic-ambi "ceiling"           "maximum"        ceiling        max))
(define op-flo (make-arithmetic-ambi "floor"             "minimum"        floor          min))
(define op-cir (make-arithmetic-ambi "multiply by pi"    "circle"         (curry * pi)   circle))
(define op-exp (make-arithmetic-ambi "natural logarithm" "logarithm"      log            logt))
(define op-exp (make-arithmetic-ambi "exponential"       "exponentiation" exp            expt))

(define op-le  (make-arithmetic-dyadic "le"  (to-predicate < )))
(define op-leq (make-arithmetic-dyadic "le"  (to-predicate <=)))
(define op-eq  (make-arithmetic-dyadic "eq"  (to-predicate  =)))
(define op-geq (make-arithmetic-dyadic "geq" (to-predicate >=)))
(define op-ge  (make-arithmetic-dyadic "ge"  (to-predicate > )))
(define op-neq (make-arithmetic-dyadic "neq" (to-predicate (lambda (x y) (not (= x y))))))

(define op-or  (make-arithmetic-dyadic "or"  our-or))
(define op-and (make-arithmetic-dyadic "and" our-and))

(define op-mem (make-dyadic  "membership" membership))
(define op-tak (make-dyadic  "take"       our-take))
(define op-dro (make-dyadic  "drop"       our-drop))
(define op-enc (make-dyadic  "encode"     encode))
(define op-dec (make-dyadic  "decode"     decode))
(define op-cat (make-dyadic  "catenate"   append))
(define op-lef (make-dyadic  "left"       (lambda (l r) l)))
(define op-rig (make-dyadic  "right"      (lambda (l r) r)))
(define op-rho (make-monadic "length"     (lambda (l) (list (length l)))))

(define op-iot (make-ambi "iota" "find" (liftify iota) find-index))
(define op-red (make-dyadic "reduction/expansion" reduce-expand)) 

(define (error str) (begin (display str) '()))
(define (string-car str) (string-ref str 0))
(define (string-cdr str) (string-tail str 1))
(define (first-digit nustr) (char->digit (string-car nustr)))
(define (string-take-until str charset)
  (string-head str
    (let ((nextchar (string-find-next-char-in-set str charset)))
      (if nextchar nextchar (string-length str)))))
(define (string-take-until-including str charset)
  (string-head str
    (let ((nextchar (string-find-next-char-in-set str charset)))
      (if nextchar (1+ nextchar) (string-length str)))))
(define (operator? str) (char=? (string-car str) #\#))
(define (left-parenthesis? str) (char=? (string-car str) #\())
(define (strip-parentheses parstr) (substring parstr 1 (-1+ (string-length parstr))))

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

(define (false-check e) (if e e (error "undefined variable")))

(define (substitute vars variable)
  (second (false-check (assoc variable vars))))

(define (parse-value vars valstr)
  (cond
    ((char-in-set? (string-car valstr) char-set:alphabetic) (substitute vars (intern valstr)))
    (else (parse-array valstr))))

(define (parse-op opstr)
  (symbol-append 'op- (intern (string-cdr opstr))))
    ;  (if (eqv? (string-car opstr) #\#)
            ;    (symbol-append 'op- (intern (string-cdr opstr))))
    ;    (error (string-append "invalid operator: \"" opstr "\"" )))

(define (print-array array)
  (if (null? array)
    ""
    (begin
      (display (car array))
      (display " ")
      (print-array (cdr array)))))

(define (parse vars lexemes)
  (cond
    ((null? lexemes) 
      (list vars ()))
    ((= (length lexemes) 1)
      (cond 
        ((left-parenthesis? (first lexemes))
          `(,vars ',(eval (second (parse vars (lex (strip-parentheses (first lexemes))))) user-initial-environment)))
        ((operator? (first lexemes)) 
          `(,vars ',(list (parse-op (first lexemes)))))
        (else 
          `(,vars ',(parse-value vars (first lexemes))))))
    ((equal? (second lexemes) "#assign")
      `(,(cons (list (intern (first lexemes)) (eval (second (parse vars (cddr lexemes))) user-initial-environment)) vars) ()))
    ((operator? (second lexemes)) ; binary
      `(,vars (apply-operator ,(parse-op (second lexemes)) ,(second (parse vars (take lexemes 1))) ,(second (parse vars (cddr lexemes))))))
    ((operator? (first lexemes)) ; unary
      `(, vars (apply-operator ,(parse-op (first lexemes)) ,(second (parse vars (cdr lexemes))))))
    (else (error "malformed expression"))))

(define ov '((a (5)) (b (3))))
(define (apl-repl vars)
    (display "& ")
    (let* ((parsed (parse vars (lex (read-line))))
          (new-vars (first parsed))
          (result (eval (second parsed) user-initial-environment)))
            (print-array result)
            (newline)
            (apl-repl new-vars)))


