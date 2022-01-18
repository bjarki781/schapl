;;; SchAPL - simple APL interpreter written in Scheme

; todo
; * redo lexer, define better what is valid syntax
;   (e.g. '# plus' and '3 a b c' is a valid now)
; * implement more monadic and dyadic operators
; * implement reduce and scan operators

(define-syntax push!
  (syntax-rules ()
    ((push! list item)
      (set! list (cons item list)))))

(define-syntax dequote
  (syntax-rules ()
    ((dequote (quote thing)) (dequote thing))
    ((dequote thing) thing)))
      

(define pi (* 4 (atan 1)))
(define (dot f g) (lambda (x) (f (g x))))
(define (swap op) (lambda (a b) (op b a)))
(define (curry op a) (lambda (b) (op a b)))
(define (liftify f) (lambda (mx) (f (car mx))))
(define (liftify2 f) (lambda (mx my) (f (car mx) (car my))))
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

(define (sort-index pred)
  (lambda (l0)
    (let qsort ((l l0) (indeces (iota (length l0))))
      (cond
        ((null? indeces) ())
        (else (append
          (qsort l (list-pred pred l (car indeces) (cdr indeces)))
          (cons (car indeces) ())
          (qsort l (list-pred (lambda (x y) (not (pred x y))) l (car indeces) (cdr indeces)))))))))

(define (list-pred pred l a0 b0)
  (let lp ((a a0) (b b0))
    (cond
      ((or (null? a) (null? b)) 
        ())
      ((pred (list-ref l a) (list-ref l (car b))) 
        (lp a (cdr b)))
      (else 
        (cons (car b) (lp a (cdr b)))))))


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

(define (our-lambda args function)
  (list (make-monadic "anonymous" (lambda args (eval function user-initial-environment)))))

(define (reduce-expand a b)
    (list (fold (arithmetic-ambi-dyadic (eval (car a) user-initial-environment)) (car b) (cdr b))))

(define (ambi-template monadic-name dyadic-name monadic dyadic args)
  (cond
    ((and (= (length args) 1) monadic) (monadic (car args)))
    ((and (= (length args) 2) dyadic) (dyadic (car args) (cadr args)))
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
(define (apply-operator op-id . args) 
  (let ((op (cadr (assq op-id ov))))
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
   (else (error "error")))))


(define ov
  `((mul ,(make-arithmetic-ambi "signum"            "multiply"       signum         *))
    (add ,(make-arithmetic-ambi "conjugate"         "addition"       (lambda (x) x) +))
    (sub ,(make-arithmetic-ambi "negation"          "subtraction"    (curry * -1)   -))
    (div ,(make-arithmetic-ambi "reciprocal"        "division"       (curry /  1)   /))
    (ban ,(make-arithmetic-ambi "factorial"         "combinations"   fac            nCr))
    (dea ,(make-arithmetic-ambi "roll "             "deal"           random-integer deal))
    (abs ,(make-arithmetic-ambi "magnitude"         "residue"        abs            remainder))
    (cei ,(make-arithmetic-ambi "ceiling"           "maximum"        ceiling        max))
    (flo ,(make-arithmetic-ambi "floor"             "minimum"        floor          min))
    (cir ,(make-arithmetic-ambi "multiply by pi"    "circle"         (curry * pi)   circle))
    (exp ,(make-arithmetic-ambi "natural logarithm" "logarithm"      log            logt))
    (exp ,(make-arithmetic-ambi "exponential"       "exponentiation" exp            expt))

    (le  ,(make-arithmetic-dyadic "le"  (to-predicate < )))
    (leq ,(make-arithmetic-dyadic "le"  (to-predicate <=)))
    (eq  ,(make-arithmetic-dyadic "eq"  (to-predicate  =)))
    (geq ,(make-arithmetic-dyadic "geq" (to-predicate >=)))
    (ge  ,(make-arithmetic-dyadic "ge"  (to-predicate > )))
    (neq ,(make-arithmetic-dyadic "neq" (to-predicate (lambda (x y) (not (= x y))))))

    (or  ,(make-arithmetic-dyadic "or"  our-or))
    (and ,(make-arithmetic-dyadic "and" our-and))

    (mem ,(make-dyadic  "membership" membership))
    (tak ,(make-dyadic  "take"       our-take))
    (dro ,(make-dyadic  "drop"       our-drop))
    (enc ,(make-dyadic  "encode"     encode))
    (dec ,(make-dyadic  "decode"     decode))
    (cat ,(make-dyadic  "catenate"   append))
    (lef ,(make-dyadic  "left"       (lambda (l r) l)))
    (rig ,(make-dyadic  "right"      (lambda (l r) r)))
    (rho ,(make-monadic "length"     (lambda (l) (list (length l)))))
    (gup ,(make-monadic "grade up"   (sort-index <)))
    (gdo ,(make-monadic "grade down" (sort-index >)))

    (iot ,(make-ambi "iota" "find" (liftify iota) find-index))
    (red ,(make-dyadic "reduction/expansion" reduce-expand))

    (lam ,(make-dyadic "lambda" our-lambda))

    (ind ,(make-dyadic "list-ref" (lambda (l r) (map (curry list-ref l) r))))))

(define (error str) (begin (display str) ()))
(define (string-car str) (string-ref str 0))
(define (string-cdr str) (string-tail str 1))
(define (car-digit nustr) (char->digit (string-car nustr)))
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
    ()
    (let* ((tok (reader expr)))
      (cons (string-trim tok) (lex (string-trim (string-tail expr (string-length tok))))))))

(define (parse-number nustr)
  ((char=? (string-car nustr) #\_) (* -1 (parse-number (string-cdr nustr)))))

; both functions expect a string of digits
(define (parse-integer nustr)
  (cond
    ((string-null? nustr) 0)
    (else
      (+ (* (expt 10 (-1+ (string-length nustr))) (car-digit nustr))
         (parse-number (string-cdr nustr))))))

(define (parse-fraction nustr)
  (let loop ((str (reverse nustr)))
    (cond
      ((string-null? nustr) 0)
      (else
        (+ (* (expt 10 (* -1 (string-length str))) (car-digit str))
           (loop (string-cdr nustr)))))))

(define (tokenize-array arrstr)
  (if (string-null? arrstr)
    ()
    (let* ((elem (string-take-until arrstr char-set:whitespace)))
      (cons (string-trim elem) (tokenize-array (string-trim (string-tail arrstr (string-length elem))))))))

(define (string->number/symbol str)
  (if (char-in-set? (string-car str) char-set:alphabetic) 
      (string->symbol str) 
      (string->number str)))

(define (parse-array arrstr)
; (map parse-number (tokenize-array arrstr)))
  (map string->number/symbol (tokenize-array arrstr)))

(define (false-check e) (if e e (error "undefined variable")))

(define (substitute variable)
  (cdr (false-check (assoc variable ov))))

(define (parse-value valstr)
  (cond
    ((char=? (string-car valstr) #\$) (substitute (intern (string-cdr valstr))))
    (else (parse-array valstr))))

(define (parse-op opstr)
  (list 'quote (intern (string-cdr opstr))))
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

(define (parse lexemes)
  (cond
    ((null? lexemes) 
      ())
    ((= (length lexemes) 1)
      (cond 
        ((equal? (car lexemes) "#quit") (list ''quit))
        ((left-parenthesis? (car lexemes))
          (eval (parse (lex (strip-parentheses (car lexemes)))) user-initial-environment))
        ((operator? (car lexemes)) 
          (list (parse-op (car lexemes))))
        (else 
          (list 'quote (parse-value (car lexemes))))))
    ((equal? (cadr lexemes) "#assign")
      (begin (push! ov (list (string->symbol (car lexemes)) (eval (parse (cddr lexemes)) user-initial-environment))) ()))
    ((operator? (cadr lexemes)) ; binary
      (if (equal? (cadr lexemes) "#lam") 
          `(car (apply-operator ,(parse-op (cadr lexemes)) ,(parse (take lexemes 1)) ',(parse (cddr lexemes))))
          `(apply-operator ,(parse-op (cadr lexemes)) ,(parse (take lexemes 1)) ,(parse (cddr lexemes)))))
    ((operator? (car lexemes)) ; unary
      `(apply-operator ,(parse-op (car lexemes)) ,(parse (cdr lexemes))))
    (else (error "malformed expression"))))

(define (apl-repl)
    (display "& ")
    (let* ((parsed (parse (lex (read-line))))
          (result (eval parsed user-initial-environment)))
            (print-array result)
            (newline)
            (if (equal? result 'quit) () (apl-repl))))


