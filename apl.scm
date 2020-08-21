;;; SchAPL - simple APL interpreter written in Scheme

; todo
; * redo lexer, define better what is valid syntax
;   (e.g. '# plus' and '3 a b c' is a valid now)
; * implement more monadic and dyadic operators
; * implement reduce and scan operators
; * implement matrices and some operators

(define (error str) (begin (display str) '()))
(define (swap op) (lambda (a b) (op b a)))
(define (curry op a) (lambda (b) (op a b)))
(define (string-car str) (string-ref str 0))
(define (string-cdr str) (string-tail str 1))
(define (first-digit nustr) (char->digit (string-car nustr)))
(define (string-take-until str charset)
  (string-head str 
    (let ((nextchar (string-find-next-char-in-set str charset)))
      (if nextchar nextchar (string-length str)))))

(define (array? str) (first-digit str))

(define (reader str)
  (if (array? str) 
    (read-array str)
    (read-op str)))

(define (read-array str) 
  (string-take-until str (string->char-set "#)")))

(define (read-op str) 
  (if (char=? (string-car str) #\#)
      (string-append "#" (string-take-until (string-cdr str) (char-set-union char-set:numeric (string->char-set "#)")))) 
      (error "operator must be begin with hash sign (#)")))

(define (lex expr)
  (if (string-null? expr) 
    '()
    (let* ((tok (reader expr)))
      (cons (string-trim tok) (lex (string-tail expr (string-length tok)))))))

(define (parse-number nustr)
  (cond 
    ((string-null? nustr) 0)
    ((char=? (string-car nustr) #\-) (* -1 (parse-number (string-cdr nustr))))
    ((char=? (string-car nustr) #\+) (parse-number (string-cdr nustr)))
    (else 
    (+ (* (expt 10 (- (string-length nustr) 1)) (first-digit nustr))
       (parse-number (string-cdr nustr)))))) 

(define (tokenize-array arrstr)
  (if (string-null? arrstr)
    '()
    (let* ((elem (string-take-until arrstr char-set:whitespace)))
      (cons (string-trim elem) (tokenize-array (string-trim (string-tail arrstr (string-length elem))))))))

(define (parse-array arrstr)
  (map parse-number (tokenize-array arrstr)))

(define (apply-binary op arg1 arg2)
  (cond 
    ((and (null? arg1) (null? arg2)) '())
    ((= (length arg1) (length arg2)) 
     (cons (op (car arg1) (car arg2))
           (apply-binary op (cdr arg1) (cdr arg2))))
    ((= (length arg1) 1) (map (curry op (car arg1)) arg2))
    ((= (length arg2) 1) (map (curry (swap op) (car arg2)) arg1))
    (else (error "vectors must be as long or one a singlet"))))

(define (parse-op opstr)
  (if (not (equal? (string-car opstr) #\#)) 
    (error "invalid operator")
    (symbol-append 'op- (string->symbol (string-cdr opstr))))) 

(define (signum a) 
  (cond
    ((< a 0) -1)
    ((= a 0) 0)
    ((> a 0) 1)
    )) 

(define (fac n)
  (if (< n 2) 1 (* n (fac (- n 1)))))

(define (op-plus . args)
  (cond
    ((= (length args) 1) (car args))
    ((= (length args) 2) (apply-binary + (first args) (second args)))
    (else (error "add error"))))

(define (op-minus . args)
  (cond
    ((= (length args) 1) (map (curry * -1) (first args)))
    ((= (length args) 2) (apply-binary - (first args) (second args)))
    (else (error "sub error"))))

(define (op-multiply . args)
  (cond
    ((= (length args) 1) (map signum (first args)))
    ((= (length args) 2) (apply-binary * (first args) (second args)))
    (else (error "mul error"))))

(define (op-divide . args)
  (cond
    ((= (length args) 1) (map (curry / 1) (first args)))
    ((= (length args) 2) (apply-binary / (first args) (second args)))))

(define (op-bang . args)
  (cond 
    ((= (length args) 1) (map fac (first args)))
    ((= (length args) 2) (error "implement nCr"))
    )
  )

(define (iota a b) 
  (if (= a b) (list b) (cons a (iota (+ a 1) b))))

(define (op-iota . args)
  (cond 
    ((= (length args) 1) (iota 1 (car (first args))))
    (else (error "iota: too many arguments"))))

(define (print-array array)
  (if (null? array) 
      '() 
      (begin 
        (display (car array)) 
        (display " ")
        (print-array (cdr array)))))

(define (parse lexemes)
  (cond
    ((null? lexemes) '())
    ((= (length lexemes) 1) 
      (list 'quote (parse-array (first lexemes))))
    ((array? (first lexemes)) ; binary
      (list (parse-op (second lexemes)) (parse (list-head lexemes 1)) (parse (cddr lexemes))))
    (else ; unary
      (list (parse-op (first lexemes)) (parse (cdr lexemes))))))

(define environ (the-environment))

(define (apl-repl) 
    (display "& ")
    (print-array (eval (parse (lex (read-line))) environ))
    (newline)
    (apl-repl))

