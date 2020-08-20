(define (error str) (begin (display str) '()))
(define (string-car str) (string-ref str 0))
(define (string-cdr str) (string-tail str 1))
(define (first-digit nustr) (char->digit (string-car nustr)))
(define (string-take-until str charset)
  (string-head str 
    (let ((nextchar (string-find-next-char-in-set str charset)))
      (if nextchar nextchar (string-length str)))))

(define (array? str) (first-digit str))

(define (unary? opstr)
  (or
    (equal? opstr "+")
    (equal? opstr "-")
    (equal? opstr "?")
    (equal? opstr "!")))

(define (binary? opstr)
  (or
    (equal? opstr "+")
    (equal? opstr "-")
    (equal? opstr ",")
    (equal? opstr "/")))

(define (reader str)
  (if (first-digit str) 
    (read-array str)
    (read-op str)))

(define (read-array str) 
  (string-take-until str (string->char-set "+-*/")))

(define (read-op str) 
  (string-take-until str char-set:numeric))

(define (lex expr)
  (if (string-null? expr) 
    '()
    (let* ((tok (reader expr)))
      (cons (string-trim tok) (lex (string-tail expr (string-length tok)))))))

(define (parse-number nustr)
  (if (string-null? nustr)
    0
    (+ (* (expt 10 (- (string-length nustr) 1)) (first-digit nustr))
       (parse-number (string-cdr nustr)))))

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
    (else (error "vectors not the same length"))))

(define (parse-op opstr)
  (cond
    ((equal? opstr "+") 'op-add)
    ((equal? opstr "-") 'op-sub)
    ((equal? opstr "*") 'op-mul)
    ((equal? opstr "!") 'op-exclemation)
    ((equal? opstr "/") 'op-compression)))


(define (op-add . args)
  (cond
    ((= (length args) 1) (car args))
    ((= (length args) 2) (apply-binary + (first args) (second args)))
    (else (error "add error"))))

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
      (list 'quote (parse-array (car lexemes))))
    ((array? (car lexemes)) ; binary
      (list (parse-op (cadr lexemes)) (parse (list-head lexemes 1)) (parse (cddr lexemes))))
    (else ; unary
      (list (parse-op (car lexemes)) (parse (cdr lexemes))))))

(define environ (the-environment))

(define (apl-repl) 
    (display "& ")
    (print-array (eval (parse (lex (read-line))) environ))
    (newline)
    (apl-repl))

