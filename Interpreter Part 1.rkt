;; Yuqi Hu, Yian Shi

#lang racket

(require "simpleParser.rkt")
(require "lex.rkt")

;; interpret
;; Call M_state_statement to interpret the parse tree of code.
;; Initial state is '(), since there are no variables.
(define interpret
  (lambda (filename)
    (if (null? filename)
        (error "File not found")
        (M_state_statementList (parser filename) '()))))

(define restOfstmtL cdr)
(define firstStmt car)

;; M_state_statementList
;; Process a list of statements.
(define M_state_statementList
  (lambda (statementList state)
    (cond
      ((null? statementList)       state)
      ((list? (car statementList)) (M_state_statementList (restOfstmtL statementList) (M_state_statement (firstStmt statementList) state)))
      (else                        (error "Incorrect statement list")))))

;; M_state_statement
;; Receive a statement and a state, process the statement, and return the new state.
;; Identify the type of the statement and call correspondent function.
(define M_state_statement
  (lambda (statement state)
    (cond
      ((null? statement) state)
      ((eq? 'var (operator statement))    (M_state_declare statement state))
      ((eq? '= (operator statement))      (M_state_assign statement state))
      ((eq? 'return (operator statement)) (M_state_return statement state))
      ((eq? 'if (operator statement))     (M_state_if statement state))
      ((eq? 'while (operator statement))  (M_state_while statement state))
      (else                               (error "Not a statement")))))

;; M_value
;; Receive a statement with all variables replaced by their values.
;; Return the value of the statement.

;; Identify whether an element is variable (starting with letter).
;; || is special. It returns empty string when applying symbol->string.
(define var?
  (lambda (x)
    (if (or (number? x) (eq? x '||))
        #f
        (char-alphabetic? (car (string->list (symbol->string x)))))))

(define variable caar)
(define value cadar)

;; Replace a variable with its value according to the state.
(define replacevar
  (lambda (var state)
    (cond
      ((eq? var 'true) #t)
      ((eq? var 'false) #f)
      ((null? state) (error (string-append (symbol->string var) " is not declared")))
      ((eq? var (variable state)) (if (null? (cdar state))
                                (error (string-append (symbol->string var) " has no value"))
                                (value state)))
      (else (replacevar var (restOfState state))))))

(define restOfStmt cdr)
(define first car)

;; Replace all variables in the statement.
(define replaceall
  (lambda (statement state)
    (if (list? statement)
        (cond
          ((null? statement) '())
          ((list? (first statement)) (cons (replaceall (first statement) state) (replaceall (restOfStmt statement) state)))
          ((var? (first statement)) (cons (replacevar (first statement) state) (replaceall (restOfStmt statement) state)))
          (else (cons (first statement) (replaceall (restOfStmt statement) state))))
        (cond
          ((boolean? statement) statement)
          ((number? statement) statement)
          ((var? statement) (replacevar statement state))))))

(define operator car)
(define l-operand cadr)
(define r-operand caddr)

;; Compute the value of an expression.
(define M_value
  (lambda (statement)
    (if (list? statement)
      (cond
        ((eq? (operator statement) '+) (+ (M_value (l-operand statement)) (M_value (r-operand statement))))
        ((eq? (operator statement) '-) (if (null? (cddr statement))
                                      (- 0 (M_value (l-operand statement)))
                                      (- (M_value (l-operand statement)) (M_value (r-operand statement)))))
        ((eq? (operator statement) '*) (* (M_value (l-operand statement)) (M_value (r-operand statement))))
        ((eq? (operator statement) '/) (quotient (M_value (l-operand statement)) (M_value (r-operand statement))))
        ((eq? (operator statement) '%) (remainder (M_value (l-operand statement)) (M_value (r-operand statement))))
        ((eq? (operator statement) '==) (eq? (M_value (l-operand statement)) (M_value (r-operand statement))))
        ((eq? (operator statement) '>) (> (M_value (l-operand statement)) (M_value (r-operand statement))))
        ((eq? (operator statement) '<) (< (M_value (l-operand statement)) (M_value (r-operand statement))))
        ((eq? (operator statement) '>=)
         (or (eq? (M_value (l-operand statement)) (M_value (r-operand statement))) (> (M_value (l-operand statement)) (M_value (r-operand statement)))))
        ((eq? (operator statement) '<=)
         (or (eq? (M_value (l-operand statement)) (M_value (r-operand statement))) (< (M_value (l-operand statement)) (M_value (r-operand statement)))))
        ((eq? (operator statement) '!=) (not (eq? (M_value (l-operand statement)) (M_value (r-operand statement)))))
        ((eq? (operator statement) '!) (not (M_value (l-operand statement))))
        ((eq? (operator statement) '&&) (and (M_value (l-operand statement)) (M_value (r-operand statement))))
        ((eq? (operator statement) '||) (or (M_value (l-operand statement)) (M_value (r-operand statement)))))
      statement)))

(define condition cadr)
(define body caddr)

;; M_state_while
;; Receive a statement and a state.
;; Return the new state after excuting the while statement.
(define M_state_while
  (lambda (statement state)
    (cond
      ((not (boolean? (M_value (replaceall (condition statement) state)))) (error "Condition is not a boolean value"))
      ((M_value (replaceall (condition statement) state)) (M_state_while statement (M_state_statement (body statement) state)))
      (else state))))

(define option-else cdddr)
(define option-else-statement cadddr)

;; M_state_if
;; Receive a statement and a state.
;; Return the new state after excuting the if statement.
(define M_state_if
  (lambda (statement state)
    (cond
      ((not (boolean? (M_value (replaceall (condition statement) state)))) (error "Condition is not a boolean value"))
      ((M_value (replaceall (condition statement) state)) (M_state_statement (body statement) state))
      ((null? (option-else statement)) state)
      (else (M_state_statement (option-else-statement statement) state)))))

;; M_state_assign
;; Receive a statement and a state.
;; Return the new state after assign the value to the variable.

;; Store the variable to the state
(define store
  (lambda (var value state)
    (cond
      ((null? value)              (cons (list var) state))
      ((null? state)              (list (cons var (list value))))
      ((eq? var (variable state)) (cons (cons var (list value)) (cdr state)))
      (else                       (cons (car state) (store var value (cdr state)))))))

(define restOfState cdr)

;; Identify whether a variable is declared (could be found in the state).
(define declared?
  (lambda (var state)
    (cond
      ((null? state) #f)
      ((eq? var (variable state)) #t)
      (else (declared? var (restOfState state))))))

;; Assign the value. If the variable x is not declared, print "x is not declared".
(define M_state_assign
  (lambda (statement state)
    (if (declared? (l-operand statement) state)
        (store (l-operand statement) (M_value (replaceall (r-operand statement) state)) state)
        (error (string-append (symbol->string (l-operand statement)) " is not declared")))))

(define stmtOfRoperand cddr)

;; M_state_declare
;; Receive a statement and a state.
;; Return the new state after declare the variable.
;; Identify whether the variable has been defined.
;; Then store the variable to the state by calling store method.
(define M_state_declare
  (lambda (statement state)
    (if (null? (stmtOfRoperand statement))
        (if (declared? (l-operand statement) state)
            (error (string-append (symbol->string (l-operand statement)) " is being redefined"))
            (store (l-operand statement) '() state))
        (if (declared? (l-operand statement) state)
            (error (string-append (symbol->string (l-operand statement)) " is being redefined"))
            (store (l-operand statement) (M_value (replaceall (r-operand statement) state)) state)))))

;; M_state_return
;; Receive a statement and a state.
;; Return the value of the statement after "return" by calling M_value method.
(define result
  (lambda (statement state)
    (M_value (replaceall (cadr statement) state))))

(define M_state_return
  (lambda (statement state)
    (if (boolean? (result statement state))
        (if (result statement state) 'true 'false)
        (result statement state))))
