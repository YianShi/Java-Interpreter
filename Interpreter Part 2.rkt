#lang racket
;; Yuqi Hu, Yian Shi

(require "simpleParser.rkt")
(require "lex.rkt")

;; Interpret
;; Call M_state_statement to interpret the parse tree of code.
;; Define initial state, return, break, continue, throw, and next.
(define interpret
  (lambda (filename)
    (if (null? filename)
        (error "File not found")
        (M_state_statementList (parser filename)
                               (state)                                    ; initial state
                               (lambda (v) v)                           ; return continuation
                               (lambda (s) (error "Error in break"))    ; break continuation
                               (lambda (s) (error "Error in continue")) ; continue continuation
                               (lambda (s v) (error "Error in throw"))  ; throw continuation
                               (lambda (s) s)))))                       ; next continuation

;; M_state_statementList
;; Process a list of statements.
(define M_state_statementList
  (lambda (statementList state return break continue throw next)
    (cond
      ((null? statementList) (next state))
      ((list? (first statementList)) (M_state_statement (first statementList) state return break continue throw
                                                      (lambda (s) (M_state_statementList (restOfstmtL statementList) s return break continue throw next))))
      (else (error "Incorrect statement list")))))

;; M_state_statement
;; Identify the type of the statement and call correspondent function.
(define M_state_statement
  (lambda (statement state return break continue throw next)
    (cond
      ((eq? 'var (operator statement)) (M_state_declare statement state next))
      ((eq? '= (operator statement)) (M_state_assign statement state next))
      ((eq? 'return (operator statement)) (M_state_return statement state return))
      ((eq? 'if (operator statement)) (M_state_if statement state return break continue throw next))
      ((eq? 'while (operator statement)) (M_state_while statement state return throw next))
      ((eq? 'continue (operator statement)) (continue state))
      ((eq? 'break (operator statement)) (break state))
      ((eq? 'throw (operator statement)) (M_state_throw statement state throw))
      ((eq? 'begin (operator statement)) (M_state_block statement state return break continue throw next))
      ((eq? 'try (operator statement)) (M_state_try statement state return break continue throw next))
      (else (error "Not a statement")))))

(define stmtOfRoperand cddr)

;; Process declare statement.
;; Store the variable to the state by calling store method.
(define M_state_declare
  (lambda (statement state next)
    (if (null? (stmtOfRoperand statement))
        (next (store (l-operand statement) '() state))
        (next (store (l-operand statement) (M_value (replaceAll (r-operand statement) state)) state)))))

;; Process assign statement.
;; Add the value to the correspondent variable in the state.
;; If the variable x is not declared, print "x is not declared".
(define M_state_assign
  (lambda (statement state next)
    (if (declared? (l-operand statement) state)
        (next (assign (l-operand statement) (M_value (replaceAll (r-operand statement) state)) state))
        (error (string-append (symbol->string (l-operand statement)) " is not declared")))))

;; Process return statement.
;; Return the value of the statement by calling M_value method.
;; Use variable result to deal with #t and #f.
(define M_state_return
  (lambda (statement state return)
    (let ((result (return (M_value (replaceAll (cadr statement) state)))))
      (if (boolean? result)
          (if result 'true 'false)
          result))))

(define option-else cdddr)
(define option-else-statement cadddr)

;; Process if statement.
(define M_state_if
  (lambda (statement state return break continue throw next)
    (cond
      ((not (boolean? (M_value (replaceAll (condition statement) state)))) (error "Condition is not a boolean value"))
      ((M_value (replaceAll (condition statement) state)) (M_state_statement (body statement) state return break continue throw next))
      ((null? (option-else statement)) (next state))        ; check if there is "else" statement
      (else (M_state_statement (option-else-statement statement) state return break continue throw next)))))

(define nextCondition cadr)
(define nextStatement caddr)

;; Process while statement.
;; Redefine break and continue in the whileLoop.
(define M_state_while
  (lambda (statement state return throw next)
    (if (not (boolean? (M_value (replaceAll (cadr statement) state))))
      (error "Condition is not a boolean value")
      (letrec ((whileLoop (lambda (condition body state)
                       (if (M_value (replaceAll condition state))
                           (M_state_statement body state return (lambda (s) (next s)) (lambda (s) (whileLoop condition body s)) throw (lambda (s) (whileLoop condition body s)))
                           (next state)))))
        (whileLoop (nextCondition statement) (nextStatement statement) state)))))

(define throwStmt cadr)

;; Process throw statement.
(define M_state_throw
  (lambda (statement state throw)
    (throw state (M_value (replaceAll (throwStmt statement) state)))))

;; Process block statement.
;; Push a frame when interpreting the block and pop the frame when leaving it.
(define M_state_block
  (lambda (statement state return break continue throw next)
    (M_state_statementList (restOfStmt statement)
                           (pushframe state)
                           return
                           (lambda (s) (break (popframe s)))
                           (lambda (s) (continue (popframe s)))
                           (lambda (s v) (throw (popframe s) v))
                           (lambda (s) (next (popframe s))))))

;; Process try statement.
;; Update return, break, continue, and throw before interpreting the finallyBlock.
(define M_state_try
  (lambda (statement state return break continue throw next)
    (let* ((tryBlock (cons 'begin (trystmt statement)))
           (catchStatement (catchstmt statement))
           (catchBody (if (null? catchStatement) '() (catchbody catchStatement)))
           (catchVar (if (null? catchStatement) '() (catchexception catchStatement)))
           (finallyBlock (if (null? (cadddr statement)) (list 'begin) (cons 'begin (cadr (cadddr statement)))))
           (return2 (lambda (v) (M_state_block finallyBlock state return break continue throw (lambda (s) (return v)))))
           (break2 (lambda (bs) (M_state_block finallyBlock state return break continue throw (lambda (s) (break s)))))
           (continue2 (lambda (cs) (M_state_block finallyBlock state return break continue throw (lambda (s) (continue s)))))
           (throw2 (if (null? catchStatement)
                       (lambda (ts ex) (M_state_block finallyBlock state return break continue throw (lambda (s) (throw ts ex))))
                       (lambda (ts ex) (M_state_statementList catchBody
                                                          (store catchVar ex (pushframe ts))
                                                          return
                                                          (lambda (s) (break (popframe s)))
                                                          (lambda (s) (continue (popframe s)))
                                                          (lambda (s v) (throw (popframe s) v))
                                                          (lambda (s) (M_state_block finallyBlock (popframe s) return break continue throw next)))))))
      (M_state_block tryBlock state return2 break2 continue2 throw2 (lambda (s) (M_state_block finallyBlock s return break continue throw next))))))


;; Help functions
(define operator car)
(define first car)
(define restOfStmt cdr)
(define l-operand cadr)
(define r-operand caddr)
(define condition cadr)
(define body caddr)
(define restOfState cdr)
(define firstFrame car)
(define varName caar)
(define restOfstmtL cdr)
(define value cdar)
(define trystmt cadr)
(define catchstmt caddr)
(define catchbody caddr)
(define catchexception caadr)

;; Identify whether an element is variable (starting with letter).
;; || is special. It returns empty string when applying symbol->string.
(define var?
  (lambda (x)
    (if (or (number? x) (eq? x '||))
        #f
        (char-alphabetic? (car (string->list (symbol->string x)))))))

;; Replace a variable with its value according to the state.
(define replaceVar
  (lambda (var state)
    (cond
      ((null? state) (error (string-append (symbol->string var) " is not defined")))
      ((inFrame? var (firstFrame state)) (replaceVarInFrame var (firstFrame state)))
      (else (replaceVar var (restOfState state))))))

(define replaceVarInFrame
  (lambda (var frame)
    (if (eq? var (varName frame))
        (if (null? (value frame))
            (error (string-append (symbol->string var) " has no value"))
            (cadar frame))
        (replaceVarInFrame var (restOfState frame)))))

;; Replace all variables in the statement.
(define replaceAll
  (lambda (statement state)
    (if (list? statement)
        (cond
          ((null? statement) '())
          ((list? (first statement)) (cons (replaceAll (first statement) state) (replaceAll (restOfStmt statement) state)))
          ((eq? 'false (first statement)) (cons #f (replaceAll (restOfStmt statement) state)))
          ((eq? 'true (first statement)) (cons #t (replaceAll (restOfStmt statement) state)))
          ((var? (first statement)) (cons (replaceVar (first statement) state) (replaceAll (restOfStmt statement) state)))
          (else (cons (first statement) (replaceAll (restOfStmt statement) state))))
        (cond
          ((boolean? statement) statement)
          ((number? statement) statement)
          ((eq? 'false statement) #f)
          ((eq? 'true statement) #t)
          ((var? statement) (replaceVar statement state))))))

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
        ((eq? (operator statement) '>=) (or (eq? (M_value (l-operand statement)) (M_value (r-operand statement))) (> (M_value (l-operand statement)) (M_value (r-operand statement)))))
        ((eq? (operator statement) '<=) (or (eq? (M_value (l-operand statement)) (M_value (r-operand statement))) (< (M_value (l-operand statement)) (M_value (r-operand statement)))))
        ((eq? (operator statement) '!=) (not (eq? (M_value (l-operand statement)) (M_value (r-operand statement)))))
        ((eq? (operator statement) '!) (not (M_value (l-operand statement))))
        ((eq? (operator statement) '&&) (and (M_value (l-operand statement)) (M_value (r-operand statement))))
        ((eq? (operator statement) '||) (or (M_value (l-operand statement)) (M_value (r-operand statement)))))
      statement)))

;; Check wheather a variable is in the state.
(define declared?
  (lambda (var state)
    (cond
      ((null? state) #f)
      ((inFrame? var (firstFrame state)) #t)
      (else (declared? var (restOfState state))))))

;; Check wheather a variable is in the frame.
(define inFrame?
  (lambda (var frame)
    (cond
      ((null? frame) #f)
      ((eq? var (varName frame)) #t)
      (else (inFrame? var (restOfState frame))))))

;; Store the variable in the state.
(define store
  (lambda (var value state)
    (cond
      ((inFrame? var (firstFrame state)) (error (string-append (symbol->string var) " is being redefined")))
      ((null? value) (cons (cons (list var) (firstFrame state)) (restOfState state)))
      (else (cons (cons (cons var (list value)) (firstFrame state)) (restOfState state))))))

;; Add the value to the correspondent variable in the state.
;; First find the frame where the variable exists, then operate on this frame and update it in the state.
(define assign
  (lambda (var value state)
    (if (inFrame? var (firstFrame state))
        (cons (updateFrame var value (firstFrame state)) (restOfState state))
        (cons (firstFrame state) (assign var value (restOfState state))))))

;; create the initial state
(define state
  (lambda ()
   (list (newlayer))))

;; create an additional layer of state
(define newlayer
  (lambda ()
    '()))

;; Add the value to the correspondent variable in the frame.
(define updateFrame
  (lambda (var value frame)
    (if (eq? (varName frame) var)
        (cons (cons var (list value)) (restOfState frame))
        (cons (firstFrame frame) (updateFrame var value (restOfState frame))))))

;; Add an additional frame on the current state.
(define pushframe
  (lambda (state)
    (cons '() state)))

;; Delete the first frame when exit a block of code
(define popframe
  (lambda (state)
    (restOfState state)))
