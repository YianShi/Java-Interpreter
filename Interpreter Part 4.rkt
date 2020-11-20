#lang racket
; By Yian Shi, yx600
; Fails in test 8, 9, 11, 12, 13

; If you are using scheme instead of racket, comment these two lines, uncomment the (load "simpleParser.scm") and comment the (require "simpleParser.rkt")
(require "classParser.rkt")
; (load "simpleParser.scm")

; An interpreter for the simple language that uses call/cc for the continuations.  Does not handle side effects.
(define call/cc call-with-current-continuation)


; The functions that start interpret-...  all return the current environment.
; The functions that start eval-...  all return a value

; The main function. Calls parser to get the parse tree and interprets it with a new environment.  The returned value is in the environment.
(define interpret
  (lambda (file class-with-main-function)
    (scheme->language
     (call/cc
      (lambda (return)
        (get-the-parse-tree (parser file) (newenvironment) (string->symbol class-with-main-function) return
                            (lambda (env) (myerror "Break used outside of loop")) (lambda (env) (myerror "Continue used outside of loop"))
                            (lambda (v env) (myerror "Uncaught exception thrown"))))))))

; Gets the entire parse tree and stores it in the environment. Then executes main class.
(define get-the-parse-tree
  (lambda (statement-list environment class-name return break continue throw)
    (if (null? statement-list)
        (interpret-main class-name environment return break continue throw)
        (get-the-parse-tree (cdr statement-list) (interpret-statement (car statement-list) environment return break continue throw) class-name return break continue throw))))

; interprets a list of statements. The environment from each statement is used for the next ones.
; when the statement-list is empty, interpret the main class.
(define interpret-statement-list
  (lambda (statement-list environment return break continue throw)
    (if (null? statement-list)
        environment
        (interpret-statement-list (cdr statement-list) (interpret-statement (car statement-list) environment return break continue throw) return break continue throw))))

; interpret a statement in the environment with continuations for return, break, continue, throw
(define interpret-statement
  (lambda (statement environment return break continue throw)
    (cond
      ((eq? 'return (statement-type statement)) (interpret-return statement environment return throw))
      ((eq? 'var (statement-type statement)) (interpret-declare statement environment return break continue throw))
      ((eq? '= (statement-type statement)) (interpret-assign statement environment return break continue throw))
      ((eq? 'if (statement-type statement)) (interpret-if statement environment return break continue throw))
      ((eq? 'while (statement-type statement)) (interpret-while statement environment return throw))
      ((eq? 'continue (statement-type statement)) (continue environment))
      ((eq? 'break (statement-type statement)) (break environment))
      ((eq? 'begin (statement-type statement)) (interpret-block statement environment return break continue throw))
      ((eq? 'throw (statement-type statement)) (interpret-throw statement environment throw))
      ((eq? 'try (statement-type statement)) (interpret-try statement environment return break continue throw))
      ((eq? 'class (statement-type statement)) (interpret-class statement environment))
      ((eq? 'function (statement-type statement)) (interpret-function statement environment))
      ((eq? 'static-function (statement-type statement)) (interpret-function statement environment))
      ((eq? 'new (statement-type statement)) (interpret-new statement environment return break continue throw))
      ((eq? 'funcall (statement-type statement)) (update (get-class-name (cadr statement)) (interpret-funcall statement environment return) environment))
      (else (myerror "Unknown statement:" (statement-type statement))))))

; Adds a class to the environment
(define interpret-class
  (lambda (statement environment)
    (insert (get-class-name statement) (get-class-closure statement) environment)))

(define get-class-name cadr)

(define get-class-closure cddr)

; Adds a function to the environment
(define interpret-function
  (lambda (statement environment)
    (insert (get-function-name statement) (get-function-closure statement) environment)))

(define get-function-name cadr)  ; (function f (parameter) body) => f

(define get-function-closure cddr)  ; (function f (parameter) body) => ((parameter) body)

; Gets a value as the closure of a class object. For example, when interpreting (new A), stores A and its closure, including its parent class.
(define interpret-new
  (lambda (statement environment return break continue throw)
    (if (has-parent-class? (lookup (get-class-name statement) environment))
        (get-class-value (get-class-body (lookup (get-class-name statement) environment))
                         ; add its parent class's closure to its own closure
                         (insert 'super (interpret-new (cons 'new (list (get-parent-name (lookup (get-class-name statement) environment)))) environment return (lambda (env) (myerror "Break used outside of loop")) (lambda (env) (myerror "Continue used outside of loop")) throw) (newenvironment))
                         return break continue throw)
        (get-class-value (get-class-body (lookup (get-class-name statement) environment)) (newenvironment) return break continue throw))))

(define get-class-body cadr)

; Gets the closure of a class by interpreting the class body.
(define get-class-value
  (lambda (class-body environment return break continue throw)
    (if (null? class-body)
        environment
        (get-class-value (cdr class-body) (interpret-statement (car class-body) environment return break continue throw) return break continue throw))))

; Checks whether has parent class
(define has-parent-class?
  (lambda (class-closure)
    (if (eq? '() (car class-closure)) #f #t)))

(define get-parent-name cadar)

; Adds parent class's closure to its own closure.
(define add-parent-closure
  (lambda (environment)
    (cons (car environment) (cons (car (lookup 'super environment)) (pop-frame environment)))))

; Interpretes funcall. For example, (funcall (dot (new A) f) 3 5)
(define interpret-funcall
  (lambda (statement environment throw)
    (call/cc
     (lambda (return)
       (if (list? (cadr statement))
           (if (eq? 'super (get-class-name (cadr statement)))  ; if the object has parent class
               ; execute-function (body environment return break continue throw)
               (execute-function (get-function-body (get-function-closure-in-class (get-function-name-in-dot (cadr statement)) (get-class-closure-in-dot (cadr statement) environment return throw)))
                                                    (get-function-environment-in-super (get-formal-parameter (cadr statement) environment return throw)
                                                                                       (eval-parameter-list (get-actual-parameter statement) environment throw)
                                                                                       (get-class-closure-in-dot (cadr statement) environment return throw)
                                                                                       environment)
                                                                                       return
                                                                                       (lambda (env) (myerror "Break used outside of loop"))
                                                                                       (lambda (env) (myerror "Continue used outside of loop"))
                                                                                       throw)
               (execute-function (get-function-body (get-function-closure-in-class (get-function-name-in-dot (cadr statement)) (get-class-closure-in-dot (cadr statement) environment return throw)))
                                 (get-function-environment (get-formal-parameter (cadr statement) environment return throw)
                                                           (eval-parameter-list (get-actual-parameter statement) environment throw)
                                                           (get-class-closure-in-dot (cadr statement) environment return throw)
                                                           environment)
                                 return
                                 (lambda (env) (myerror "Break used outside of loop"))
                                 (lambda (env) (myerror "Continue used outside of loop"))
                                 throw))
           (interpret-funcall (add-this-to-statement statement 0 (cadr statement)) environment throw)))))) ; if no dot (uses the function in this class). For example, (funcall f 3 5). Then add 'this to the function.

; Adds 'this to the function. (funcall f 3 5) => (funcall (dot this f) 3 5))
(define add-this-to-statement
  (lambda (statement num function)
    (if (eq? 1 num)
        (cons (append '(dot) '(this) (list function)) (cddr statement))
        (cons (car statement) (add-this-to-statement statement (+ 1 num) function)))))
       
(define get-function-body cadr)

; For example, in (funcall (dot (new A) f) 3 5), gets the closure of (new A)
(define get-class-closure-in-dot
  (lambda (statement environment return throw)
    (cond
      ; if it is a new class. For example, (funcall (dot (new A) f) 3 5)
      ((and (list? (get-class-name statement)) (eq? 'new (car (get-class-name statement)))) (interpret-new (get-class-name statement) environment return (lambda (env) (myerror "Break used outside of loop")) (lambda (env) (myerror "Continue used outside of loop")) throw))
      ; if there is a funcall inside. For example, (funcall (dot (funcall (dot l getNext)) getNext))
      ((and (list? (get-class-name statement)) (eq? 'funcall (car (get-class-name statement)))) (interpret-funcall (get-class-name statement) environment throw))
      ; other situation. For example, (funcall (dot l setVal) 10)
      (else (lookup (get-class-name statement) environment)))))
    
(define get-function-name-in-dot caddr)

; Find the function closure in the class by function name.
(define get-function-closure-in-class
  (lambda (function-name class-closure)
    (lookup function-name class-closure)))

; Find the formal parameters of a function in the function closure. A function closure is like ((parameters) body)
(define get-formal-parameter
  (lambda (dot-statement environment return throw)
    (get-function-parameter (get-function-closure-in-class (get-function-name-in-dot dot-statement) (get-class-closure-in-dot dot-statement environment return throw)))))

(define get-function-parameter car)

(define get-actual-parameter cddr)

; Takes formal parameter, actual parameter, closure of the object's class, and entire environment
; Generates the function environment => (((this formal-parameter) (this-closure actual-parameter)) this-closure cdr-environment)
(define get-function-environment
  (lambda (formal-parameter actual-parameter this-closure environment)
    (append (list (cons (cons 'this formal-parameter) (list (cons this-closure actual-parameter)))) this-closure (cdr environment))))

; Takes formal parameter, actual parameter, closure of the object's super-class, and entire environment
; Generates the function environment => (((this formal-parameter) (this-closure actual-parameter)) super-closure cdr-environment)
(define get-function-environment-in-super
  (lambda (formal-parameter actual-parameter super-closure environment)
    (append (list (cons (cons 'this formal-parameter) (list (cons (list (cadr environment)) actual-parameter)))) super-closure (cdr environment))))
        
; Get the value of a actual parameter list. (1+1, 1+2) => (2, 3)
(define eval-parameter-list
  (lambda (parameter-list environment throw)
    (if (null? parameter-list) '()
        (cons (eval-expression (car parameter-list) environment throw) (eval-parameter-list (cdr parameter-list) environment throw)))))

; Executes the function, like interpreting the statement list.
(define execute-function
  (lambda (function-body environment return break continue throw)
    (cond
      ((null? function-body) (pop-frame environment))
      (else (execute-function (cdr function-body) (interpret-statement (car function-body) environment return break continue throw) return break continue throw)))))
   
; Interprets the class with main function
(define interpret-main
  (lambda (class-name environment return break continue throw)
    (if (has-parent-class? (lookup class-name environment))
        ; interpret-statement-list (main-function-body environment return break continue throw) 
        (interpret-statement-list (get-main-function-body (get-class-body (lookup class-name environment)))
                                  (get-class-value (get-class-body (lookup class-name environment))
                                                   ; add its parent class's closure to its own closure
                                                   (insert 'super (interpret-new (cons 'new (list (get-parent-name (lookup class-name environment)))) environment return (lambda (env) (myerror "Break used outside of loop")) (lambda (env) (myerror "Continue used outside of loop")) throw) (push-frame environment))
                                                   return break continue throw)
                                  return break continue throw)
        (interpret-statement-list (get-main-function-body (get-class-body (lookup class-name environment)))
                                  (get-class-value (get-class-body (lookup class-name environment)) (push-frame environment) return break continue throw)
                                  return break continue throw))))

;(define add-this-to-main
;  (lambda (environment)
;    (insert 'this (car environment) (push-frame environment))))

(define get-main-function-body
  (lambda (class-body)
    (cond
      ((null? class-body) (myerror "Main function does not exits"))
      ((eq? 'main (get-function-name (car class-body))) (cadddr (car class-body)))
      (else (get-main-function-body (cdr class-body))))))

; Interprets dot
(define interpret-dot
  (lambda (statement environment throw)
    (call/cc
     (lambda (return)
         (eval-expression (caddr statement) (get-class-closure-in-dot statement environment return throw) throw)))))
       
; Calls the return continuation with the given expression value
(define interpret-return
  (lambda (statement environment return throw)
    (return (eval-expression (get-expr statement) environment throw))))

; Adds a new variable binding to the environment.  There may be an assignment with the variable
(define interpret-declare
  (lambda (statement environment return break continue throw)
    (if (exists-declare-value? statement)
        (if (and (list? (get-declare-value statement)) (eq? 'new (car (get-declare-value statement))))
            (insert (get-declare-var statement) (interpret-new (get-declare-value statement) environment return break continue throw) environment)
            (insert (get-declare-var statement) (eval-expression (get-declare-value statement) environment throw) environment))
        (insert (get-declare-var statement) 'novalue environment))))

; Updates the environment to add an new binding for a variable
(define interpret-assign
  (lambda (statement environment return break continue throw)
    (if (and (list? (get-assign-rhs statement)) (eq? 'new (car (get-assign-rhs statement))))
        (if (and (list? (get-assign-lhs statement)) (eq? 'this (cadr (get-assign-lhs statement))))
            (cons (car environment) (update (caddr (get-assign-lhs statement)) (interpret-new (get-assign-rhs statement) environment return break continue throw) (pop-frame environment)))
            (update (get-assign-lhs statement) (interpret-new (get-assign-rhs statement) environment return break continue throw) environment))
        (if (and (list? (get-assign-lhs statement)) (eq? 'this (cadr (get-assign-lhs statement))))
            (cons (car environment) (update (caddr (get-assign-lhs statement)) (eval-expression (caddr (get-assign-lhs statement)) environment throw) (pop-frame environment)))
            (update (get-assign-lhs statement) (eval-expression (get-assign-rhs statement) environment throw) environment)))))

; We need to check if there is an else condition.  Otherwise, we evaluate the expression and do the right thing.
(define interpret-if
  (lambda (statement environment return break continue throw)
    (cond
      ((eval-expression (get-condition statement) environment throw) (interpret-statement (get-then statement) environment return break continue throw))
      ((exists-else? statement) (interpret-statement (get-else statement) environment return break continue throw))
      (else environment))))

; Interprets a while loop.  We must create break and continue continuations for this loop
(define interpret-while
  (lambda (statement environment return throw)
    (call/cc
     (lambda (break)
       (letrec ((loop (lambda (condition body environment)
                        (if (eval-expression condition environment throw)
                            (loop condition body (interpret-statement body environment return break (lambda (env) (break (loop condition body env))) throw))
                         environment))))
         (loop (get-condition statement) (get-body statement) environment))))))

; Interprets a block.  The break, continue, and throw continuations must be adjusted to pop the environment
(define interpret-block
  (lambda (statement environment return break continue throw)
    (pop-frame (interpret-statement-list (cdr statement)
                                         (push-frame environment)
                                         return
                                         (lambda (env) (break (pop-frame env)))
                                         (lambda (env) (continue (pop-frame env)))
                                         (lambda (v env) (throw v (pop-frame env)))))))

; We use a continuation to throw the proper value. Because we are not using boxes, the environment/state must be thrown as well so any environment changes will be kept
(define interpret-throw
  (lambda (statement environment throw)
    (throw (eval-expression (get-expr statement) environment throw) environment)))

; Interpret a try-catch-finally block

; Create a continuation for the throw.  If there is no catch, it has to interpret the finally block, and once that completes throw the exception.
;   Otherwise, it interprets the catch block with the exception bound to the thrown value and interprets the finally block when the catch is done
(define create-throw-catch-continuation
  (lambda (catch-statement environment return break continue throw jump finally-block)
    (cond
      ((null? catch-statement) (lambda (ex env) (throw ex (interpret-block finally-block env return break continue throw)))) 
      ((not (eq? 'catch (statement-type catch-statement))) (myerror "Incorrect catch statement"))
      (else (lambda (ex env)
              (jump (interpret-block finally-block
                                     (pop-frame (interpret-statement-list 
                                                 (get-body catch-statement) 
                                                 (insert (catch-var catch-statement) ex (push-frame env))
                                                 return 
                                                 (lambda (env2) (break (pop-frame env2))) 
                                                 (lambda (env2) (continue (pop-frame env2))) 
                                                 (lambda (v env2) (throw v (pop-frame env2)))))
                                     return break continue throw)))))))

; To interpret a try block, we must adjust  the return, break, continue continuations to interpret the finally block if any of them are used.
;  We must create a new throw continuation and then interpret the try block with the new continuations followed by the finally block with the old continuations
(define interpret-try
  (lambda (statement environment return break continue throw)
    (call/cc
     (lambda (jump)
       (let* ((finally-block (make-finally-block (get-finally statement)))
              (try-block (make-try-block (get-try statement)))
              (new-return (lambda (v) (begin (interpret-block finally-block environment return break continue throw) (return v))))
              (new-break (lambda (env) (break (interpret-block finally-block env return break continue throw))))
              (new-continue (lambda (env) (continue (interpret-block finally-block env return break continue throw))))
              (new-throw (create-throw-catch-continuation (get-catch statement) environment return break continue throw jump finally-block)))
         (interpret-block finally-block
                          (interpret-block try-block environment new-return new-break new-continue new-throw)
                          return break continue throw))))))

; helper methods so that I can reuse the interpret-block method on the try and finally blocks
(define make-try-block
  (lambda (try-statement)
    (cons 'begin try-statement)))

(define make-finally-block
  (lambda (finally-statement)
    (cond
      ((null? finally-statement) '(begin))
      ((not (eq? (statement-type finally-statement) 'finally)) (myerror "Incorrectly formatted finally block"))
      (else (cons 'begin (cadr finally-statement))))))

; Evaluates all possible boolean and arithmetic expressions, including constants and variables.
(define eval-expression
  (lambda (expr environment throw)
    (cond
      ((number? expr) expr)
      ((eq? expr 'true) #t)
      ((eq? expr 'false) #f)
      ((not (list? expr)) (lookup expr environment))
      ; if the value is like (funcall (dot (new A) f) 3 5)
      ((eq? 'funcall (operator expr)) (interpret-funcall expr environment throw))
      ; if the value is like (dot A x)
      ((eq? 'dot (operator expr)) (interpret-dot expr environment throw))
      (else (eval-operator expr environment throw)))))

; Evaluate a binary (or unary) operator.  Although this is not dealing with side effects, I have the routine evaluate the left operand first and then
; pass the result to eval-binary-op2 to evaluate the right operand.  This forces the operands to be evaluated in the proper order in case you choose
; to add side effects to the interpreter
(define eval-operator
  (lambda (expr environment throw)
    (cond
      ((eq? '! (operator expr)) (not (eval-expression (operand1 expr) environment throw)))
      ((and (eq? '- (operator expr)) (= 2 (length expr))) (- (eval-expression (operand1 expr) environment throw)))
      (else (eval-binary-op2 expr (eval-expression (operand1 expr) environment throw) environment throw)))))

; Complete the evaluation of the binary operator by evaluating the second operand and performing the operation.
(define eval-binary-op2
  (lambda (expr op1value environment throw)
    (cond
      ((eq? '+ (operator expr)) (+ op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '- (operator expr)) (- op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '* (operator expr)) (* op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '/ (operator expr)) (quotient op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '% (operator expr)) (remainder op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '== (operator expr)) (isequal op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '!= (operator expr)) (not (isequal op1value (eval-expression (operand2 expr) environment throw))))
      ((eq? '< (operator expr)) (< op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '> (operator expr)) (> op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '<= (operator expr)) (<= op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '>= (operator expr)) (>= op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '|| (operator expr)) (or op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '&& (operator expr)) (and op1value (eval-expression (operand2 expr) environment throw)))
      (else (myerror "Unknown operator:" (operator expr))))))

; Determines if two values are equal.  We need a special test because there are both boolean and integer types.
(define isequal
  (lambda (val1 val2)
    (if (and (number? val1) (number? val2))
        (= val1 val2)
        (eq? val1 val2))))


;-----------------
; HELPER FUNCTIONS
;-----------------

; These helper functions define the operator and operands of a value expression
(define operator car)
(define operand1 cadr)
(define operand2 caddr)
(define operand3 cadddr)

(define exists-operand2?
  (lambda (statement)
    (not (null? (cddr statement)))))

(define exists-operand3?
  (lambda (statement)
    (not (null? (cdddr statement)))))

; these helper functions define the parts of the various statement types
(define statement-type operator)
(define get-expr operand1)
(define get-declare-var operand1)
(define get-declare-value operand2)
(define exists-declare-value? exists-operand2?)
(define get-assign-lhs operand1)
(define get-assign-rhs operand2)
(define get-condition operand1)
(define get-then operand2)
(define get-else operand3)
(define get-body operand2)
(define exists-else? exists-operand3?)
(define get-try operand1)
(define get-catch operand2)
(define get-finally operand3)

(define catch-var
  (lambda (catch-statement)
    (car (operand1 catch-statement))))


;------------------------
; Environment/State Functions
;------------------------

; create a new empty environment
(define newenvironment
  (lambda ()
    (list (newframe))))

; create an empty frame: a frame is two lists, the first are the variables and the second is the "store" of values
(define newframe
  (lambda ()
    '(() ())))

; add a frame onto the top of the environment
(define push-frame
  (lambda (environment)
    (cons (newframe) environment)))

; remove a frame from the environment
(define pop-frame
  (lambda (environment)
    (cdr environment)))

; some abstractions
(define topframe car)
(define remainingframes cdr)

; does a variable exist in the environment?
(define exists?
  (lambda (var environment)
    (cond
      ((null? environment) #f)
      ((exists-in-list? var (variables (topframe environment))) #t)
      (else (exists? var (remainingframes environment))))))

; does a variable exist in a list?
(define exists-in-list?
  (lambda (var l)
    (cond
      ((null? l) #f)
      ((eq? var (car l)) #t)
      (else (exists-in-list? var (cdr l))))))

; Looks up a value in the environment.  If the value is a boolean, it converts our languages boolean type to a Scheme boolean type
(define lookup
  (lambda (var environment)
    (lookup-variable var environment)))
  
; A helper function that does the lookup.  Returns an error if the variable does not have a legal value
(define lookup-variable
  (lambda (var environment)
    (let ((value (lookup-in-env var environment)))
      (if (eq? 'novalue value)
          (myerror "error: variable without an assigned value:" var)
          value))))

; Return the value bound to a variable in the environment
(define lookup-in-env
  (lambda (var environment)
    (cond
      ((null? environment) (myerror "error: undefined variable" var))
      ((exists-in-list? var (variables (topframe environment))) (lookup-in-frame var (topframe environment)))
      (else (lookup-in-env var (cdr environment))))))

; Return the value bound to a variable in the frame
(define lookup-in-frame
  (lambda (var frame)
    (cond
      ((not (exists-in-list? var (variables frame))) (myerror "error: undefined variable" var))
      (else (language->scheme (get-value (indexof var (variables frame)) (store frame)))))))

; Get the location of a name in a list of names
(define indexof
  (lambda (var l)
    (cond
      ((null? l) 0)  ; should not happen
      ((eq? var (car l)) 0)
      (else (+ 1 (indexof var (cdr l)))))))

; Get the value stored at a given index in the list
(define get-value
  (lambda (n l)
    (cond
      ((zero? n) (car l))
      (else (get-value (- n 1) (cdr l))))))

; Adds a new variable/value binding pair into the environment.  Gives an error if the variable already exists in this frame.
(define insert
  (lambda (var val environment)
    (if (exists-in-list? var (variables (car environment)))
        (myerror "error: variable is being re-declared:" var)
        (cons (add-to-frame var val (car environment)) (cdr environment)))))

; Changes the binding of a variable to a new value in the environment.  Gives an error if the variable does not exist.
(define update
  (lambda (var val environment)
    (if (exists? var environment)
        (update-existing var val environment)
        (myerror "error: variable used but not defined:" var))))

; Add a new variable/value pair to the frame.
(define add-to-frame
  (lambda (var val frame)
    (list (cons var (variables frame)) (cons (scheme->language val) (store frame)))))

; Changes the binding of a variable in the environment to a new value
(define update-existing
  (lambda (var val environment)
    (if (exists-in-list? var (variables (car environment)))
        (cons (update-in-frame var val (topframe environment)) (remainingframes environment))
        (cons (topframe environment) (update-existing var val (remainingframes environment))))))

; Changes the binding of a variable in the frame to a new value.
(define update-in-frame
  (lambda (var val frame)
    (list (variables frame) (update-in-frame-store var val (variables frame) (store frame)))))

; Changes a variable binding by placing the new value in the appropriate place in the store
(define update-in-frame-store
  (lambda (var val varlist vallist)
    (cond
      ((eq? var (car varlist)) (cons (scheme->language val) (cdr vallist)))
      (else (cons (car vallist) (update-in-frame-store var val (cdr varlist) (cdr vallist)))))))

; Returns the list of variables from a frame
(define variables
  (lambda (frame)
    (car frame)))

; Returns the store from a frame
(define store
  (lambda (frame)
    (cadr frame)))

; Functions to convert the Scheme #t and #f to our languages true and false, and back.

(define language->scheme
  (lambda (v) 
    (cond 
      ((eq? v 'false) #f)
      ((eq? v 'true) #t)
      (else v))))

(define scheme->language
  (lambda (v)
    (cond
      ((eq? v #f) 'false)
      ((eq? v #t) 'true)
      (else v))))



; Because the error function is not defined in R5RS scheme, I create my own:
(define error-break (lambda (v) v))
(call-with-current-continuation (lambda (k) (set! error-break k)))

(define myerror
  (lambda (str . vals)
    (letrec ((makestr (lambda (str vals)
                        (if (null? vals)
                            str
                            (makestr (string-append str (string-append " " (symbol->string (car vals)))) (cdr vals))))))
      (error-break (display (string-append str (makestr "" vals)))))))

(define-syntax debug
  (lambda (syn)
    (define slist (syntax->list syn))
    (datum->syntax syn `(let ((x ,(cadr slist)))
                          (begin (print x) (newline) x)))))

