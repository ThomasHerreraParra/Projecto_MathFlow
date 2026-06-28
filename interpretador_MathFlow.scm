#lang eopl

;******************************************************************************************
;; La definición BNF para las expresiones del lenguaje:
;; <program> ::= <expression>
;;               <a-program(body)>
;;
;; <expression> ::= <number>
;;                  <lit-exp(datum)>
;;
;;              ::= <string>
;;                  <string-exp(s)>
;;
;;              ::= true
;;                  <true-exp>
;;
;;              ::= false
;;                  <false-exp>
;;
;;              ::= null
;;                  <null-exp>
;;
;;              ::= <identifier>
;;                  <id-exp(id, id-var-tail)>
;;
;;              ::= <identifier> = <expression>
;;                  <id-exp(id, id-assign-tail)>
;;
;;              ::= <identifier>(<expression> {, <expression>}*)
;;                  <id-exp(id, id-call-tail)>
;;
;;              ::= if <expression> then <expression> else <expression> end
;;                  <if-exp(test-exp true-exp false-exp)>
;;
;;              ::= begin <expression> {; <expression>}* end
;;                  <begin-exp(exp exps)>
;;
;;              ::= var <identifier> = <expression>
;;                  <var-decl-exp(id rhs)>
;;
;;              ::= const <identifier> = <expression>
;;                  <const-decl-exp(id rhs)>
;;
;;              ::= while <expression> do <expression> done
;;                  <while-exp(test-exp body-exp)>
;;
;;              ::= for <identifier> in <expression> do <expression> done
;;                  <for-exp(id list-exp body-exp)>
;;
;;              ::= func <identifier>(<identifier> {, <identifier>}*) {
;;                     {<expression> ;}*
;;                     {return <expression> ;}*
;;                  }
;;                  <func-def-exp(name params body-exps return-exps)>
;;
;;              ::= print(<expression>)
;;                  <print-exp(exp)>
;;
;;              ::= switch <expression> {
;;                     {case <expression> : <expression>}*
;;                     default : <expression>
;;                  }
;;                  <switch-exp(test-exp cases-exps cases-bodies default-exp)>
;;
;;              ::= (<expression> <primitive> <expression>)
;;                  <primitive-exp(left op right)>
;;
;;              ::= add1(<expression>)
;;                  <add1-exp(exp)>
;;
;;              ::= sub1(<expression>)
;;                  <sub1-exp(exp)>
;;
;; <primitive> ::= + | - | * | / | % | == | <> | < | > | <= | >= | and | or

;******************************************************************************************

;******************************************************************************************
;Especificación Léxica

(define scanner-spec-simple-interpreter
'((white-sp
   (whitespace) skip)
  (comment
   ("%" (arbno (not #\newline))) skip)
  (identifier
   (letter (arbno (or letter digit "?"))) symbol)
  (number
   (digit (arbno digit)) number)
  (number
   ("-" digit (arbno digit)) number)
  (number
     (digit (arbno digit) "." (arbno digit)) number)
  (number
     ("-" digit (arbno digit) "." (arbno digit)) number)
  (string
     ("\"" (arbno (not #\")) "\"") string)))

;Especificación Sintáctica (gramática)

(define grammar-simple-interpreter
  '((program (expression) a-program)
    (expression (number) lit-exp)
    
    (expression (identifier id-tail) id-exp)
    (id-tail ("(" expression (arbno "," expression) ")") id-call-tail)
    (id-tail ("=" expression) id-assign-tail)
    (id-tail () id-var-tail)
    
    (expression (string) string-exp)
    (expression ("true") true-exp)
    (expression ("false") false-exp)
    (expression ("null") null-exp)
    
    (expression ("if" expression "then" expression "else" expression "end")
                if-exp)

    (expression ("begin" expression (arbno ";" expression) "end")
                begin-exp)

    (expression
     ("var" identifier "=" expression (arbno "," identifier "=" expression) "endvar")
     var-decl-exp)

    (expression
     ("const" identifier "=" expression (arbno "," identifier "=" expression) "endconst")
     const-decl-exp)
    
    (expression
     ("while" expression "do" expression "done")
     while-exp)

    (expression
     ("for" identifier "in" expression "do" expression "done")
     for-exp)

    (expression
     ("func" identifier "(" identifier (arbno "," identifier) ")" "{"
             (arbno expression ";")
             (arbno "return" expression ";")
             "}")
     func-def-exp)

    (expression
     ("print" "(" expression ")")
     print-exp)

    (expression
     ("switch" expression "{"
               (arbno "case" expression ":" expression)
               "default" ":" expression "}")
     switch-exp)

;;  --------------------------------------------------------------------    
    (expression ("(" expression primitive expression ")") primitive-exp)

    (primitive  ("+") add-op)
    (primitive  ("-") sub-op)
    (primitive  ("*") mul-op)
    (primitive  ("/") div-op)
    (primitive  ("%") mod-op)
    (primitive  ("==") eq-op)
    (primitive  ("<>") neq-op)
    (primitive  ("<") lt-op)
    (primitive  (">") gt-op)
    (primitive  ("<=") lte-op)
    (primitive  (">=") gte-op)
    (primitive  ("and") and-op)
    (primitive  ("or") or-op)

    (expression ("add1" "(" expression ")") add1-exp)
    (expression ("sub1" "(" expression ")") sub1-exp)

    ))


;Tipos de datos para la sintaxis abstracta de la gramática
;Construidos automáticamente:

(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))

;*******************************************************************************************
;Parser, Scanner, Interfaz

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program  pgm)) 
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))

;*******************************************************************************************
;El Interprete

;eval-program: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (body)
                 (eval-expression body (init-env))))))

(define init-env
  (lambda ()
    (extend-env
     '(x y z)
     (list (direct-target 1)
           (direct-target 5)
           (direct-target 10))
     (empty-env))))

;(define init-env
;  (lambda ()
;    (extend-env
;     '(x y z f)
;     (list 4 2 5 (closure '(y) (primapp-exp (mult-prim) (cons (var-exp 'y) (cons (primapp-exp (decr-prim) (cons (var-exp 'y) ())) ())))
;                      (empty-env)))
;     (empty-env))))

;eval-expression: <expression> <enviroment> -> numero
; evalua la expresión en el ambiente de entrada

;**************************************************************************************
;Definición tipos de datos referencia y blanco

(define-datatype target target?
  (direct-target (expval expval?))
  (const-target (expval expval?)) ;;Variables Constantes
  (indirect-target (ref ref-to-direct-target?)))

(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)))

;**************************************************************************************

(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (lit-exp (datum) datum)
      (id-exp (id tail)
        (cases id-tail tail
          (id-call-tail (arg args)
                        (let ((all-args (cons arg args)))
                          (let ((proc (apply-env env id))
                                (arg-vals
                                 (map (lambda (x)
                                        (direct-target (eval-expression x env)))
                                      all-args)))
                            (if (procval? proc)
                                (apply-procedure proc arg-vals)
                                (eopl:error 'eval-expression
                                            "~s no es una funcion"
                                            id)))))

          (id-assign-tail (rhs)
                          (begin
                            (setref!
                             (apply-env-ref env id)
                             (eval-expression rhs env))
                            (eval-expression rhs env)))
          (id-var-tail ()
                       (apply-env env id))))
      (string-exp (s)
            (substring s 1 (- (string-length s) 1)))
      (true-exp () #t)
      (false-exp () #f)
      (null-exp () '())
      (primitive-exp (left op right)
               (let ((l (eval-expression left env))
                     (r (eval-expression right env)))
                 (cases primitive op
                   (add-op ()
                           (cond
                             ((and (number? l) (number? r))
                              (+ l r))
                             ((and (string? l) (string? r))
                              (string-append l r))
                             (else
                              (eopl:error 'add-op
                                          "Tipos incompatibles: ~s y ~s"
                                          l r))))
                   (sub-op () (- l r))
                   (mul-op () (* l r))
                   (div-op () (/ l r))
                   (mod-op () (modulo l r))
                   (eq-op () (equal? l r))
                   (neq-op () (not (equal? l r)))
                   (lt-op () (< l r))
                   (gt-op () (> l r))
                   (lte-op () (<= l r))
                   (gte-op () (>= l r))
                   (and-op () (and (true-value? l) (true-value? r)))
                   (or-op () (or (true-value? l) (true-value? r))))))
      (add1-exp (exp)
          (+ (eval-expression exp env) 1))

      (sub1-exp (exp)
          (- (eval-expression exp env) 1))
      (if-exp (test-exp true-exp false-exp)
              (if (true-value? (eval-expression test-exp env))
                  (eval-expression true-exp env)
                  (eval-expression false-exp env)))
      (begin-exp (exp exps)
           (let loop ((acc (eval-expression exp env))
                      (exps exps)
                      (env env))
             (if (null? exps)
                 acc
                 (let ((new-env (if (environment? acc) acc env)))
                   (loop (eval-expression (car exps) new-env)
                         (cdr exps)
                         new-env)))))
      (var-decl-exp (id rhs ids rhss)
              (let loop ((ids (cons id ids))
                         (rhss (cons rhs rhss))
                         (env env))
                (if (null? ids)
                    env
                    (loop (cdr ids)
                          (cdr rhss)
                          (extend-env (list (car ids))
                                      (list (direct-target (eval-expression (car rhss) env)))
                                      env)))))

      (const-decl-exp (id rhs ids rhss)
                (let loop ((ids (cons id ids))
                           (rhss (cons rhs rhss))
                           (env env))
                  (if (null? ids)
                      env
                      (loop (cdr ids)
                            (cdr rhss)
                            (extend-env (list (car ids))
                                        (list (const-target (eval-expression (car rhss) env)))
                                        env)))))

      (while-exp (test-exp body-exp)
           (let loop ()
             (if (true-value? (eval-expression test-exp env))
                 (begin
                   (eval-expression body-exp env)
                   (loop))
                 'done)))

      (for-exp (id list-exp body-exp)
         (let ((lst (eval-expression list-exp env)))
           (for-each
            (lambda (val)
              (eval-expression body-exp
                               (extend-env (list id)
                                           (list (direct-target val))
                                           env)))
            lst)))

      (func-def-exp (name param params body-exps return-exps)
                    (let ((all-params (cons param params)))
                      (extend-env-recursively-ext
                       name
                       all-params
                       body-exps
                       return-exps
                       env)))
      (print-exp (exp)
                 (eopl:printf "~a~%" (eval-expression exp env))
                 '---Fin-Operacion---) ;;Sustituye el imprimir un espacio en blanco

      (switch-exp (test-exp cases-exps cases-bodies default-exp)
            (let ((val (eval-expression test-exp env)))
              (let loop ((cases cases-exps)
                         (bodies cases-bodies))
                (if (null? cases)
                    (eval-expression default-exp env)
                    (if (equal? val (eval-expression (car cases) env))
                        (eval-expression (car bodies) env)
                        (loop (cdr cases) (cdr bodies)))))))
      )))

; funciones auxiliares para aplicar eval-expression a cada elemento de una 
; lista de operandos (expresiones)
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (cases expression rand
      (id-exp (id tail)
              (cases id-tail tail
                (id-var-tail ()
                             (indirect-target
                              (let ((ref (apply-env-ref env id)))
                                (cases target (primitive-deref ref)
                                  (direct-target (v) ref)
                                  (const-target (v) ref) ;;Constantes
                                  (indirect-target (ref1) ref1)))))
                (else (direct-target (eval-expression rand env)))))
      (else
       (direct-target (eval-expression rand env))))))

(define eval-let-exp-rands
  (lambda (rands env)
    (map (lambda (x) (eval-let-exp-rand x env))
         rands)))

(define eval-let-exp-rand
  (lambda (rand env)
    (direct-target (eval-expression rand env))))

;true-value?: determina si un valor dado corresponde a un valor booleano falso o verdadero
(define true-value?
  (lambda (x)
    (cond
      ((equal? x #f) #f)
      ((equal? x 0) #f)
      ((equal? x "") #f)
      ((equal? x '()) #f)
      (else #t))))

;*******************************************************************************************
;Procedimientos
(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body expression?)
   (env environment?))
  (closure-ext
   (name symbol?)
   (ids (list-of symbol?))
   (body-exps (list-of expression?))
   (return-exps (list-of expression?))
   (env environment?)))

;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env)
               (eval-expression body (extend-env ids args env)))
      (closure-ext (name ids body-exps return-exps env)
                   (let ((new-env (extend-env ids args env)))
                     (for-each (lambda (e) (eval-expression e new-env)) body-exps)
                     (if (null? return-exps)
                         '()
                         (eval-expression (car return-exps) new-env)))))))
;*******************************************************************************************
;Ambientes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vec vector?)
   (env environment?)))

(define scheme-value? (lambda (v) #t))

;empty-env:      -> enviroment
;función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (empty-env-record)))       ;llamado al constructor de ambiente vacío 


;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) env)))

;extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
;función que crea un ambiente extendido para procedimientos recursivos
(define extend-env-recursively-ext
  (lambda (name ids body-exps return-exps old-env)

    (let ((vec (make-vector 1)))

      (let ((env
             (extended-env-record
              (list name)
              vec
              old-env)))

        (vector-set!
         vec
         0
         (direct-target
          (closure-ext
           name
           ids
           body-exps
           return-exps
           env)))

        env))))

;iota: number -> list
;función que retorna una lista de los números desde 0 hasta end
(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
        (cons next (loop (+ 1 next)))))))

;función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
    ;(begin
     ; (display env)
      ;(display "jajajaj ")
      (deref (apply-env-ref env sym))))
    ;)
(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env-ref "No binding for ~s" sym))
      (extended-env-record (syms vals env)
                           (let ((pos (rib-find-position sym syms)))
                             (if (number? pos)
                                 (a-ref pos vals)
                                 (apply-env-ref env sym)))))))

;*******************************************************************************************
;Blancos y Referencias

(define expval?
  (lambda (x)
    (or (number? x)
        (string? x)
        (boolean? x)
        (null? x)
        (procval? x))))

(define ref-to-direct-target?
  (lambda (x)
    (and (reference? x)
         (cases reference x
           (a-ref (pos vec)
                  (cases target (vector-ref vec pos)
                    (direct-target (v) #t)
                    (const-target (v) #t)
                    (indirect-target (v) #f)))))))

(define deref
  (lambda (ref)
    (cases target (primitive-deref ref)
      (direct-target (expval) expval)
      (const-target (expval) expval)
      (indirect-target (ref1)
                       (cases target (primitive-deref ref1)
                         (direct-target (expval) expval)
                         (const-target (expval) expval) ;;Constantes
                         (indirect-target (p)
                                          (eopl:error 'deref
                                                      "Illegal reference: ~s" ref1)))))))

(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))

(define setref!
  (lambda (ref expval)
    (let
        ((ref (cases target (primitive-deref ref)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                (direct-target  (expval1) ref)
                (const-target   (expval1)          ;; Constantes
                  (eopl:error 'setref!
                    "No se puede modificar una constante"))
                (indirect-target (ref1) ref1))))
      (primitive-setref! ref (direct-target expval)))))


(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec)
             (vector-set! vec pos val)))))

;****************************************************************************************
;Funciones Auxiliares

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de un ambiente

(define rib-find-position 
  (lambda (sym los)
    (list-find-position sym los)))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))

;******************************************************************************************

(interpretador)