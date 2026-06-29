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
;;              ::= symbol <identifier>
;;                  <sym-decl-exp(id)>
;;
;;              ::= simplificar(<expression>)
;;                  <simplificar-exp(exp)>
;;
;;              ::= evaluar(<expression>, <identifier> = <expression>
;;                          {, <identifier> = <expression>}*)
;;                  <evaluar-exp(exp id rhs ids rhss)>
;;
;;
;              ::= vacio
;;                 <vacio-exp>
;;                  Representa la lista vacía (equivalente a '() en Racket).
;;
;;              ::= vacio?(<expression>)
;;                  <vacio?-exp(exp)>
;;                  Devuelve true si la lista está vacía, false en caso contrario.
;;
;;              ::= crear-lista(<expression>, <expression>)
;;                  <crear-lista-exp(elem lst)>
;;                  Crea una nueva lista añadiendo elem al inicio de lst.
;;                  Equivale a cons en Racket.
;;
;;              ::= lista?(<expression>)
;;                  <lista?-exp(exp)>
;;                  Devuelve true si el valor x es una lista válida (incluyendo vacio).
;;
;;              ::= cabeza(<expression>)
;;                  <cabeza-exp(lst)>
;;                  Devuelve el primer elemento de la lista.
;;                  Si está vacía, devuelve nulo o genera un error.
;;
;;              ::= cola(<expression>)
;;                  <cola-exp(lst)>
;;                  Devuelve una nueva lista con todos los elementos excepto el primero.
;;                  Si hay un solo elemento, devuelve vacio.
;;
;;              ::= append(<expression>, <expression>)
;;                  <append-exp(lst1 lst2)>
;;                  Concatena las listas lst1 y lst2, devolviendo una nueva lista.
;;                  Equivale a append en Racket. No modifica las listas originales.
;;
;;              ::= ref-list(<expression>, <expression>)
;;                  <ref-list-exp(lst i)>
;;                  Devuelve el elemento en la posición i (índices desde 0).
;;                  Si el índice es inválido, devuelve nulo.
;;
;;              ::= set-list(<expression>, <expression>, <expression>)
;;                  <set-list-exp(lst i valor)>
;;                  Reemplaza el elemento en la posición i por valor.
;;                  Devuelve la lista modificada (nueva lista, no mutación).
;;
;;              ::= [ {<expression>}*(,) ]
;;                  <list-lit-exp(exps)>
;;                  Crea una lista a partir de una secuencia de expresiones separadas por comas.
;;                  Equivale a crear-lista anidados terminados en vacio.
;;                  Ejemplo: [1, 2, 3]  =>  crear-lista(1, crear-lista(2, crear-lista(3, vacio)))
;;
;;              ::= longitud(<expression>)
;;                  <longitud-exp(exp)>
;;                  Devuelve la cantidad de caracteres de una cadena
;;                  o la cantidad de elementos de una lista.
;;
;;
;; <primitive> ::= + | - | * | / | % | == | <> | < | > | <= | >= | and | or | not

;******************************************************************************************

;******************************************************************************************
;Especificación Léxica

(define scanner-spec-simple-interpreter
'((white-sp
   (whitespace) skip)
  (comment
   ("#" (arbno (not #\newline))) skip)
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

    (expression
     ("print" "(" expression ")")
     print-exp)
    
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
     ("switch" expression "{"
               (arbno "case" expression ":" expression)
               "default" ":" expression "}")
     switch-exp)

    
    ;;Simbolos 2.3

    (expression
     ("symbol" identifier)
     sym-decl-exp)

    (expression
     ("simplificar" "(" expression ")")
     simplificar-exp)

    (expression
     ("evaluar" "(" expression ","
                identifier "=" expression
                (arbno "," identifier "=" expression) ")")
     evaluar-exp)


    ;; Sección 4 – Listas: producciones de la gramática
    ;; ------------------------------------------------------------------

    ;; vacio: representa la lista vacía (constante terminal)
    (expression ("vacio") vacio-exp)

    ;; vacio?(lst): devuelve true si lst es la lista vacía
    (expression ("vacio?" "(" expression ")") vacio?-exp)

    ;; crear-lista(elem, lst): inserta elem al inicio de lst (equivale a cons)
    (expression ("crear-lista" "(" expression "," expression ")") crear-lista-exp)

    ;; lista?(x): devuelve true si x es una lista válida (listval o vacio)
    (expression ("lista?" "(" expression ")") lista?-exp)

    ;; cabeza(lst): retorna el primer elemento de la lista
    (expression ("cabeza" "(" expression ")") cabeza-exp)

    ;; cola(lst): retorna la lista sin su primer elemento
    (expression ("cola" "(" expression ")") cola-exp)

    ;; append(lst1, lst2): concatena lst1 y lst2 en una nueva lista
    (expression ("append" "(" expression "," expression ")") append-exp)

    ;; ref-list(lst, i): devuelve el elemento en la posición i (desde 0)
    ;;                   devuelve nulo si el índice es inválido
    (expression ("ref-list" "(" expression "," expression ")") ref-list-exp)

    ;; set-list(lst, i, valor): devuelve una nueva lista con la posición i
    ;;                          reemplazada por valor
    (expression ("set-list" "(" expression "," expression "," expression ")") set-list-exp)


    ;; [e1, e2, ..., eN]: sintaxis literal de lista
    ;; Construye una listval equivalente a crear-lista anidados terminados en vacio.
    ;; Una lista vacía se escribe [].  Nota: se usa arbno con "," para 0 o más elementos.
    (expression ("[" (separated-list expression ",") "]") list-lit-exp)

    ;; longitud(exp): longitud de una cadena (número de caracteres)
    ;;                o de una lista (número de elementos)
    (expression ("longitud" "(" expression ")") longitud-exp)
    

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
    (expression ("not" "(" expression ")" ) not-exp)
    
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
;Definicion de expresiones simbolicas

(define-datatype symval symval?
  (sym-atom
   (name symbol?))
  (sym-op
   (op symbol?)          ; uno de: + - * /
   (left scheme-value?)
   (right scheme-value?)))

;**************************************************************************************

;Definición del tipo de dato para Listas (Sección 4)
;
; Una lista MathFlow es un valor de tipo listval con dos variantes:
;   - empty-list : la lista vacía (equivale a '() / vacio)
;   - cons-cell  : una celda con un elemento (head) y el resto (tail),
;                  donde tail debe ser también un listval.
;                  Equivale al cons de Racket.

(define-datatype listval listval?
  (empty-list)                              ; vacio
  (cons-cell                               ; crear-lista(elem, lst)
   (head scheme-value?)
   (tail listval?)))


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
                 (if (or (symval? l) (symval? r))
                     ; Al menos un operando es simbólico
                     (if (prim-aritmetica? op)
                         (sym-op (prim->sym op) l r)
                         (eopl:error 'primitive-exp
                                     "Operador no aritmetico sobre expresion simbolica: ~s" op))
                     ; Ambos son valores concretos: evaluación normal
                     (cases primitive op
                       (add-op ()
                               (cond
                                 ((and (number? l) (number? r)) (+ l r))
                                 ((and (string? l) (string? r)) (string-append l r))
                                 (else (eopl:error 'add-op "Tipos incompatibles: ~s y ~s" l r))))
                       (sub-op () (- l r))
                       (mul-op () (* l r))
                       (div-op () (/ l r))
                       (mod-op () (modulo l r))
                       (eq-op  () (equal? l r))
                       (neq-op () (not (equal? l r)))
                       (lt-op  () (< l r))
                       (gt-op  () (> l r))
                       (lte-op () (<= l r))
                       (gte-op () (>= l r))
                       (and-op () (and (true-value? l) (true-value? r)))
                       (or-op  () (or  (true-value? l) (true-value? r)))))))
      (add1-exp (exp)
          (+ (eval-expression exp env) 1))

      (sub1-exp (exp)
          (- (eval-expression exp env) 1))

      (not-exp (exp)
          (not (true-value? (eval-expression exp env))))
      
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
              (let loop ((ids  (cons id ids))
                         (rhss (cons rhs rhss))
                         (env  env))
                (if (null? ids)
                    env
                    (let ((cur-id (car ids)))
                      ; Chequeo: no puede declararse var si ya existe como símbolo
                      (if (and (env-bound? env cur-id)
                               (symval? (apply-env env cur-id)))
                          (eopl:error 'var-decl-exp
                                      "~s ya existe como simbolo algebraico; no puede ser variable"
                                      cur-id)
                          (loop (cdr ids)
                                (cdr rhss)
                                (extend-env (list cur-id)
                                            (list (direct-target
                                                   (eval-expression (car rhss) env)))
                                            env)))))))
      (const-decl-exp (id rhs ids rhss)
                (let loop ((ids  (cons id ids))
                           (rhss (cons rhs rhss))
                           (env  env))
                  (if (null? ids)
                      env
                      (let ((cur-id (car ids)))
                        (if (and (env-bound? env cur-id)
                                 (symval? (apply-env env cur-id)))
                            (eopl:error 'const-decl-exp
                                        "~s ya existe como simbolo algebraico; no puede ser constante"
                                        cur-id)
                            (loop (cdr ids)
                                  (cdr rhss)
                                  (extend-env (list cur-id)
                                              (list (const-target
                                                     (eval-expression (car rhss) env)))
                                              env)))))))

      (while-exp (test-exp body-exp)
           (let loop ()
             (if (true-value? (eval-expression test-exp env))
                 (begin
                   (eval-expression body-exp env)
                   (loop))
                 'done)))

      (for-exp (id list-exp body-exp)
         (let ((lst (eval-expression list-exp env)))
           (if (not (listval? lst))
               (eopl:error 'for-exp
                           "Se esperaba una lista para iterar, se recibio: ~s" lst)
               (let loop ((l lst))
                 (cases listval l
                   (empty-list () 'done)
                   (cons-cell (h t)
                              (eval-expression body-exp
                                               (extend-env (list id)
                                                           (list (direct-target h))
                                                           env))
                              (loop t)))))))

      (func-def-exp (name param params body-exps return-exps)
                    (let ((all-params (cons param params)))
                      (extend-env-recursively-ext
                       name
                       all-params
                       body-exps
                       return-exps
                       env)))
      (print-exp (exp)
           (let ((val (eval-expression exp env)))
             (print-val val)
             (newline)
             '---Fin-Operacion---))
      (switch-exp (test-exp cases-exps cases-bodies default-exp)
            (let ((val (eval-expression test-exp env)))
              (let loop ((cases cases-exps)
                         (bodies cases-bodies))
                (if (null? cases)
                    (eval-expression default-exp env)
                    (if (equal? val (eval-expression (car cases) env))
                        (eval-expression (car bodies) env)
                        (loop (cdr cases) (cdr bodies)))))))


      ; symbol x → extiende el ambiente con x ligado a sym-atom(x)
      ; Error si x ya existe (no puede ser variable y símbolo a la vez)
      (sym-decl-exp (id)
                    (if (env-bound? env id)
                        (eopl:error 'sym-decl-exp
                                    "~s ya existe en el ambiente; no puede ser simbolo" id)
                        (extend-env (list id)
                                    (list (direct-target (sym-atom id)))
                                    env)))

      ; simplificar(exp) → aplica reglas algebraicas recursivamente
      (simplificar-exp (exp)
                       (simplificar (eval-expression exp env)))

      ; evaluar(exp, x=v1, y=v2...) → sustituye símbolos y reduce
      (evaluar-exp (exp id rhs ids rhss)
                   (let* ((all-ids  (cons id ids))
                          (all-rhss (cons rhs rhss))
                          (sust-vals (map (lambda (e) (eval-expression e env)) all-rhss))
                          (val       (eval-expression exp env)))
                     (evaluar-sym val all-ids sust-vals)))


      ;; Sección 4 – Evaluación de Listas
      ;; ----------------------------------------------------------------

      ;; vacio → devuelve la lista vacía (empty-list)
      (vacio-exp ()
        (empty-list))

      ;; vacio?(lst) → true si lst es empty-list, false si es cons-cell
      (vacio?-exp (lst-exp)
        (let ((lst (eval-expression lst-exp env)))
          (if (listval? lst)
              (cases listval lst
                (empty-list () #t)
                (cons-cell (h t) #f))
              (eopl:error 'vacio?-exp
                          "Se esperaba una lista, se recibio: ~s" lst))))

      ;; crear-lista(elem, lst) → cons-cell con elem como cabeza y lst como cola
      ;; Si lst no es un listval (por ejemplo vacio), genera error descriptivo
      (crear-lista-exp (elem-exp lst-exp)
        (let ((elem (eval-expression elem-exp env))
              (lst  (eval-expression lst-exp env)))
          (if (listval? lst)
              (cons-cell elem lst)
              (eopl:error 'crear-lista-exp
                          "El segundo argumento debe ser una lista, se recibio: ~s" lst))))

      ;; lista?(x) → true si x es cualquier listval (empty-list o cons-cell)
      (lista?-exp (exp)
        (listval? (eval-expression exp env)))

      ;; cabeza(lst) → primer elemento de la lista
      ;; Error si la lista está vacía
      (cabeza-exp (lst-exp)
        (let ((lst (eval-expression lst-exp env)))
          (if (listval? lst)
              (cases listval lst
                (empty-list ()
                  (eopl:error 'cabeza-exp "No se puede obtener la cabeza de una lista vacia"))
                (cons-cell (h t) h))
              (eopl:error 'cabeza-exp
                          "Se esperaba una lista, se recibio: ~s" lst))))

      ;; cola(lst) → lista sin el primer elemento
      ;; Si hay un solo elemento, devuelve empty-list (vacio)
      (cola-exp (lst-exp)
        (let ((lst (eval-expression lst-exp env)))
          (if (listval? lst)
              (cases listval lst
                (empty-list ()
                  (eopl:error 'cola-exp "No se puede obtener la cola de una lista vacia"))
                (cons-cell (h t) t))
              (eopl:error 'cola-exp
                          "Se esperaba una lista, se recibio: ~s" lst))))


      ;; append(lst1, lst2) → concatena lst1 y lst2 en una nueva lista
      ;; Recorre lst1 recursivamente y al llegar al final enlaza con lst2.
      ;; No modifica ninguna de las dos listas originales.
      (append-exp (lst1-exp lst2-exp)
        (let ((lst1 (eval-expression lst1-exp env))
              (lst2 (eval-expression lst2-exp env)))
          (if (not (listval? lst1))
              (eopl:error 'append-exp
                          "El primer argumento debe ser una lista, se recibio: ~s" lst1)
              (if (not (listval? lst2))
                  (eopl:error 'append-exp
                              "El segundo argumento debe ser una lista, se recibio: ~s" lst2)
                  ;; append-aux: recorre lst1 hasta el final y lo enlaza con lst2
                  (let loop ((l lst1))
                    (cases listval l
                      (empty-list () lst2)
                      (cons-cell (h t)
                                 (cons-cell h (loop t)))))))))
      

      ;; ref-list(lst, i) → elemento en la posición i (índices desde 0)
      ;; Devuelve nulo si i < 0 o i >= longitud de la lista
      (ref-list-exp (lst-exp i-exp)
        (let ((lst (eval-expression lst-exp env))
              (i   (eval-expression i-exp env)))
          (if (not (listval? lst))
              (eopl:error 'ref-list-exp
                          "El primer argumento debe ser una lista, se recibio: ~s" lst)
              (if (not (number? i))
                  (eopl:error 'ref-list-exp
                              "El indice debe ser un numero, se recibio: ~s" i)
                  ;; recorre la lista decrementando el índice hasta encontrar la posición
                  (let loop ((l lst) (pos i))
                    (cases listval l
                      (empty-list () '())          ; índice fuera de rango → nulo
                      (cons-cell (h t)
                                 (if (= pos 0)
                                     h
                                     (loop t (- pos 1))))))))))

      
     ;; set-list(lst, i, valor) → nueva lista con la posición i reemplazada por valor
      ;; Devuelve la lista modificada sin mutar la original.
      ;; Si i está fuera de rango, devuelve la lista sin cambios.
      (set-list-exp (lst-exp i-exp val-exp)
        (let ((lst (eval-expression lst-exp env))
              (i   (eval-expression i-exp env))
              (val (eval-expression val-exp env)))
          (if (not (listval? lst))
              (eopl:error 'set-list-exp
                          "El primer argumento debe ser una lista, se recibio: ~s" lst)
              (if (not (number? i))
                  (eopl:error 'set-list-exp
                              "El indice debe ser un numero, se recibio: ~s" i)
                  ;; recorre la lista reconstruyéndola; en la posición i pone val
                  (let loop ((l lst) (pos i))
                    (cases listval l
                      (empty-list () (empty-list))   ; índice fuera de rango
                      (cons-cell (h t)
                                 (if (= pos 0)
                                     (cons-cell val t)
                                     (cons-cell h (loop t (- pos 1)))))))))))

      ; [e1, e2, ..., eN] → listval equivalente a crear-lista anidados
      ;; Evalúa cada expresión de izquierda a derecha y construye la lista
      ;; en orden, terminando en empty-list.  Una lista vacía [] devuelve empty-list.
      (list-lit-exp (exps)
        (let loop ((elems (map (lambda (e) (eval-expression e env)) exps)))
          (if (null? elems)
              (empty-list)
              (cons-cell (car elems) (loop (cdr elems))))))

      ;; longitud(exp) → número de caracteres si exp es string,
      ;;                  número de elementos si exp es listval
      (longitud-exp (exp)
        (let ((val (eval-expression exp env)))
          (cond
            ((string? val)  (string-length val))
            ((listval? val)
             (let loop ((l val) (n 0))
               (cases listval l
                 (empty-list  ()    n)
                 (cons-cell   (h t) (loop t (+ n 1))))))
            (else
             (eopl:error 'longitud-exp
                         "longitud espera una cadena o lista, se recibio: ~s" val)))))
      
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
;Simplificación de expresiones simbólicas

;simplificar: expval -> expval
;aplica reglas algebraicas recursivamente a una expresión simbólica
(define simplificar
  (lambda (val)
    (if (not (symval? val))
        val  ; número u otro valor concreto: ya está en forma mínima
        (cases symval val
          (sym-atom (name) val)
          (sym-op (op left right)
                  ; primero simplificar hijos (bottom-up, como indica el PDF)
                  (let ((l (simplificar left))
                        (r (simplificar right)))
                    (aplicar-reglas op l r)))))))

;aplicar-reglas: symbol expval expval -> expval
;aplica las reglas del PDF para cada operador
(define aplicar-reglas
  (lambda (op l r)
    (cond
      ;; ---- SUMA ----
      ((eq? op '+)
       (cond
         ((and (number? r) (= r 0)) l)                  ; x + 0 → x
         ((and (number? l) (= l 0)) r)                  ; 0 + x → x
         ((and (number? l) (number? r)) (+ l r))        ; c1 + c2 → evaluar
         ; ((x + c1) + c2) → (x + (c1+c2))
         ((and (symval? l) (number? r))
          (cases symval l
            (sym-op (op2 ll lr)
                    (if (and (eq? op2 '+) (number? lr))
                        (aplicar-reglas '+ ll (+ lr r))
                        (sym-op '+ l r)))
            (else (sym-op '+ l r))))
         ; (c1 + (x + c2)) → ((c1+c2) + x)
         ((and (number? l) (symval? r))
          (cases symval r
            (sym-op (op2 rl rr)
                    (if (and (eq? op2 '+) (number? rl))
                        (aplicar-reglas '+ (+ l rl) rr)
                        (sym-op '+ l r)))
            (else (sym-op '+ l r))))
         (else (sym-op '+ l r))))

      ;; ---- RESTA ----
      ((eq? op '-)
       (cond
         ((and (number? r) (= r 0)) l)                  ; x - 0 → x
         ((and (number? l) (number? r)) (- l r))        ; c1 - c2 → evaluar
         (else (sym-op '- l r))))

      ;; ---- MULTIPLICACIÓN ----
      ((eq? op '*)
       (cond
         ((and (number? r) (= r 1)) l)                  ; x * 1 → x
         ((and (number? l) (= l 1)) r)                  ; 1 * x → x
         ((and (number? r) (= r 0)) 0)                  ; x * 0 → 0
         ((and (number? l) (= l 0)) 0)                  ; 0 * x → 0
         ((and (number? l) (number? r)) (* l r))        ; c1 * c2 → evaluar
         ; ((x * c1) * c2) → (x * (c1*c2))
         ((and (symval? l) (number? r))
          (cases symval l
            (sym-op (op2 ll lr)
                    (if (and (eq? op2 '*) (number? lr))
                        (aplicar-reglas '* ll (* lr r))
                        (sym-op '* l r)))
            (else (sym-op '* l r))))
         ; (c1 * (x * c2)) → ((c1*c2) * x)
         ((and (number? l) (symval? r))
          (cases symval r
            (sym-op (op2 rl rr)
                    (if (and (eq? op2 '*) (number? rl))
                        (aplicar-reglas '* (* l rl) rr)
                        (sym-op '* l r)))
            (else (sym-op '* l r))))
         (else (sym-op '* l r))))

      ;; ---- DIVISIÓN ----
      ((eq? op '/)
       (cond
         ((and (number? r) (= r 1)) l)                  ; x / 1 → x
         ((and (number? l) (= l 0)) 0)                  ; 0 / x → 0
         ((and (number? l) (number? r)) (/ l r))        ; c1 / c2 → evaluar
         (else (sym-op '/ l r))))

      (else (sym-op op l r)))))

;Evaluación/sustitución de expresiones simbólicas

;evaluar-sym: expval list-of-symbol list-of-expval -> expval
;sustituye los símbolos por sus valores y simplifica el resultado
(define evaluar-sym
  (lambda (val ids sust-vals)
    (if (not (symval? val))
        val  ; valor concreto: no hay nada que sustituir
        (cases symval val
          (sym-atom (name)
                    (let ((pos (list-find-position name ids)))
                      (if (number? pos)
                          (list-ref sust-vals pos)   ; sustituir por el valor dado
                          val)))                      ; símbolo sin sustitución: queda igual
          (sym-op (op left right)
                  (let ((l (evaluar-sym left  ids sust-vals))
                        (r (evaluar-sym right ids sust-vals)))
                    ; después de sustituir, simplificar el nodo resultante
                    (simplificar (sym-op op l r))))))))


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
        (procval? x)
        (symval? x) ;;Simbolos
        (listval? x) ;;Listas
        )))



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

;env-bound?: environment symbol -> boolean
;verifica si un símbolo ya existe en el ambiente sin lanzar error
(define env-bound?
  (lambda (env sym)
    (cases environment env
      (empty-env-record () #f)
      (extended-env-record (syms vals env)
                           (if (number? (rib-find-position sym syms))
                               #t
                               (env-bound? env sym))))))


;;Funciones auxiliares para simbolos

;prim-aritmetica?: primitive -> boolean
;determina si una primitiva es aritmética (válida en expresiones simbólicas)
(define prim-aritmetica?
  (lambda (op)
    (cases primitive op
      (add-op () #t)
      (sub-op () #t)
      (mul-op () #t)
      (div-op () #t)
      (else   #f))))

;prim->sym: primitive -> symbol
;convierte el tipo primitiva al símbolo del operador para almacenarlo en sym-op
(define prim->sym
  (lambda (op)
    (cases primitive op
      (add-op () '+)
      (sub-op () '-)
      (mul-op () '*)
      (div-op () '/)
      (else (eopl:error 'prim->sym "Operador no aritmetico")))))


;print-symval: symval -> void
;imprime una expresión simbólica de forma legible
(define print-symval
  (lambda (sv)
    (cases symval sv
      (sym-atom (name)
                (display name))
      (sym-op (op left right)
              (display "(")
              (print-val left)
              (display " ")
              (display op)
              (display " ")
              (print-val right)
              (display ")")))))




;print-listval: listval -> void
;imprime una lista MathFlow en formato [e1, e2, ..., eN]
;La lista vacía se imprime como []
(define print-listval
  (lambda (lst)
    (display "[")
    (let loop ((lst lst) (first #t))
      (cases listval lst
        (empty-list () '())
        (cons-cell (h t)
                   (if (not first) (display ", ") '())
                   (print-val h)
                   (loop t #f))))
    (display "]")))

;print-val: expval -> void
;imprime cualquier valor expresado, incluyendo simbólicos
(define print-val
  (lambda (val)
    (cond
      ((listval? val) (print-listval val))   ;; Sección 4: primero listas (antes del else)
      ((symval? val)  (print-symval val))
      ((boolean? val) (display (if val "true" "false")))
      ((null? val)    (display "null"))
      (else           (display val)))))

;******************************************************************************************

(interpretador)