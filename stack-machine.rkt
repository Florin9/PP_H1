#lang racket
(require "opcodes.rkt")
(provide make-stack-machine)
(provide run-stack-machine)
(provide get-stack)
(provide get-varnames)
(provide get-consts)
(provide get-names)
(provide get-code)
(provide get-IC)
(provide empty-stack)
(provide make-stack)
(provide push)
(provide pop)
(provide top)


;; TODO 1:
;; Alegeți metoda de reprezentarea a unei stive.
;; Implementați:
(define empty-stack '())
(define (make-stack) '())

(define (push element stack) (cons element stack))
(define (top stack) (car stack))
(define (pop stack) (cdr stack))

;; TODO 2:
;; Alegeți metoda de reprezentare a unei mașini stivă.
;; Definiți make-stack-machine, acesta trebuie sa primeasca cele 4 segmente de date
;; Veți avea nevoie de o stivă pentru execuție și un counter ca să stiți
;; la ce instrucțiune sunteți.
(define (make-stack-machine stack co-varnames co-consts co-names co-code IC)
  (list stack co-varnames co-consts co-names co-code IC))

;; Definiți funcțiile `get-varnames`, `get-consts`, `get-names`,
;; `get-code`, `get-stack`, `get-IC` care primesc o mașina stivă și întorc
;; componenta respectivă

;; ex:
;; > (get-varnames (make-stack-machine empty-stack 'dummy-co-varnames (hash) (hash) (list) 0))
;; 'dummy-co-varnames
(define (get-varnames stack-machine) (second stack-machine))

;; ex:
;; > (get-consts (make-stack-machine empty-stack (hash) 'dummy-co-consts (hash) (list) 0))
;; 'dummy-co-consts
(define (get-consts stack-machine) (third stack-machine))

;; ex:
;; > (get-names (make-stack-machine empty-stack (hash) (hash) 'dummy-co-names (list) 0))
;; 'dummy-co-names
(define (get-names stack-machine) (fourth stack-machine))

;; ex:
;; > (get-code (make-stack-machine empty-stack (hash) (hash) (hash) 'dummy-co-code 0))
;; dummy-co-code
(define (get-code stack-machine) (fifth stack-machine))

;; Întoarce stiva de execuție.
;; ex:
;; > (get-code (make-stack-machine 'dummy-exec-stack (hash) (hash) (hash) (list) 0))
;; dummy-exec-stack
(define (get-stack stack-machine) (car stack-machine))

;; Întoarce instruction counterul.
;; ex:
;; > (get-code (make-stack-machine empty-stack (hash) (hash) (hash) (list) 0))
;; 0
(define (get-IC stack-machine) (last stack-machine))



(define symbols (list 'STACK 'CO-VARNAMES 'CO-CONSTS 'CO-NAMES 'CO-CODE 'INSTRUCTION-COUNTER))

;; TODO 3:
;; Definiți funcția get-symbol-index care gasește index-ul simbolului in listă.
(define (get-symbol-index symbol)
  (let iter((lst symbols) (index 0))
    (cond ((empty? lst) #f)
          ((equal? (first lst) symbol) index)
          (else (iter (rest lst) (add1 index))))))

;; Definiți funcția update-stack-machine care intoarce o noua mașina stivă
;; înlocuind componenta corespondentă simbolului cu item-ul dat în paremetri.
;; > (get-varnames (update-stack-machine "new-varnames" 'CO-VARNAMES stack-machine))
;; "new-varnames"
;; > (get-varnames (update-stack-machine "new-names" 'CO-NAMES stack-machine))
;; "new-names"
;;(define (make-stack-machine stack co-varnames co-consts co-names co-code IC)
(define (update-stack-machine item symbol stack-machine)
  (let ((index (get-symbol-index symbol)))
    (cond ((= index 0) (make-stack-machine item (get-varnames stack-machine) (get-consts stack-machine) (get-names stack-machine) (get-code stack-machine) (get-IC stack-machine)))
          ((= index 1) (make-stack-machine (get-stack stack-machine) item  (get-consts stack-machine) (get-names stack-machine) (get-code stack-machine) (get-IC stack-machine)))
          ((= index 2) (make-stack-machine (get-stack stack-machine) (get-varnames stack-machine) item (get-names stack-machine) (get-code stack-machine) (get-IC stack-machine)))
          ((= index 3) (make-stack-machine (get-stack stack-machine) (get-varnames stack-machine) (get-consts stack-machine) item (get-code stack-machine) (get-IC stack-machine)))
          ((= index 4) (make-stack-machine (get-stack stack-machine) (get-varnames stack-machine) (get-consts stack-machine) (get-names stack-machine) item (get-IC stack-machine)))
          ((= index 5) (make-stack-machine (get-stack stack-machine) (get-varnames stack-machine) (get-consts stack-machine) (get-names stack-machine) (get-code stack-machine) item))
          ((= index #f) stack-machine))))

;; Definiți funcția push-exec-stack care primește o masină stivă și o valoare
;; și intoarce o noua mașina unde valoarea este pusă pe stiva de execuție
(define (push-exec-stack value stack-machine)
  (update-stack-machine (cons value (get-stack stack-machine)) 'STACK stack-machine))

;;  Definiți funcția pop-exec-stack care primește o masină stivă
;;  și intoarce o noua mașina aplicând pop pe stiva de execuție.
(define (pop-exec-stack stack-machine)
  (if (empty? (get-stack stack-machine))
      stack-machine
      (update-stack-machine (cdr (get-stack stack-machine)) 'STACK stack-machine)))

;; TODO 4:
;; Definiți funcția run-stack-machine care execută operații pană epuizează co-code.
(define (TOP stack-machine)
  (if (empty? (get-stack stack-machine))
      '()
      (car (get-stack stack-machine))))
(define (TOP1 stack-machine)
  (if (>= (length (get-stack stack-machine)) 2)
      (car (cdr (get-stack stack-machine)))
      '()))
(define (increase-IC stack-machine)
  (update-stack-machine (add1 (get-IC stack-machine)) 'INSTRUCTION-COUNTER stack-machine))

(define (execute-instruction stack-machine)
  (let ((instruction (car (list-ref (get-code stack-machine) (get-IC stack-machine)))))
    (cond
      ((equal? instruction 'LOAD_CONST) (increase-IC (load_const stack-machine)))
      ((equal? instruction 'STORE_FAST) (increase-IC (store_fast stack-machine)))
      ((equal? instruction 'RETURN_VALUE) (increase-IC stack-machine))
      ((equal? instruction 'LOAD_FAST) (increase-IC (load_fast stack-machine)))
      ((equal? instruction 'BINARY_ADD) (increase-IC (binary-add stack-machine)))
      ((equal? instruction 'BINARY_SUBTRACT) (increase-IC (binary-subtract stack-machine)))
      ((equal? instruction 'BINARY_MODULO) (increase-IC (binary-modulo stack-machine)))
      ((equal? instruction 'INPLACE_ADD) (increase-IC (binary-add stack-machine)))
      ((equal? instruction 'INPLACE_SUBTRACT) (increase-IC (binary-subtract stack-machine)))
      ((equal? instruction 'INPLACE_MODULO) (increase-IC (binary-modulo stack-machine)))
      ((equal? instruction 'COMPARE_OP) (increase-IC (compare-op stack-machine)))
      ((equal? instruction 'POP_JUMP_IF_FALSE) (increase-IC (jump-false stack-machine)))
      ((equal? instruction 'POP_JUMP_IF_TRUE) (increase-IC (jump-true stack-machine)))
      ((equal? instruction 'JUMP_ABSOLUTE) (increase-IC (jump-absolute stack-machine)))
      ((equal? instruction 'FOR_ITER) (increase-IC (for-iter stack-machine)))
      ((equal? instruction 'POP_TOP) (increase-IC (pop-exec-stack stack-machine)))
      ((equal? instruction 'LOAD_GLOBAL) (increase-IC (load-global stack-machine)))
      ((equal? instruction 'CALL_FUNCTION) (increase-IC (call-function stack-machine)))
      [else (increase-IC stack-machine)])))

(define (run-stack-machine stack-machine)
  (if (>= (get-IC stack-machine) (length (get-code stack-machine)))
      stack-machine
      (run-stack-machine (execute-instruction stack-machine))))

(define (load_const stack-machine)
  (let ((instruction-pair (list-ref (get-code stack-machine) (get-IC stack-machine)))) 
    (push-exec-stack (hash-ref (get-consts stack-machine) (cdr instruction-pair)) stack-machine)))

(define (store_fast stack-machine)
  (let ((instruction-pair (list-ref (get-code stack-machine) (get-IC stack-machine)))
        (element (TOP stack-machine)))
    (pop-exec-stack (update-stack-machine (hash-set (get-varnames stack-machine) (cdr instruction-pair) element) 'CO-VARNAMES stack-machine))))

(define (load_fast stack-machine)
  (let ((instruction-pair (list-ref (get-code stack-machine) (get-IC stack-machine))))
    (push-exec-stack (hash-ref (get-varnames stack-machine) (cdr instruction-pair)) stack-machine)))

(define (binary-add stack-machine)
  (let ((t1 (TOP1 stack-machine)) (t (TOP stack-machine)))
    (push-exec-stack (+ t1 t) (pop-exec-stack (pop-exec-stack stack-machine)))))

(define (binary-subtract stack-machine)
  (let ((t1 (TOP1 stack-machine)) (t (TOP stack-machine)))
    (push-exec-stack (- t1 t) (pop-exec-stack (pop-exec-stack stack-machine)))))

(define (binary-modulo stack-machine)
  (let ((t1 (TOP1 stack-machine)) (t (TOP stack-machine)))
    (push-exec-stack (modulo t1 t) (pop-exec-stack (pop-exec-stack stack-machine)))))

(define ops (list < <= equal? '!= > >=))

(define (compare-op stack-machine)
  (let ((t1 (TOP1 stack-machine)) (t (TOP stack-machine)) (index (cdr (list-ref (get-code stack-machine) (get-IC stack-machine)))))
    (if (equal? index 3)
        (push-exec-stack (not (equal? t t1)) (pop-exec-stack (pop-exec-stack stack-machine)))
        (push-exec-stack ((list-ref ops index) t1 t) (pop-exec-stack (pop-exec-stack stack-machine))))))

(define (jump-false stack-machine)
  (if (equal? (TOP stack-machine) #f)
      (let ((instruction-pair (list-ref (get-code stack-machine) (get-IC stack-machine))))
        (update-stack-machine (- (/ (cdr instruction-pair) 2) 1) 'INSTRUCTION-COUNTER (pop-exec-stack stack-machine)))
      (pop-exec-stack stack-machine)
      ))
(define (jump-true stack-machine)
  (if (equal? (TOP stack-machine) #t)
      (let ((instruction-pair (list-ref (get-code stack-machine) (get-IC stack-machine))))
        (update-stack-machine (- (/ (cdr instruction-pair) 2) 1) 'INSTRUCTION-COUNTER (pop-exec-stack stack-machine)))
      (pop-exec-stack stack-machine)
      ))

(define (jump-absolute stack-machine)
  (let ((instruction-pair (list-ref (get-code stack-machine) (get-IC stack-machine))))
    (update-stack-machine (- (/ (cdr instruction-pair) 2) 1) 'INSTRUCTION-COUNTER stack-machine)))

(define (for-iter stack-machine)
  (let ((instruction-pair (list-ref (get-code stack-machine) (get-IC stack-machine))) (t (TOP stack-machine)))
    (cond ((empty? t) (update-stack-machine (+ (get-IC stack-machine) (/ (cdr instruction-pair) 2)) 'INSTRUCTION-COUNTER (pop-exec-stack stack-machine)))
          ((equal? (length t) 1) (push-exec-stack (car t) (push-exec-stack '() (pop-exec-stack stack-machine))))
          [else (push-exec-stack (car t) (push-exec-stack (cdr t) (pop-exec-stack stack-machine)))])))

(define (load-global stack-machine)
  (let ((instruction-pair (list-ref (get-code stack-machine) (get-IC stack-machine)))) 
    (push-exec-stack (hash-ref (get-names stack-machine) (cdr instruction-pair)) stack-machine)))

(define (call-function  stack-machine)
  (define (call-helper stack nr-of-pop args)
    (if (zero? nr-of-pop)
        (cons (car stack) args)
        (call-helper (cdr stack) (- nr-of-pop 1) (cons (car stack) args))))
  (define (pop-first-n stack-machine n)
    (if (zero? n)
        stack-machine
        (pop-first-n (pop-exec-stack stack-machine) (- n 1))))
  (let ((n (cdr (list-ref (get-code stack-machine) (get-IC stack-machine)))))
    (let ((args (call-helper (get-stack stack-machine) n '())))
      (cond ((equal? (car args) "print") (push-exec-stack (println (car(cdr args))) (pop-first-n stack-machine (+ 1 n))))
            ((equal? (car args) "range") (push-exec-stack (range (car(cdr args))) (pop-first-n stack-machine (+ 1 n))))
            ((equal? (car args) "sqrt") (push-exec-stack (sqrt (car(cdr args))) (pop-first-n stack-machine (+ 1 n))))
            ((equal? (car args) "prod") (push-exec-stack (mymultiplication (cdr args) 1) (pop-first-n stack-machine (+ 1 n))))
                                        
            [else stack-machine]))))

(define (mymultiplication args acc)
  (cond ((empty? args) acc)
        ((equal? (length args) 1)(mymultiplication '() (* acc (car args))))
        [else (mymultiplication (cdr args) (* acc (car args)))]))
