;;; test-tiny-c.lisp - Tests for the tiny-c parser example

(defpackage #:xyz.shunter.parsnip.test-tiny-c
  (:use #:cl #:parachute)
  (:local-nicknames (#:tc #:xyz.shunter.parsnip.examples.tiny-c)))

(in-package #:xyz.shunter.parsnip.test-tiny-c)

(define-test tiny-c-parser
  :parent NIL)

;;; --- Simple expressions ---

(define-test simple-function
  :parent tiny-c-parser
  (is equal
      '((:FUNCTION "add" ("a" "b") (:RETURN (#\+ "a" "b"))))
      (tc:parse-tiny-c-from-string "add(a, b) { return a + b; }")))

(define-test empty-params
  :parent tiny-c-parser
  (is equal
      '((:FUNCTION "main" NIL (:RETURN 0)))
      (tc:parse-tiny-c-from-string "main() { return 0; }")))

;;; --- Expressions ---

(define-test arithmetic-precedence
  :parent tiny-c-parser
  (is equal
      '((:FUNCTION "f" NIL (:RETURN (#\+ 1 (#\* 2 3)))))
      (tc:parse-tiny-c-from-string "f() { return 1 + 2 * 3; }")))

(define-test assignment
  :parent tiny-c-parser
  (is equal
      '((:FUNCTION "f" NIL (:EXPR (:ASSIGN "x" 42)) (:RETURN "x")))
      (tc:parse-tiny-c-from-string "f() { x = 42; return x; }")))

(define-test comparison-operators
  :parent tiny-c-parser
  (is equal
      '((:FUNCTION "f" ("a" "b")
         (:IF (#\> "a" "b") (:RETURN "a"))
         (:RETURN "b")))
      (tc:parse-tiny-c-from-string "f(a, b) { if (a > b) return a; return b; }")))

(define-test equality-operators
  :parent tiny-c-parser
  (is equal
      '((:FUNCTION "f" ("n") (:IF ("==" "n" 0) (:RETURN 1)) (:RETURN 0)))
      (tc:parse-tiny-c-from-string "f(n) { if (n == 0) return 1; return 0; }")))

(define-test inequality-operator
  :parent tiny-c-parser
  (is equal
      '((:FUNCTION "f" ("n") (:IF ("!=" "n" 0) (:RETURN "n")) (:RETURN 0)))
      (tc:parse-tiny-c-from-string "f(n) { if (n != 0) return n; return 0; }")))

;;; --- Statements ---

(define-test while-loop
  :parent tiny-c-parser
  (is equal
      '((:FUNCTION "f" ("n")
         (:WHILE (#\> "n" 0)
           (:BLOCK (:EXPR (:ASSIGN "n" (#\- "n" 1)))))
         (:RETURN "n")))
      (tc:parse-tiny-c-from-string
       "f(n) { while (n > 0) { n = n - 1; } return n; }")))

(define-test function-call
  :parent tiny-c-parser
  (is equal
      '((:FUNCTION "f" NIL (:EXPR (:CALL "g" 1 2 3)) (:RETURN 0)))
      (tc:parse-tiny-c-from-string "f() { g(1, 2, 3); return 0; }")))

(define-test nested-calls
  :parent tiny-c-parser
  (is equal
      '((:FUNCTION "f" NIL (:RETURN (:CALL "g" (:CALL "h" 1)))))
      (tc:parse-tiny-c-from-string "f() { return g(h(1)); }")))

;;; --- Multiple functions ---

(define-test multiple-functions
  :parent tiny-c-parser
  (is equal
      '((:FUNCTION "inc" ("x") (:RETURN (#\+ "x" 1)))
        (:FUNCTION "main" NIL (:RETURN (:CALL "inc" 5))))
      (tc:parse-tiny-c-from-string
       "inc(x) { return x + 1; } main() { return inc(5); }")))

;;; --- Integration test: full tiny-c.c file ---

(define-test full-tiny-c-file
  :parent tiny-c-parser
  (let* ((file-path (asdf:system-relative-pathname :parsnip/examples
                                                    "examples/tiny-c.c"))
         (result (with-open-file (s file-path)
                   (tc:parse-tiny-c s))))
    ;; Should parse 4 functions
    (is = 4 (length result))
    ;; Check function names
    (is equal '("fact" "fib" "add" "main")
        (mapcar #'second result))
    ;; Check fact function structure
    (let ((fact (first result)))
      (is equal '("n") (third fact))
      (is eq :IF (first (fourth fact)))
      (is eq :RETURN (first (fifth fact))))
    ;; Check fib function has while loop
    (let ((fib (second result)))
      (is equal '("n") (third fib))
      (true (find :WHILE (cdddr fib) :key #'first)))
    ;; Check add function
    (let ((add (third result)))
      (is equal '("a" "b") (third add))
      (is equal '(:RETURN (#\+ "a" "b")) (fourth add)))
    ;; Check main function
    (let ((main (fourth result)))
      (is equal NIL (third main))
      ;; main calls sayn(fib(10)) and sayn(fact(10))
      (is equal '(:EXPR (:CALL "sayn" (:CALL "fib" 10))) (fourth main))
      (is equal '(:EXPR (:CALL "sayn" (:CALL "fact" 10))) (fifth main))
      (is equal '(:RETURN 0) (sixth main)))))
