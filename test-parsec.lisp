;;; test-parsec.lisp - Tests for Parsec-compatible combinators

;;; Copyright 2021 Samuel Hunter <samuel (at) shunter (dot) xyz>
;;; Copyright 2026 Brian O'Reilly <fade@deepsky.com>
;;; BSD-3-Clause

(in-package #:cl-user)
(defpackage #:xyz.shunter.parsnip.test-parsec
  (:use #:cl
        #:xyz.shunter.parsnip
        #:parachute)
  (:shadowing-import-from #:parachute #:fail #:skip)
  (:nicknames #:parsnip/test-parsec))
(in-package #:xyz.shunter.parsnip.test-parsec)

(defun parse-string (parser string)
  (with-input-from-string (stream string)
    (parse parser stream)))

(defun capture-error (form)
  "Helper to capture parser errors for testing."
  (handler-case
      (progn form nil)
    (parser-error (e) e)))

;;; ============================================================
;;; Error Labeling Tests
;;; ============================================================

(define-test "parsec.label"
  (is equal "digit"
         (handler-case
             (parse-string (label (char-of #\a) "digit") "z")
           (parser-error (e) (parser-error-expected e))))
  ;; Label doesn't change successful parse
  (is equal #\a (parse-string (label (char-of #\a) "letter-a") "abc")))

(define-test "parsec.unexpected"
  (fail (parse-string (unexpected "end of file") "abc") error)
  ;; Check the error message contains "unexpected"
  (let ((expected (handler-case
                      (progn (parse-string (unexpected "EOF") "abc") nil)
                    (parser-error (e) (parser-error-expected e)))))
    (true (and expected (search "unexpected" expected)))))

;;; ============================================================
;;; Option Combinator Tests
;;; ============================================================

(define-test "parsec.option"
  ;; Returns parsed value on success
  (is equal #\a (parse-string (option #\z (char-of #\a)) "abc"))
  ;; Returns default on failure
  (is equal #\z (parse-string (option #\z (char-of #\a)) "xyz"))
  ;; Works with complex parsers
  (is equal 0 (parse-string (option 0 (natural)) "abc"))
  (is equal 42 (parse-string (option 0 (natural)) "42")))

;;; ============================================================
;;; Separator Combinator Tests
;;; ============================================================

(define-test "parsec.sep-end-by"
  ;; Zero items
  (is equal '() (parse-string (sep-end-by (char-of #\a) (char-of #\,)) ""))
  ;; One item, no trailing sep
  (is equal '(#\a) (parse-string (sep-end-by (char-of #\a) (char-of #\,)) "a"))
  ;; One item, trailing sep
  (is equal '(#\a) (parse-string (sep-end-by (char-of #\a) (char-of #\,)) "a,"))
  ;; Multiple items, no trailing sep
  (is equal '(#\a #\a #\a) (parse-string (sep-end-by (char-of #\a) (char-of #\,)) "a,a,a"))
  ;; Multiple items, trailing sep
  (is equal '(#\a #\a #\a) (parse-string (sep-end-by (char-of #\a) (char-of #\,)) "a,a,a,")))

(define-test "parsec.sep-end-by1"
  ;; Must have at least one item
  (fail (parse-string (sep-end-by1 (char-of #\a) (char-of #\,)) "") error)
  ;; One item works
  (is equal '(#\a) (parse-string (sep-end-by1 (char-of #\a) (char-of #\,)) "a"))
  ;; Trailing sep works
  (is equal '(#\a #\a) (parse-string (sep-end-by1 (char-of #\a) (char-of #\,)) "a,a,"))
  (is equal '(#\a #\a) (parse-string (sep-end-by1 (char-of #\a) (char-of #\,)) "a,a")))

;;; ============================================================
;;; Skip Combinator Tests
;;; ============================================================

(define-test "parsec.skip-many1"
  ;; Fails if no match
  (fail (parse-string (skip-many1 (char-of #\a)) "bbb") error)
  ;; Succeeds with one or more, returns nil
  (is equal nil (parse-string (skip-many1 (char-of #\a)) "aaa"))
  (is equal nil (parse-string (skip-many1 (char-of #\a)) "a")))

;;; ============================================================
;;; Position Access Tests
;;; ============================================================

(define-test "parsec.get-line"
  (is equal 1 (parse-string (get-line) "abc"))
  ;; After newline, line increases
  (is equal 2 (parse-string (progn! (char-of #\Newline) (get-line))
                               (format nil "~%abc"))))

(define-test "parsec.get-column"
  (is equal 0 (parse-string (get-column) "abc"))
  ;; After consuming chars via char-of, column increases
  ;; Note: string-of uses read-sequence and doesn't track position per-char
  (is equal 3 (parse-string (progn! (char-of #\a) (char-of #\b) (char-of #\c) (get-column)) "abcdef")))

(define-test "parsec.get-position"
  (let ((pos (parse-string (get-position) "abc")))
    (is equal 3 (length pos))  ; (position line column)
    (is equal 1 (second pos))  ; line
    (is equal 0 (third pos)))) ; column

;;; ============================================================
;;; Expression Parser Tests
;;; ============================================================

(define-test "parsec.expression-parser-basic"
  ;; Simple addition
  (let ((expr (build-expression-parser
               (list (list (infix (progn! (char-of #\+) (ok #'+)) (assoc-left))))
               (natural))))
    (is equal 3 (parse-string expr "1+2"))
    (is equal 6 (parse-string expr "1+2+3"))
    (is equal 10 (parse-string expr "10"))))

(define-test "parsec.expression-parser-precedence"
  ;; Multiplication has higher precedence than addition
  (let ((expr (build-expression-parser
               (list
                ;; Higher precedence first
                (list (infix (progn! (char-of #\*) (ok #'*)) (assoc-left)))
                ;; Lower precedence
                (list (infix (progn! (char-of #\+) (ok #'+)) (assoc-left))))
               (natural))))
    (is equal 7 (parse-string expr "1+2*3"))   ; 1 + (2*3) = 7
    (is equal 14 (parse-string expr "2*3+2*4")))) ; (2*3) + (2*4) = 14

(define-test "parsec.expression-parser-prefix"
  ;; Negation prefix operator
  (let ((expr (build-expression-parser
               (list (list (prefix (progn! (char-of #\-) (ok #'-)))))
               (natural))))
    (is equal -5 (parse-string expr "-5"))
    (is equal 5 (parse-string expr "5"))))

(define-test "parsec.expression-parser-right-assoc"
  ;; Exponentiation is right-associative
  (let ((expr (build-expression-parser
               (list (list (infix (progn! (char-of #\^) (ok #'expt)) (assoc-right))))
               (natural))))
    (is equal 8 (parse-string expr "2^3"))
    ;; 2^3^2 = 2^(3^2) = 2^9 = 512 (right assoc)
    (is equal 512 (parse-string expr "2^3^2"))))

(define-test "parsec.expression-parser-postfix"
  ;; Factorial-like postfix operator
  (let ((expr (build-expression-parser
               (list (list (postfix (progn! (char-of #\!) (ok (lambda (n)
                                                                (loop for i from 1 to n
                                                                      for r = 1 then (* r i)
                                                                      finally (return r))))))))
               (natural))))
    (is equal 120 (parse-string expr "5!"))
    (is equal 5 (parse-string expr "5"))))

(define-test "parsec.expression-parser-assoc-none"
  ;; Non-associative comparison operator - should parse a single use but not chain
  (let ((expr (build-expression-parser
               (list (list (infix (progn! (char-of #\=) (ok (lambda (a b) (list := a b))))
                                  (assoc-none))))
               (natural))))
    (is equal '(:= 1 2) (parse-string expr "1=2"))
    (is equal 5 (parse-string expr "5"))))

(define-test "parsec.get-input"
  ;; get-input returns the underlying stream without consuming
  (let ((result (parse-string (let! ((s (get-input))
                                     (c (any-char)))
                                (ok (list (streamp s) c)))
                              "abc")))
    (is equal t (first result))
    (is equal #\a (second result))))

;;; ============================================================
;;; Token Parser Tests
;;; ============================================================

(define-test "parsec.token-parser-basic"
  (let* ((lang (make-language-def
                :comment-line "//"
                :comment-start "/*"
                :comment-end "*/"
                :ident-start (letter)
                :ident-letter (or! (alpha-num) (char-of #\_))
                :reserved-names '("if" "then" "else")
                :case-sensitive t))
         (tp (make-token-parser lang)))
    ;; Natural number
    (is equal 42 (parse-string (token-parser-natural tp) "42"))
    ;; Integer
    (is equal -5 (parse-string (token-parser-integer tp) "-5"))
    ;; Symbol
    (is equal "+" (parse-string (funcall (token-parser-symbol tp) "+") "+"))))

(define-test "parsec.deftoken"
  (let* ((lang (make-language-def
                :comment-line "//"
                :ident-start (letter)
                :ident-letter (or! (alpha-num) (char-of #\_))
                :reserved-names '("let" "in")
                :case-sensitive t))
         (tp (make-token-parser lang)))
    ;; deftoken creates a function that returns the accessor's parser
    (deftoken test-natural tp token-parser-natural)
    (deftoken test-integer tp token-parser-integer)
    (is equal 42 (parse-string (test-natural) "42"))
    (is equal -7 (parse-string (test-integer) "-7"))))

;;; ============================================================
;;; Permutation Parser Tests
;;; ============================================================

(define-test "parsec.permute-basic"
  ;; Parse a and b in any order
  (let ((parser (permute (perm-req (char-of #\a))
                         (perm-req (char-of #\b)))))
    (is equal '(#\a #\b) (parse-string parser "ab"))
    (is equal '(#\a #\b) (parse-string parser "ba"))))

(define-test "parsec.permute-optional"
  ;; Parse a (required) and b (optional, default #\z)
  (let ((parser (permute (perm-req (char-of #\a))
                         (perm-opt #\z (char-of #\b)))))
    (is equal '(#\a #\b) (parse-string parser "ab"))
    (is equal '(#\a #\b) (parse-string parser "ba"))
    (is equal '(#\a #\z) (parse-string parser "a"))))

;;; ============================================================
;;; Debugging Combinator Tests
;;; ============================================================

(define-test "parsec.parser-trace"
  ;; parser-trace should succeed and return nil
  (is equal nil (parse-string (parser-trace "test") "abc"))
  ;; It shouldn't consume input
  (is equal #\a (parse-string (progn! (parser-trace "test") (any-char)) "abc")))

(define-test "parsec.parser-traced"
  ;; parser-traced wraps a parser and traces it
  (is equal #\a (parse-string (parser-traced "any" (any-char)) "abc"))
  ;; Failure case
  (fail (parse-string (parser-traced "digit" (digit)) "abc") error))
