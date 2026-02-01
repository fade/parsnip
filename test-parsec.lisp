;;; test-parsec.lisp - Tests for Parsec-compatible combinators

;;; Copyright 2021 Samuel Hunter <samuel (at) shunter (dot) xyz>
;;; Copyright 2026 Brian O'Reilly <fade@deepsky.com>
;;; BSD-3-Clause

(in-package #:cl-user)
(defpackage #:xyz.shunter.parsnip.test-parsec
  (:use #:cl
        #:xyz.shunter.parsnip)
  (:local-nicknames (#:tt #:parachute))
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

(tt:define-test "parsec.label"
  (tt:is equal "digit"
         (handler-case
             (parse-string (label (char-of #\a) "digit") "z")
           (parser-error (e) (parser-error-expected e))))
  ;; Label doesn't change successful parse
  (tt:is equal #\a (parse-string (label (char-of #\a) "letter-a") "abc")))

(tt:define-test "parsec.unexpected"
  (tt:fail (parse-string (unexpected "end of file") "abc") error)
  ;; Check the error message contains "unexpected"
  (let ((expected (handler-case
                      (progn (parse-string (unexpected "EOF") "abc") nil)
                    (parser-error (e) (parser-error-expected e)))))
    (tt:true (and expected (search "unexpected" expected)))))

;;; ============================================================
;;; Option Combinator Tests
;;; ============================================================

(tt:define-test "parsec.option"
  ;; Returns parsed value on success
  (tt:is equal #\a (parse-string (option #\z (char-of #\a)) "abc"))
  ;; Returns default on failure
  (tt:is equal #\z (parse-string (option #\z (char-of #\a)) "xyz"))
  ;; Works with complex parsers
  (tt:is equal 0 (parse-string (option 0 (natural)) "abc"))
  (tt:is equal 42 (parse-string (option 0 (natural)) "42")))

;;; ============================================================
;;; Separator Combinator Tests
;;; ============================================================

(tt:define-test "parsec.sep-end-by"
  ;; Zero items
  (tt:is equal '() (parse-string (sep-end-by (char-of #\a) (char-of #\,)) ""))
  ;; One item, no trailing sep
  (tt:is equal '(#\a) (parse-string (sep-end-by (char-of #\a) (char-of #\,)) "a"))
  ;; One item, trailing sep
  (tt:is equal '(#\a) (parse-string (sep-end-by (char-of #\a) (char-of #\,)) "a,"))
  ;; Multiple items, no trailing sep
  (tt:is equal '(#\a #\a #\a) (parse-string (sep-end-by (char-of #\a) (char-of #\,)) "a,a,a"))
  ;; Multiple items, trailing sep
  (tt:is equal '(#\a #\a #\a) (parse-string (sep-end-by (char-of #\a) (char-of #\,)) "a,a,a,")))

(tt:define-test "parsec.sep-end-by1"
  ;; Must have at least one item
  (tt:fail (parse-string (sep-end-by1 (char-of #\a) (char-of #\,)) "") error)
  ;; One item works
  (tt:is equal '(#\a) (parse-string (sep-end-by1 (char-of #\a) (char-of #\,)) "a"))
  ;; Trailing sep works
  (tt:is equal '(#\a #\a) (parse-string (sep-end-by1 (char-of #\a) (char-of #\,)) "a,a,"))
  (tt:is equal '(#\a #\a) (parse-string (sep-end-by1 (char-of #\a) (char-of #\,)) "a,a")))

;;; ============================================================
;;; Skip Combinator Tests
;;; ============================================================

(tt:define-test "parsec.skip-many1"
  ;; Fails if no match
  (tt:fail (parse-string (skip-many1 (char-of #\a)) "bbb") error)
  ;; Succeeds with one or more, returns nil
  (tt:is equal nil (parse-string (skip-many1 (char-of #\a)) "aaa"))
  (tt:is equal nil (parse-string (skip-many1 (char-of #\a)) "a")))

;;; ============================================================
;;; Position Access Tests
;;; ============================================================

(tt:define-test "parsec.get-line"
  (tt:is equal 1 (parse-string (get-line) "abc"))
  ;; After newline, line increases
  (tt:is equal 2 (parse-string (progn! (char-of #\Newline) (get-line))
                               (format nil "~%abc"))))

(tt:define-test "parsec.get-column"
  (tt:is equal 0 (parse-string (get-column) "abc"))
  ;; After consuming chars via char-of, column increases
  ;; Note: string-of uses read-sequence and doesn't track position per-char
  (tt:is equal 3 (parse-string (progn! (char-of #\a) (char-of #\b) (char-of #\c) (get-column)) "abcdef")))

(tt:define-test "parsec.get-position"
  (let ((pos (parse-string (get-position) "abc")))
    (tt:is equal 3 (length pos))  ; (position line column)
    (tt:is equal 1 (second pos))  ; line
    (tt:is equal 0 (third pos)))) ; column

;;; ============================================================
;;; Expression Parser Tests
;;; ============================================================

(tt:define-test "parsec.expression-parser-basic"
  ;; Simple addition
  (let ((expr (build-expression-parser
               (list (list (infix (progn! (char-of #\+) (ok #'+)) (assoc-left))))
               (natural))))
    (tt:is equal 3 (parse-string expr "1+2"))
    (tt:is equal 6 (parse-string expr "1+2+3"))
    (tt:is equal 10 (parse-string expr "10"))))

(tt:define-test "parsec.expression-parser-precedence"
  ;; Multiplication has higher precedence than addition
  (let ((expr (build-expression-parser
               (list
                ;; Higher precedence first
                (list (infix (progn! (char-of #\*) (ok #'*)) (assoc-left)))
                ;; Lower precedence
                (list (infix (progn! (char-of #\+) (ok #'+)) (assoc-left))))
               (natural))))
    (tt:is equal 7 (parse-string expr "1+2*3"))   ; 1 + (2*3) = 7
    (tt:is equal 14 (parse-string expr "2*3+2*4")))) ; (2*3) + (2*4) = 14

(tt:define-test "parsec.expression-parser-prefix"
  ;; Negation prefix operator
  (let ((expr (build-expression-parser
               (list (list (prefix (progn! (char-of #\-) (ok #'-)))))
               (natural))))
    (tt:is equal -5 (parse-string expr "-5"))
    (tt:is equal 5 (parse-string expr "5"))))

(tt:define-test "parsec.expression-parser-right-assoc"
  ;; Exponentiation is right-associative
  (let ((expr (build-expression-parser
               (list (list (infix (progn! (char-of #\^) (ok #'expt)) (assoc-right))))
               (natural))))
    (tt:is equal 8 (parse-string expr "2^3"))
    ;; 2^3^2 = 2^(3^2) = 2^9 = 512 (right assoc)
    (tt:is equal 512 (parse-string expr "2^3^2"))))

;;; ============================================================
;;; Token Parser Tests
;;; ============================================================

(tt:define-test "parsec.token-parser-basic"
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
    (tt:is equal 42 (parse-string (token-parser-natural tp) "42"))
    ;; Integer
    (tt:is equal -5 (parse-string (token-parser-integer tp) "-5"))
    ;; Symbol
    (tt:is equal "+" (parse-string (funcall (token-parser-symbol tp) "+") "+"))))

;;; ============================================================
;;; Permutation Parser Tests
;;; ============================================================

(tt:define-test "parsec.permute-basic"
  ;; Parse a and b in any order
  (let ((parser (permute (perm-req (char-of #\a))
                         (perm-req (char-of #\b)))))
    (tt:is equal '(#\a #\b) (parse-string parser "ab"))
    (tt:is equal '(#\a #\b) (parse-string parser "ba"))))

(tt:define-test "parsec.permute-optional"
  ;; Parse a (required) and b (optional, default #\z)
  (let ((parser (permute (perm-req (char-of #\a))
                         (perm-opt #\z (char-of #\b)))))
    (tt:is equal '(#\a #\b) (parse-string parser "ab"))
    (tt:is equal '(#\a #\b) (parse-string parser "ba"))
    (tt:is equal '(#\a #\z) (parse-string parser "a"))))

;;; ============================================================
;;; Debugging Combinator Tests
;;; ============================================================

(tt:define-test "parsec.parser-trace"
  ;; parser-trace should succeed and return nil
  (tt:is equal nil (parse-string (parser-trace "test") "abc"))
  ;; It shouldn't consume input
  (tt:is equal #\a (parse-string (progn! (parser-trace "test") (any-char)) "abc")))

(tt:define-test "parsec.parser-traced"
  ;; parser-traced wraps a parser and traces it
  (tt:is equal #\a (parse-string (parser-traced "any" (any-char)) "abc"))
  ;; Failure case
  (tt:fail (parse-string (parser-traced "digit" (digit)) "abc") error))
