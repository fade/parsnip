;;; test-literals.lisp - Tests for literal parsers
;;;
;;; Additions by Brian O'Reilly <fade@deepsky.com>
;;; BSD-3-Clause

(in-package #:cl-user)
(defpackage #:xyz.shunter.parsnip.test-literals
  (:use #:cl
        #:xyz.shunter.parsnip)
  (:local-nicknames (#:tt #:parachute))
  (:nicknames #:parsnip/test-literals))
(in-package #:xyz.shunter.parsnip.test-literals)

(defun parse-string (parser string)
  (with-input-from-string (stream string)
    (parse parser stream)))

(tt:define-test "parsnip.literals")

(tt:define-test "any-char"
  :parent "parsnip.literals"
  (tt:is equal #\a (parse-string (any-char) "abc"))
  (tt:is equal #\z (parse-string (any-char) "z"))
  (tt:fail (parse-string (any-char) ""))
  ;; Unicode characters work in SBCL
  (tt:is equal #\日 (parse-string (any-char) "日本語")))

(tt:define-test "a-space"
  :parent "parsnip.literals"
  (tt:is equal #\Space (parse-string (a-space) " ")))

(tt:define-test "spaces"
  :parent "parsnip.literals"
  (tt:is equal nil (parse-string (spaces) "   abc"))
  (tt:is equal nil (parse-string (spaces) "abc")))

(tt:define-test "newline"
  :parent "parsnip.literals"
  (tt:is equal #\Newline (parse-string (newline) (format nil "~%")))
  (tt:fail (parse-string (newline) "a")))

(tt:define-test "tab"
  :parent "parsnip.literals"
  (tt:is equal #\Tab (parse-string (tab) (format nil "~C" #\Tab)))
  (tt:fail (parse-string (tab) "a")))

(tt:define-test "upper"
  :parent "parsnip.literals"
  (tt:is equal #\A (parse-string (upper) "Abc"))
  (tt:fail (parse-string (upper) "abc")))

(tt:define-test "lower"
  :parent "parsnip.literals"
  (tt:is equal #\a (parse-string (lower) "aBc"))
  (tt:fail (parse-string (lower) "ABC")))

(tt:define-test "alpha-num"
  :parent "parsnip.literals"
  (tt:is equal #\a (parse-string (alpha-num) "abc"))
  (tt:is equal #\1 (parse-string (alpha-num) "123"))
  (tt:fail (parse-string (alpha-num) "-")))

(tt:define-test "letter"
  :parent "parsnip.literals"
  (tt:is equal #\a (parse-string (letter) "abc"))
  (tt:fail (parse-string (letter) "123")))

(tt:define-test "hex-digit"
  :parent "parsnip.literals"
  (tt:is equal 10 (parse-string (hex-digit) "A"))
  (tt:is equal 15 (parse-string (hex-digit) "f"))
  (tt:fail (parse-string (hex-digit) "G")))

(tt:define-test "oct-digit"
  :parent "parsnip.literals"
  (tt:is equal 7 (parse-string (oct-digit) "7"))
  (tt:fail (parse-string (oct-digit) "8")))

(tt:define-test "crlf"
  :parent "parsnip.literals"
  (tt:is equal (format nil "~C~C" #\Return #\Newline) (parse-string (crlf) (format nil "~C~C" #\Return #\Newline)))
  (tt:fail (parse-string (crlf) (format nil "~%"))))

(tt:define-test "end-of-line"
  :parent "parsnip.literals"
  (tt:is equal #\Newline (parse-string (end-of-line) (format nil "~%")))
  (tt:is equal (format nil "~C~C" #\Return #\Newline) (parse-string (end-of-line) (format nil "~C~C" #\Return #\Newline))))

(tt:define-test "sign"
  :parent "parsnip.literals"
  (tt:is equal #\+ (parse-string (sign) "+1"))
  (tt:is equal #\- (parse-string (sign) "-1"))
  (tt:fail (parse-string (sign) "1")))

(tt:define-test "an-integer"
  :parent "parsnip.literals"
  (tt:is equal 123 (parse-string (an-integer) "123"))
  (tt:is equal -456 (parse-string (an-integer) "-456"))
  (tt:is equal 0 (parse-string (an-integer) "0"))
  (tt:fail (parse-string (an-integer) "abc")))

(tt:define-test "a-float"
  :parent "parsnip.literals"
  (tt:is equal 123.0 (parse-string (a-float) "123.0"))
  (tt:is equal -4.56 (parse-string (a-float) "-4.56"))
  (tt:is equal 1.0e10 (parse-string (a-float) "1e10"))
  (tt:is equal 1.2e-5 (parse-string (a-float) "1.2e-5"))
  (tt:fail (parse-string (a-float) "abc"))
  (tt:fail (parse-string (a-float) "123"))) ; Requires fractional part or exponent

(tt:define-test "many"
  :parent "parsnip.literals"
  (tt:is equal '(#\a #\b #\c) (parse-string (many (any-char)) "abc"))
  (tt:is equal '() (parse-string (many (any-char)) ""))
  (tt:is equal '(1 2) (parse-string (many (digit)) "12abc")))

(tt:define-test "many1"
  :parent "parsnip.literals"
  (tt:is equal '(#\a #\b #\c) (parse-string (many1 (any-char)) "abc"))
  (tt:fail (parse-string (many1 (any-char)) ""))
  (tt:is equal '(1 2) (parse-string (many1 (digit)) "12abc")))
