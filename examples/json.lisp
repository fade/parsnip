;;; json.lisp - Parsnip example JSON decoder

;;; Copyright 2021 Samuel Hunter <samuel (at) shunter (dot) xyz>
;;; Additions by Brian O'Reilly <fade@deepsky.com>
;;; BSD-3-Clause

(defpackage #:xyz.shunter.parsnip.examples.json
  (:documentation "Parsnip example JSON decoder using a token parser.")
  (:use #:cl #:xyz.shunter.parsnip)
  (:export #:decode-json
           #:decode-json-from-string))

(in-package #:xyz.shunter.parsnip.examples.json)

(defparameter *json-language-def*
  (make-language-def
   :comment-line ""
   :comment-start ""
   :comment-end ""
   :reserved-names '("true" "false" "null")
   :case-sensitive t))

(defparameter *token-parser*
  (make-token-parser *json-language-def*))

(defparameter *json-white-space* (token-parser-white-space *token-parser*))
(defparameter *json-lexeme* (token-parser-lexeme *token-parser*)) ; The combinator to apply lexeme once

;; Raw (non-whitespace skipping) token parsers for delimiters
(defparameter *raw-l-brace* (char-of #\{))
(defparameter *raw-r-brace* (char-of #\}))
(defparameter *raw-l-bracket* (char-of #\[))
(defparameter *raw-r-bracket* (char-of #\]))
(defparameter *raw-colon* (char-of #\:))
(defparameter *raw-comma* (char-of #\,))

;; These are the lexemes that skip their own trailing whitespace
(defparameter *l-brace* (funcall *json-lexeme* *raw-l-brace*))
(defparameter *r-brace* (funcall *json-lexeme* *raw-r-brace*))
(defparameter *l-bracket* (funcall *json-lexeme* *raw-l-bracket*))
(defparameter *r-bracket* (funcall *json-lexeme* *raw-r-bracket*))
(defparameter *colon* (funcall *json-lexeme* *raw-colon*))
(defparameter *comma* (funcall *json-lexeme* *raw-comma*))

(defparameter *string-lit* (token-parser-string-literal *token-parser*)) ; Already a lexeme
(defparameter *number-parser* (token-parser-natural-or-float *token-parser*)) ; Already a lexeme

(defparameter *true-parser*
  (flatmap (lambda (s) (declare (ignore s)) (ok :true))
           (funcall (token-parser-reserved *token-parser*) "true")))
(defparameter *false-parser*
  (flatmap (lambda (s) (declare (ignore s)) (ok :false))
           (funcall (token-parser-reserved *token-parser*) "false")))
(defparameter *null-parser*
  (flatmap (lambda (s) (declare (ignore s)) (ok :null))
           (funcall (token-parser-reserved *token-parser*) "null")))

;; Forward declaration to satisfy compiler for mutually recursive functions.
(declaim (ftype (function () (values t &optional)) json-object json-array value))

(defparser json-object ()
  (let! ((_l-brace *l-brace*)
         (middle (sep-end-by (let! ((key *string-lit*)
                                    (_ *colon*)
                                    (val 'value)) ; Quoted symbol
                               (ok (cons key val)))
                             *comma*))
         (_r-brace *r-brace*))
    (ok middle)))

(defparser json-array ()
  (let! ((_l-bracket *l-bracket*)
         (middle (sep-end-by 'value *comma*))
         (_r-bracket *r-bracket*))
    (ok middle)))

(defparser value ()
  (label (choice (list *true-parser*
                       *false-parser*
                       *null-parser*
                       *string-lit*
                       *number-parser*
                       'json-object
                       'json-array))
         "JSON value"))

(defparameter *text*
  (prog2! *json-white-space* ; Consume leading whitespace
          (funcall *json-lexeme* 'value) ; Parse the value, and consume its trailing whitespace
          (eof)))

(defun decode-json (&optional (stream *standard-input*))
  (parse *text* stream))

(defun decode-json-from-string (string)
  (with-input-from-string (stream string)
    (decode-json stream)))