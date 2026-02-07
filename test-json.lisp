;;; test-json.lisp - Test suite for the Parsnip JSON example

(defpackage #:xyz.shunter.parsnip.test-json
  (:use #:cl
        #:xyz.shunter.parsnip
        #:xyz.shunter.parsnip.examples.json)
  (:local-nicknames (#:tt #:parachute)))

(in-package #:xyz.shunter.parsnip.test-json)

(tt:define-test json-literals
  (tt:is eq :true (decode-json-from-string "true"))
  (tt:is eq :false (decode-json-from-string "false"))
  (tt:is eq :null (decode-json-from-string "null"))
  (tt:is eq :true (decode-json-from-string " true ")))

(tt:define-test json-numbers
  (tt:is = 42 (decode-json-from-string "42"))
  (tt:is = -123 (decode-json-from-string "-123"))
  (tt:is = 3.14 (decode-json-from-string "3.14"))
  (tt:is = -0.5 (decode-json-from-string "-0.5"))
  (tt:is = 1.0e3 (decode-json-from-string "1.0e3"))
  (tt:is = 1.0e+3 (decode-json-from-string "1.0e+3")))

(tt:define-test json-strings
  (tt:is string= "hello" (decode-json-from-string "\"hello\""))
  (tt:is string= "hello world" (decode-json-from-string "\"hello world\""))
  (tt:is string= "with\"quotes" (decode-json-from-string "\"with\\\"quotes\""))
  (tt:is string= (string #\Newline) (decode-json-from-string "\"\\n\"")))

;; Note: json-array returns lists, not vectors
(tt:define-test json-arrays
  (tt:is equalp '() (decode-json-from-string "[]"))
  (tt:is equalp '(1 2 3) (decode-json-from-string "[1, 2, 3]"))
  (tt:is equalp '("a" :true 42) (decode-json-from-string "[\"a\", true, 42]")))

(tt:define-test json-objects
  (tt:is equalp '() (decode-json-from-string "{}"))
  (tt:is equalp '(("a" . 1)) (decode-json-from-string "{\"a\": 1}"))
  (tt:is equalp '(("a" . 1) ("b" . "c")) (decode-json-from-string "{\"a\": 1, \"b\": \"c\"}")))

(tt:define-test json-nested-structures
  (tt:is equalp '(("a" . (1 2)) ("b" . :true))
        (decode-json-from-string "{\"a\": [1, 2], \"b\": true}"))
  (tt:is equalp '((("a" . 1)))
        (decode-json-from-string "[{\"a\": 1}]")))
