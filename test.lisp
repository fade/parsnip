;;; test.lisp - Parsnip library test suite

;;; Copyright 2021 Samuel Hunter <samuel (at) shunter (dot) xyz>
;;; BSD-3-Clause

(defpackage #:xyz.shunter.parsnip.test
  (:use #:cl
        #:xyz.shunter.parsnip
        #:xyz.shunter.parsnip.examples.json
        #:xyz.shunter.parsnip.examples.tiny-c)
  (:import-from #:alexandria
                #:curry)
  (:local-nicknames (#:t #:parachute)))

(in-package #:xyz.shunter.parsnip.test)



(defun parse-string (parser string)
  (with-input-from-string (stream string)
    (parse parser stream)))

(defun capture-parse-error (parser string)
  (multiple-value-bind (always-nil err) (ignore-errors (parse-string parser string))
    (declare (ignore always-nil))
    err))

(defun close-enough (float-1 float-2)
  "Return whether two decimal numbers are within tolerance (0.05%)"
  (when (zerop (+ float-1 float-2))
    (return-from close-enough nil))
  (let ((difference (abs (/ (- float-1 float-2) (abs (+ float-1 float-2)) 2)))
        (tolerance 0.0005))
    (< difference tolerance)))



;; Unit Tests

(t:define-test ok
  (t:is eq :hello
        (parse-string (ok :hello) "whatever"))

  (t:is eq :hello
        (parse-string (ok :hello) "")))

(t:define-test fail
  (t:fail (parse-string (fail "terrible!") "whatever")
          'parser-error)
  (t:fail (parse-string (fail "terrible!") "")
          'parser-error)

  (t:is string= "terrible!"
        (parser-error-expected
          (capture-parse-error (fail "terrible!") "whatever"))))

(t:define-test char-if
  (t:is char= #\a
      (parse-string (char-if #'alpha-char-p) "a"))

  (t:is char= #\z
      (parse-string (char-if #'alpha-char-p) "z"))

  (t:fail (parse-string (char-if #'alpha-char-p) "0")
          'parser-error)
  (t:fail (parse-string (char-if #'alpha-char-p) "")
          'parser-error)

  (t:is string= "letter"
        (parser-error-expected
          (capture-parse-error (char-if #'alpha-char-p "letter") "0"))))

(t:define-test char-of
  :depends-on (char-if)
  (t:is char= #\a
        (parse-string (char-of #\a) "a"))
  (t:fail (parse-string (char-of #\a) "z")
          'parser-error)
  (t:fail (parse-string (char-of #\a) "")
          'parser-error)

  (t:is char= #\z
        (parse-string (char-of #\z) "z"))
  (t:fail (parse-string (char-of #\z) "a")
          'parser-error)

  (t:is string= (format nil "~S" #\a)
        (parser-error-expected
          (capture-parse-error (char-of #\a) "z"))))

(t:define-test char-in
  :depends-on (char-if)
  (t:is char= #\a
        (parse-string (char-in "abc") "a"))
  (t:is char= #\b
        (parse-string (char-in "abc") "b"))
  (t:is char= #\c
        (parse-string (char-in "abc") "c"))

  (t:fail (parse-string (char-in "abc") "z")
          'parser-error)
  (t:fail (parse-string (char-in "abc") "")
          'parser-error))

(t:define-test string-of
  (t:is string= "hello"
        (parse-string (string-of "hello") "hello"))

  (t:is string= "hello"
        (parse-string (string-of "hello") "hello world"))

  (t:is string= ""
        (parse-string (string-of "") "something"))

  (t:is string= "a"
        (parse-string (string-of "a") "a"))

  (t:fail (parse-string (string-of "something") "other thing")
          'parser-error)
  (t:fail (parse-string (string-of "something") "")
          'parser-error))

(t:define-test eof
  (t:is eq nil
        (parse-string (eof) ""))

  (t:is eq :end
        (parse-string (eof :end) ""))

  (t:fail (parse-string (eof) "something")
          'parser-error))

(t:define-test flatmap
  :depends-on (char-if)
  (let ((parser (flatmap (lambda (c)
                           (if (char= c #\d)
                               (fail "not d")
                               (ok c)))
                         (char-if #'alpha-char-p))))
    (t:is char= #\a
          (parse-string parser "a"))

    (t:is char= #\z
          (parse-string parser "z"))

    (t:fail (parse-string parser "1")
            'parser-error)

    (t:fail (parse-string parser "d")
            'parser-error)

    (t:is string= "not d"
          (parser-error-expected
            (capture-parse-error parser "d"))))

  (let ((parser (flatmap (lambda (c)
                           (if (char= c #\Z)
                               (char-if #'digit-char-p)
                               (ok c)))
                         (char-if #'alpha-char-p))))
    (t:is char= #\a
          (parse-string parser "a"))

    (t:is char= #\1
          (parse-string parser "Z1"))

    (t:fail (parse-string parser "Z")
            'parser-error)))

(t:define-test let!
  :depends-on (char-if flatmap)
  (let ((parser (let! ((c (char-if #'alpha-char-p)))
                  (ok (char-code c)))))
    (t:is = (char-code #\a)
          (parse-string parser "a"))

    (t:is = (char-code #\b)
          (parse-string parser "b")))

  (let ((parser (let! ((d1 (char-if #'digit-char-p))
                       (d2 (char-if #'digit-char-p))
                       (d3 (char-if #'digit-char-p)))
                  (ok (+ (* 100 (digit-char-p d1))
                         (* 10  (digit-char-p d2))
                         (* 1   (digit-char-p d3)))))))
    (t:is = 123
          (parse-string parser "123"))

    (t:fail (parse-string parser "12")
            'parser-error)))

(t:define-test progn!
  :depends-on (char-of)
  (let ((a (progn! (char-of #\a)))
        (ab (progn! (char-of #\a) (char-of #\b)))
        (abc (progn! (char-of #\a) (char-of #\b) (char-of #\c))))


    (t:is char= #\a
          (parse-string a "a"))

    (t:fail (parse-string a "z")
            'parser-error)

    (t:is char= #\b
          (parse-string ab "ab"))

    (t:fail (parse-string ab "az")
            'parser-error)

    (t:is char= #\c
          (parse-string abc "abc"))

    (t:fail (parse-string abc "abz")
            'parser-error)))

(t:define-test handle
  :depends-on (eof char-of progn!)

  (let ((parser (handle (eof :eof) (constantly (ok :not-eof)))))
    (t:is eq :eof
          (parse-string parser ""))

    (t:is eq :not-eof
          (parse-string parser "whatever")))

  (let ((parser (handle (progn! (char-of #\a) (char-of #\b))
                        (constantly (ok :recovered)))))
    (t:is eq #\b
          (parse-string parser "ab"))

    (t:is eq :recovered
          (parse-string parser "cd"))

    (t:fail (parse-string parser "ac")
            'parser-error)))

(t:define-test handle-rewind
  :depends-on (ok string-of)
  (let ((parser (handle-rewind (string-of "ab")
                               (constantly (ok :recovered)))))
    (t:is string= "ab"
          (parse-string parser "ab"))

    (t:is eq :recovered
          (parse-string parser "ac"))

    (t:is eq :recovered
          (parse-string parser ""))))

(t:define-test prog1!
  :depends-on (char-of)
  (let ((a (prog1! (char-of #\a)))
        (ab (prog1! (char-of #\a) (char-of #\b)))
        (abc (prog1! (char-of #\a) (char-of #\b) (char-of #\c))))


    (t:is char= #\a (parse-string a "a"))

    (t:fail (parse-string a "z")
            'parser-error)

    (t:is char= #\a
          (parse-string ab "ab"))

    (t:fail (parse-string ab "az")
            'parser-error)

    (t:is char= #\a
          (parse-string abc "abc"))

    (t:fail (parse-string abc "abz")
            'parser-error)))

(t:define-test prog2!
  :depends-on (char-of)
  (let ((ab (prog2! (char-of #\a) (char-of #\b)))
        (abc (prog2! (char-of #\a) (char-of #\b) (char-of #\c))))

    (t:is char= #\b
          (parse-string ab "ab"))

    (t:fail (parse-string ab "az")
            'parser-error)

    (t:is char= #\b
          (parse-string abc "abc"))

    (t:fail (parse-string abc "abz")
            'parser-error)))

(t:define-test or!
  :depends-on (char-of ok)
  (let ((a-or-b-or-c (or! (char-of #\a)
                          (char-of #\b)
                          (char-of #\c)))
        (a-or-b-or-default (or! (char-of #\a)
                                (char-of #\b)
                                (ok :default))))
    (t:is char= #\a
          (parse-string a-or-b-or-c "a"))
    (t:is char= #\b
          (parse-string a-or-b-or-c "b"))
    (t:is char= #\c
          (parse-string a-or-b-or-c "c"))

    (t:fail (parse-string a-or-b-or-c "z")
            'parser-error)

    (t:is equal (mapcar (curry #'format nil "~S")
                        '(#\a #\b #\c))
          (parser-error-expected
            (capture-parse-error a-or-b-or-c "z")))

    (t:is char= #\a
          (parse-string a-or-b-or-default "a"))
    (t:is char= #\b
          (parse-string a-or-b-or-default "b"))
    (t:is eq :default
          (parse-string a-or-b-or-default "z"))))

(t:define-test try!
  :depends-on (string-of or!)
  (let ((parser (or! (try! (string-of "aa"))
                     (try! (string-of "ab"))
                     (try! (string-of "ac")))))
    (t:is string= "aa"
          (parse-string parser "aa"))

    (t:is string= "ab"
          (parse-string parser "ab"))

    (t:is string= "ac"
          (parse-string parser "ac"))

    (t:fail (parse-string parser "az")
            'parser-error)

    (t:is equal (mapcar (curry #'format nil "~S")
                        '("aa" "ab" "ac"))
          (parser-error-expected
            (capture-parse-error parser "az")))))

(t:define-test collect
  :depends-on (char-of)
  (let ((as (collect (char-of #\a))))

    (t:is equal '(#\a)
          (parse-string as "a"))
    (t:is equal '(#\a #\a #\a)
          (parse-string as "aaa"))
    (t:is equal '(#\a #\a #\a)
          (parse-string as "aaaz"))

    (t:is equal ()
          (parse-string as "z"))
    (t:is equal ()
          (parse-string as ""))))

(t:define-test collect1
  :depends-on (char-of)
  (let ((as (collect1 (char-of #\a))))

    (t:is equal '(#\a)
          (parse-string as "a"))
    (t:is equal '(#\a #\a #\a)
          (parse-string as "aaa"))
    (t:is equal '(#\a #\a #\a)
          (parse-string as "aaaz"))

    (t:fail (parse-string as "z")
            'parser-error)
    (t:fail (parse-string as "")
            'parser-error)))

(t:define-test collect-into-string
  :depends-on (char-of)
  (let ((as (collect-into-string (char-of #\a))))

    (t:is string= "a"
          (parse-string as "a"))
    (t:is string= "aaa"
          (parse-string as "aaa"))
    (t:is string= "aaa"
          (parse-string as "aaaz"))

    (t:is string= ""
          (parse-string as "z"))
    (t:is string= ""
          (parse-string as ""))))

(t:define-test sep
  :depends-on (char-of)
  (let ((as (sep (char-of #\a)
                 (char-of #\,))))

    (t:is equal '(#\a)
          (parse-string as "a"))

    (t:is equal '(#\a #\a)
          (parse-string as "a,a"))

    (t:is equal '(#\a #\a #\a)
          (parse-string as "a,a,a"))

    (t:fail (parse-string as "")
            'parser-error)

    (t:fail (parse-string as "a,a,a,")
            'parser-error)))

(t:define-test reduce!
  :depends-on (char-if flatmap)
  (let ((parser (reduce! (lambda (num dig) (+ (* num 10) dig))
                         (flatmap (lambda (c) (ok (digit-char-p c)))
                                  (char-if #'digit-char-p)))))
    (t:is = 1
          (parse-string parser "1"))

    (t:is = 12
          (parse-string parser "12"))

    (t:is = 123
          (parse-string parser "123"))

    (t:is = 123
          (parse-string parser "123."))

    (t:fail (parse-string parser "something else")
            'parser-error))

  (let ((parser (reduce! #'* (let! ((d (char-if #'digit-char-p)))
                               (ok (digit-char-p d)))
                         :initial-parser (ok 1))))
    (t:is = 2
          (parse-string parser "2"))

    (t:is = 6
          (parse-string parser "23"))

    (t:is = 30
          (parse-string parser "235"))

    (t:is = 30
          (parse-string parser "235."))

    (t:is equal 1
          (parse-string parser ""))))



;; JSON Tests

(t:define-test json-numbers
  (t:is = 123
      (decode-json-from-string "123"))

  (t:is = 50
      (decode-json-from-string "000050"))

  (t:is = -123
      (decode-json-from-string "-123"))

  (t:is close-enough 1.5
      (decode-json-from-string "1.5"))

  (t:is close-enough 1.05
      (decode-json-from-string "1.05"))

  (t:is close-enough 1e5
      (decode-json-from-string "1e5"))

  (t:is close-enough 1e5
      (decode-json-from-string "1e+5"))

  (t:is close-enough 1e-5
      (decode-json-from-string "1e-5"))

  (t:is close-enough 15.0
      (decode-json-from-string "1.5e1")))

(t:define-test json-strings
  (t:is string= "hello, world"
      (decode-json-from-string "\"hello, world\""))

  (t:is string= (coerce #(#\" #\\ #\/ #\Backspace #\Page #\Newline #\Return #\Tab) 'string)
      (decode-json-from-string "\"\\\"\\\\\\/\\b\\f\\n\\r\\t\""))

  (t:is string= "(λ (n) (* n n))"
      (decode-json-from-string "\"(\\u03BB (n) (* n n))\"")))

(t:define-test json-arrays
  :depends-on (json-numbers json-strings)
  (t:is equal '(10 20 30)
      (decode-json-from-string "[10,20,30]"))

  (t:is equal '(10)
      (decode-json-from-string "[10]"))

  (t:is equal ()
      (decode-json-from-string "[]"))

  (t:is equal '(10 "string" (20 30 40))
      (decode-json-from-string "[10, \"string\", [20, 30, 40]]"))

  (t:is equal '(10 20 30)
      (decode-json-from-string " [ 10 , 20 , 30 ] ")))

(t:define-test json-objects
  :depends-on (json-numbers json-strings)
  (t:is equal '(("key" . "value"))
      (decode-json-from-string "{\"key\":\"value\"}"))

  (t:is equal '(("one" . 1) ("two" . 2) ("three" . 3))
      (decode-json-from-string "{\"one\":1,\"two\":2,\"three\":3}"))

  (t:is equal '(("object" . (("key" . "value"))))
      (decode-json-from-string "{\"object\":{\"key\":\"value\"}}"))

  (t:is equal '(("key" . "value") ("foo" . "bar"))
      (decode-json-from-string " { \"key\" : \"value\" , \"foo\" : \"bar\" }"))

  (t:is equal '(("key" . "value") ("foo" . "bar"))
        (decode-json-from-string
          (format nil "{~%  \"key\": \"value\",~%  \"foo\": \"bar\"~%}"))))



;; Tiny C Tests

(t:define-test c-function
  (t:is equal '((:function "empty" ()))
        (parse-tiny-c-from-string
          "empty(){}"))

  (t:is equal '((:function "empty" ()))
        (parse-tiny-c-from-string
          "  empty  (  )  {  }  "))

  (t:is equal '((:function "emptyWithArg" ("a")))
        (parse-tiny-c-from-string
          "emptyWithArg(a) {}"))

  (t:is equal '((:function "emptyWithArgs" ("a" "b" "c")))
        (parse-tiny-c-from-string
          "emptyWithArgs(a, b, c) {}")))

(t:define-test c-statements
  (t:is equal '((:function "block" ()
                 (:block)
                 (:block)
                 (:block (:block) (:block))))
        (parse-tiny-c-from-string
          "block() { {} {  }  {{}{}} }"))

  (t:is equal '((:function "returnFn" ()
                 (:return 10)
                 (:return 20)
                 (:return "a")))
        (parse-tiny-c-from-string
          "returnFn() { return 10; return 20;    return     a   ; }"))

  (t:is equal '((:function "whileFn" ()
                 (:while 1 (:block))
                 (:while "a" (:while "b" (:expr "c")))))
        (parse-tiny-c-from-string
          "whileFn() { while (1) {}   while (a) while (b) c; }"))

  (t:is equal '((:function "ifFn" ()
                 (:if 1 (:block))
                 (:if "a" (:if "b" (:expr "c")))))
        (parse-tiny-c-from-string
          "ifFn() { if (1) {}    if (a) if (b) c; }")))

(t:define-test c-expressions
  (t:is equal '((:function "primary" ()
                 (:expr 10)
                 (:expr "a")
                 (:expr 10)))
        (parse-tiny-c-from-string
          "primary() { 10; a; (10); }"))

  (t:is equal '((:function "calls" ()
                 (:expr (:call "foo"))
                 (:expr (:call "add" 1 2))
                 (:expr (:call (:call (:call "fun"))))))
        (parse-tiny-c-from-string
          "calls() { foo(); add(1, 2); fun()()(); }"))

  (t:is equal '((:function "binary" ()
                 (:expr (#\+ 10 20))
                 (:expr (#\* 30 40))
                 (:expr ("==" 1 1))
                 (:expr (#\+ (#\* 1 2) (#\* 3 4)))))
        (parse-tiny-c-from-string
          "binary() { 10 + 20;   30 * 40;   1 == 1;   1 * 2 + 3 * 4; }")))

(t:define-test tiny-c-example
  (t:is equal '((:function "fact" ("n")
                 (:if ("==" "n" 0)
                  (:return 1))
                 (:return (#\* "n" (:call "fact" (#\- "n" 1)))))
                (:function "fib" ("n")
                 (:expr (:assign "a" 0))
                 (:expr (:assign "b" 1))
                 (:while (#\> "n" 0)
                  (:block
                    (:expr (:assign "b" (#\+ "b" "a")))
                    (:expr (:assign "a" (#\- "b" "a")))
                    (:expr (:assign "n" (#\- "n" 1)))))
                 (:return "a"))
                (:function "add" ("a" "b")
                 (:return (#\+ "a" "b")))
                (:function "main" ()
                 (:expr (:call "sayn" (:call "fib" 10)))
                 (:expr (:call "sayn" (:call "fact" 10)))
                 (:return 0)))
        (with-open-file (file (asdf:system-relative-pathname
                                :parsnip/examples
                                #P"examples/tiny-c.c"))
          (parse-tiny-c file))))

;;; New Combinator Tests

(t:define-test choice
  :depends-on (or!)
  (let ((a-or-b-or-c (choice (list (char-of #\a)
                                 (char-of #\b)
                                 (char-of #\c)))))
    (t:is char= #\a (parse-string a-or-b-or-c "a"))
    (t:is char= #\b (parse-string a-or-b-or-c "b"))
    (t:is char= #\c (parse-string a-or-b-or-c "c"))
    (t:fail (parse-string a-or-b-or-c "d") 'parser-error)))

(t:define-test many-till
  :depends-on (collect try!)
  (let ((parser (many-till (char-of #\a) (char-of #\b))))
    (t:is equal '(#\a #\a) (parse-string parser "aab"))
    (t:is equal '() (parse-string parser "b"))
    (t:fail (parse-string parser "aa") 'parser-error)))

(t:define-test optional
  :depends-on (or! ok)
  (let ((parser (optional (string-of "hello"))))
    (t:is string= "hello" (parse-string parser "hello"))
    (t:is eq nil (parse-string parser "world"))
    (t:is eq nil (parse-string parser ""))))

(t:define-test lookahead
  :depends-on (try!)
  (let ((parser (let! ((a (lookahead (string-of "abc")))
                       (b (string-of "abc")))
                  (ok (list a b)))))
    (t:is equal '("abc" "abc") (parse-string parser "abc")))
  (let ((parser (lookahead (string-of "a"))))
    (with-input-from-string (s "a")
      (parse parser s)
      (t:is char= #\a (read-char s)))))

(t:define-test not-followed-by
  :depends-on (try! ok fail)
  (let ((parser (prog1! (string-of "a")
                        (not-followed-by (string-of "b")))))
    (t:is string= "a" (parse-string parser "ac"))
    (t:fail (parse-string parser "ab") 'parser-error))
  (with-input-from-string (s "a")
    (parse (not-followed-by (string-of "b")) s)
    (t:is char= #\a (read-char s))))

(t:define-test chainl1
  :depends-on (or! let! ok digit)
  (let ((expr (chainl1 (natural)
                       (choice (list (prog2! (char-of #\+) (ok #'+))
                                     (prog2! (char-of #\-) (ok #'-)))))))
    (t:is = 10 (parse-string expr "10"))
    (t:is = 15 (parse-string expr "10+5"))
    (t:is = 8 (parse-string expr "10-2"))
    (t:is = 6 (parse-string expr "10-2-2"))))

(t:define-test chainr1
  :depends-on (or! let! ok digit)
  (let* ((pow (lambda (b e) (expt b e)))
         (pow-op (prog2! (char-of #\^) (ok pow)))
         (expr (chainr1 (natural) pow-op)))
    (t:is = 10 (parse-string expr "10"))
    (t:is = 1000 (parse-string expr "10^3"))
    (t:is = 256 (parse-string expr "4^2^2")) ; 4^(2^2), not (4^2)^2
    ))

