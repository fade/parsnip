;;; test.lisp - Parsnip library test suite

;;; Copyright 2021 Samuel Hunter <samuel (at) shunter (dot) xyz>
;;; Copyright 2026 Brian O'Reilly <fade@deepsky.com>
;;; BSD-3-Clause

(defpackage #:xyz.shunter.parsnip.test
  (:use #:cl
        #:xyz.shunter.parsnip)
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

(t:define-test digit
  :depends-on (char-if)
  (t:is = 7 (parse-string (digit) "7"))
  (t:is = 15 (parse-string (digit 16) "f"))
  (t:fail (parse-string (digit) "a") 'parser-error))

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

    

(t:define-test none-of
  :depends-on (char-if)
  (t:is char= #\z
        (parse-string (none-of "abc") "z"))
  (t:is char= #\1
        (parse-string (none-of "abc") "1"))
  (t:fail (parse-string (none-of "abc") "a")
          'parser-error)
  (t:fail (parse-string (none-of "abc") "b")
          'parser-error)
  (t:fail (parse-string (none-of "abc") "c")
          'parser-error)
  (t:fail (parse-string (none-of "abc") "")
          'parser-error)
  ;; Works with vectors too
  (t:is char= #\d
        (parse-string (none-of #(#\a #\b #\c)) "d"))
  (t:fail (parse-string (none-of #(#\a #\b #\c)) "a")
          'parser-error))

(t:define-test between
  :depends-on (char-of)
  (t:is char= #\b
        (parse-string (between (char-of #\() (char-of #\b) (char-of #\))) "(b)"))
  (t:fail (parse-string (between (char-of #\() (char-of #\b) (char-of #\))) "b)")
          'parser-error)
  (t:fail (parse-string (between (char-of #\() (char-of #\b) (char-of #\))) "(b")
          'parser-error)
  ;; Works with string parsers
  (t:is string= "hello"
        (parse-string (between (char-of #\[) (string-of "hello") (char-of #\])) "[hello]")))

(t:define-test sep-by
  :depends-on (sep)
  (let ((as (sep-by (char-of #\a) (char-of #\,))))
    ;; Zero items is ok (unlike sep)
    (t:is equal '()
          (parse-string as ""))
    (t:is equal '()
          (parse-string as "z"))
    ;; One or more
    (t:is equal '(#\a)
          (parse-string as "a"))
    (t:is equal '(#\a #\a #\a)
          (parse-string as "a,a,a"))))

(t:define-test end-by
  :depends-on (collect)
  (let ((as (end-by (char-of #\a) (char-of #\;))))
    ;; Zero items
    (t:is equal '()
          (parse-string as ""))
    (t:is equal '()
          (parse-string as "z"))
    ;; Items must be followed by separator
    (t:is equal '(#\a)
          (parse-string as "a;"))
    (t:is equal '(#\a #\a #\a)
          (parse-string as "a;a;a;"))
    ;; Without trailing sep on last item, it's a consumed failure
    (t:fail (parse-string as "a;a")
            'parser-error)))

(t:define-test end-by1
  :depends-on (collect1)
  (let ((as (end-by1 (char-of #\a) (char-of #\;))))
    ;; Must have at least one
    (t:fail (parse-string as "")
            'parser-error)
    (t:fail (parse-string as "z")
            'parser-error)
    ;; One or more
    (t:is equal '(#\a)
          (parse-string as "a;"))
    (t:is equal '(#\a #\a)
          (parse-string as "a;a;"))))

(t:define-test parse-count
  :depends-on (char-of)
  ;; Zero count returns empty list
  (t:is equal '()
        (parse-string (parse-count 0 (char-of #\a)) "aaa"))
  ;; Exact count
  (t:is equal '(#\a)
        (parse-string (parse-count 1 (char-of #\a)) "aaa"))
  (t:is equal '(#\a #\a #\a)
        (parse-string (parse-count 3 (char-of #\a)) "aaa"))
  ;; Fails if not enough input
  (t:fail (parse-string (parse-count 3 (char-of #\a)) "aa")
          'parser-error)
  ;; Doesn't consume more than N
  (t:is equal '(#\a #\a)
        (parse-string (parse-count 2 (char-of #\a)) "aaaa")))

(t:define-test chainl
  :depends-on (chainl1)
  (let ((expr (chainl (natural)
                      (prog2! (char-of #\+) (ok #'+))
                      0)))
    ;; Returns default when parser fails
    (t:is = 0 (parse-string expr "abc"))
    (t:is = 0 (parse-string expr ""))
    ;; Normal operation
    (t:is = 10 (parse-string expr "10"))
    (t:is = 15 (parse-string expr "10+5"))))

(t:define-test chainr
  :depends-on (chainr1)
  (let* ((pow (lambda (b e) (expt b e)))
         (expr (chainr (natural)
                       (prog2! (char-of #\^) (ok pow))
                       1)))
    ;; Returns default when parser fails
    (t:is = 1 (parse-string expr "abc"))
    (t:is = 1 (parse-string expr ""))
    ;; Normal operation
    (t:is = 10 (parse-string expr "10"))
    (t:is = 8 (parse-string expr "2^3"))))

(t:define-test string-vs-try-string
  :depends-on (string-of try! or!)
  (let ((boot (string-of "boot"))
        (bool (string-of "bool"))
        (try-boot (try! (string-of "boot")))
        (try-bool (try! (string-of "bool"))))

    ;; string-of consumes input on failure, so the second branch of or! is not tried
    (t:is string= "boot" (parse-string (or! boot bool) "boot"))
    (t:fail (parse-string (or! boot bool) "bool") 'parser-error)
    (t:fail (parse-string (or! boot bool) "booz") 'parser-error)

    ;; try! rewinds on failure, so the second branch of or! is tried
    (t:is string= "boot" (parse-string (or! try-boot try-bool) "boot"))
    (t:is string= "bool" (parse-string (or! try-boot try-bool) "bool"))
    (t:fail (parse-string (or! try-boot try-bool) "booz") 'parser-error)))

(defun mappend-parser (p1 p2)
  (let! ((r1 p1)
         (r2 p2))
    (ok (concatenate 'string r1 r2))))

(t:define-test monoid-properties
  :depends-on (ok collect-into-string string-of)
  (let ((as (collect-into-string (char-of #\a)))
        (bs (collect-into-string (char-of #\b)))
        (str-a (string-of "a")))
    (t:is string= "aabbb" (parse-string (mappend-parser as bs) "aabbb"))
    (t:is string= "aa" (parse-string (mappend-parser (ok "") as) "aabbb"))
    (t:is string= "aa" (parse-string (mappend-parser as (ok "")) "aabbb"))
    (t:is string= "" (parse-string (ok "") "aabbb"))
    (t:is string= "aa" (parse-string (mappend-parser str-a str-a) "aabbb"))
    (t:fail (parse-string (mappend-parser str-a (mappend-parser str-a str-a)) "aabbb") 'parser-error)))
