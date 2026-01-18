;;; parsnip.lisp - Parsnip library implementation

;;; Copyright 2021 Samuel Hunter <samuel (at) shunter (dot) xyz>
;;; BSD-3-Clause

(defpackage #:xyz.shunter.parsnip
  (:documentation "Monadic parser combinator library")
  (:nicknames #:parsnip)
  (:use #:cl)
  (:import-from #:alexandria
                #:curry
                #:rcurry
                #:string-designator)
  (:export #:parser-error
           #:parser-error-line
           #:parser-error-column
           #:parser-error-expected
           #:parser-error-return-trace

           #:ok
           #:fail
           #:char-if
           #:char-of
           #:char-in
           #:string-of
           #:eof

           #:flatmap
           #:let!
           #:handle
           #:handle-rewind
           #:try!

           #:progn!
           #:prog1!
           #:prog2!
           #:or!
           #:choice

           #:collect
           #:collect1
           #:collect-into-string
           #:sep
           #:many-till

           #:reduce!
           #:skip
           #:skip-many

           #:optional
           #:lookahead
           #:not-followed-by
           #:chainl1
           #:chainr1

           #:digit
           #:natural

           #:defparser

           #:parse))

(in-package #:xyz.shunter.parsnip)



(deftype function-designator ()
  '(or function symbol))

(defstruct (parse-stream (:constructor parse-stream (stream))
                         (:conc-name pstream-)
                         (:copier nil)
                         (:predicate nil))
  (stream nil :read-only t)
  (line 1)
  (column 0))

(defun save (pstream)
  "Return the position, line, and column of PSTREAM."
  (with-accessors ((stream pstream-stream)
                   (line pstream-line)
                   (column pstream-column)) pstream
    (list (file-position stream)
          line
          column)))

(defun rewind (pstream snapshot)
  "Set the (stream) position, line, and column of PSTREAM with the SNAPSHOT received from SAVE."
  (with-accessors ((stream pstream-stream)
                   (line pstream-line)
                   (column pstream-column)) pstream
    (destructuring-bind (position line* column*) snapshot
      (file-position stream position)
      (setf line line*
            column column*)))
  pstream)

(defun advance-pstream (pstream c)
  "Update the line and column of PSTREAM given the accepted char C and return PSTREAM."
  (with-accessors ((line pstream-line)
                   (column pstream-column)) pstream
    (if (char= c #\Newline)
        (setf line (1+ line)
              column 0)
        (setf column (1+ column))))
  pstream)

(declaim (inline peek consume))

(defun peek (pstream)
  (peek-char nil (pstream-stream pstream) nil))

(defun consume (pstream)
  (advance-pstream pstream (read-char (pstream-stream pstream))))

(defmacro let@ ((&rest bindings) &body body)
  "Anaphoric self-callable LET."
  (let ((names (mapcar #'first bindings))
        (values (mapcar #'second bindings)))
    `(labels ((@ ,names ,@body))
       (@ ,@values))))



;; Primary parsers

(defun ok (value)
  "Return a parser that consumes nothing and returns the given value."
  (lambda (pstream eok cok efail cfail)
    (declare (type function eok)
             (ignore cok efail cfail))
    (funcall eok pstream value)))

(defun fail (expected &optional trace)
  "Return a parser that consumes nothing and fails, reporting the expected value."
  (lambda (pstream eok cok efail cfail)
    (declare (type function efail)
             (ignore eok cok cfail))
    (funcall efail pstream expected trace)))

(declaim (inline char-if))
(defun char-if (predicate &optional message)
  "Return a parser that consumes a character that satisfies the given predicate."
  (check-type predicate function-designator)
  (check-type message (or string-designator null))
  (unless message
    (setf message (format nil "Satisfies ~S" predicate)))

  (lambda (pstream eok cok efail cfail)
    (declare (type parse-stream pstream)
             (type function cok efail)
             (ignore eok cfail))
    (let ((actual (peek pstream)))
      (if (and actual (funcall predicate actual))
          (funcall cok (consume pstream) actual)
          (funcall efail pstream message ())))))
(declaim (notinline char-if))

(defun char-of (char &optional message)
  "Return a parser that consumes the given character."
  (declare (inline char-if))
  (check-type char character)
  (char-if (curry #'char= char)
           (or message (format nil "~S" char))))

(defun char-in (charbag &optional message)
  "Return a parser that consumes a character that's only in the given charbag."
  (declare (inline char-if))
  (check-type charbag sequence)
  (char-if (rcurry #'position charbag)
           (or message (format nil "One of ~S" charbag))))

(defun string-of (string &optional message)
  "Return a parser that consumes the given simple string.
This parser may have consumed input on a failure."
  (check-type string simple-string)
  ;; Reduce to cheaper parsers on edge cases
  (when (string= string "")
    (return-from string-of (ok "")))

  (unless message
    (setf message (format nil "~S" string)))

  (let ((length (length string)))
    (lambda (pstream eok cok efail cfail)
      (declare (type parse-stream pstream)
               (type function cok efail cfail)
               (ignore eok))
      (let* ((actual (make-string length))
             (nread (read-sequence actual (pstream-stream pstream))))
        (cond
          ((< nread length)
           (funcall (if (zerop nread) efail cfail)
                    pstream message ()))
          ((string= string actual)
           (funcall cok pstream actual))
          (t (funcall cfail pstream message ())))))))

(defun eof (&optional value)
  "Return a parser that consumes nothing and returns the given value (or nil) if the input stream is exhausted."
  (lambda (pstream eok cok efail cfail)
    (declare (type parse-stream pstream)
             (type function eok efail)
             (ignore cok cfail))
    (if (peek pstream)
        (funcall efail pstream "EOF" ())
        (funcall eok pstream value))))

;; Parser combinators

(defun flatmap (function parser)
  "Return a new parser that applies the given function to the parser's result, and then runs the parser the function returns.
This function forms the basis of stringing multiple parsers together."
  (check-type function function-designator)
  (lambda (pstream eok cok efail cfail)
    (funcall parser pstream
             ;; eok
             (lambda (pstream* value)
               (funcall (funcall function value)
                        pstream* eok cok efail cfail))
             ;; cok
             (lambda (pstream* value)
               (funcall (funcall function value)
                        pstream* cok cok cfail cfail))
             ;; efail
             efail
             ;; cfail
             cfail)))

(defmacro let! ((&rest bindings) &body body)
  "Return ap arser that runs all given parsers, binds them all to their variables, evaluates the body, and then runs the parser the body returns."
  (reduce (lambda (binding body)
            (let ((name (first binding))
                  (value (second binding)))
              `(flatmap (lambda (,name) ,body)
                        ,value)))
          bindings
          :from-end t
          :initial-value `(progn ,@body)))

(defun handle (parser handler)
  "Return a new parser that, on failure, applies the handler function to the parser's expected value and parse trace (as a list), and then runs the parser the handler returns.
HANDLE does not handle partial-parse failures, which can be recovered from via HANDLE-REWIND."
  (check-type handler function-designator)
  (lambda (pstream eok cok efail cfail)
    (funcall parser pstream
             ;; eok
             eok
             ;; cok
             cok
             ;; efail
             (lambda (pstream* expected trace)
               (funcall (funcall handler expected trace)
                        pstream* eok cok efail cfail))
             ;; cfail
             cfail)))

(defun handle-rewind (parser handler)
  "Return a new parser that saves the stream's current position and, on failure, rewinds the stream, applies the handler function to the parser's expected value and parse trace (as a list), and then runs the parser the handler returns.
HANDLE-REWIND only works when the parser is given a seekable stream."
  (check-type handler function-designator)
  (lambda (pstream eok cok efail cfail)
    (let ((snapshot (save pstream)))
      (funcall parser pstream
               ;; eok
               eok
               ;; cok
               cok
               ;; efail
               (lambda (pstream* expected trace)
                 (funcall (funcall handler expected trace)
                          pstream* eok cok efail cfail))
               ;; cfail
               (lambda (pstream* expected trace)
                 (funcall (funcall handler expected trace)
                          (rewind pstream* snapshot) eok cok efail cfail))))))

(defun try! (parser)
  "Return a new parser that saves the stream's current position and, on failure, rewinds the stream before passing the failure down.
TRY! only works when the parser is given a seekable stream."
  (handle-rewind parser #'fail))

(defun progn! (&rest parsers)
  "Return a parser that strings together all given parsers and returns the last parser's result."
  (reduce (lambda (parser next)
            (flatmap (constantly next) parser))
          parsers
          :from-end t))

(defun prog1! (first &rest parsers)
  "Return a parser that strings together all given parsers and returns the first parser's result."
  (if parsers
      (flatmap (lambda (value)
                 (flatmap (constantly (ok value))
                          (apply #'progn! parsers)))
               first)
      first))

(defun prog2! (first second &rest parsers)
  "Return a parser that strings together all given parsers and returns the second parser's result."
  (if parsers
      (prog1! (progn! first second)
              (apply #'progn! parsers))
      (progn! first second)))

(defun or! (&rest parsers)
  "Return a parser that tries each given parser in order (until a partial-parse failure) and returns the result of the first successful parse."
  (flet ((handle* (parser next)
           (lambda (pstream eok cok efail cfail)
             (funcall parser pstream
                      eok cok
                      ;; efail
                      (lambda (pstream* expected trace)
                        (declare (ignore trace))
                        (funcall next pstream*
                                 eok cok
                                 ;; efail
                                 (lambda (pstream** expected2 trace)
                                   (funcall efail pstream**
                                            (cons expected expected2)
                                            trace))
                                 cfail))
                      ;; cfail
                      cfail))))
    (reduce #'handle*
            parsers
            :from-end t
            :initial-value (lambda (pstream eok cok efail cfail)
                             (declare (ignore eok cok cfail))
                             (funcall efail pstream () ())))))

(defun choice (parsers)
  "Return a parser that tries each parser in the list of `parsers` in order."
  (apply #'or! parsers))

(defun collect (parser)
  "Return a parser that runs the given parser until failure, and collects all results into a list."
  (let@ ((items ()))
    (handle
      (let! ((item parser))
        (@ (cons item items)))
      (lambda (expected trace)
        (declare (ignore expected trace))
        (ok (nreverse items))))))

(defun collect1 (parser)
  "Return a parser that runs the given parser once, keeps parsing until failure, and then collects all results into a list."
  (let@ ((items ())
         (parsed nil))
    (handle
      (let! ((item parser))
        (@ (cons item items)
           t))
      (lambda (expected trace)
        (if parsed
            (ok (nreverse items))
            (fail expected trace))))))

(defun collect-into-string (char-parser)
  "Return a parser that runs the given character parser until failure, and collects all characters into a string."
  (let ((out (make-string-output-stream)))
    (let@ ()
      (handle
        (let! ((c char-parser))
          (write-char c out)
          (@))
        (lambda (expected trace)
          (declare (ignore expected trace))
          (ok (get-output-stream-string out)))))))

(defun sep (value-parser sep-parser)
  "Return a parser that accepts a sequence of VALUE-PARSER input separated by SEP-PARSER input; such as values separated by commas."
  (let! ((first value-parser)
         (rest (collect (progn! sep-parser
                                value-parser))))
    (ok (list* first rest))))

(defun many-till (parser end)
  "Return a parser that applies `parser` zero or more times until `end` succeeds.
Returns a list of the results from `parser`."
  (or! (let! (_ (try! end)) (ok '()))
       (let! ((x parser)
              (xs (many-till parser end)))
         (ok (cons x xs)))))

(defun reduce! (function parser &key initial-parser)
  "Return a parser that keeps running until failure, and reduces its results into one value.
If INITIAL-PARSER is supplied, the parser may succeed without calling FUNCTION by returning INITIAL-PARSER's response."
  (let! ((initial-value (or initial-parser parser)))
    (let@ ((result initial-value))
      (handle
        (let! ((obj parser))
          (@ (funcall function result obj)))
        (lambda (expected trace)
          (declare (ignore expected trace))
          (ok result))))))

(defun skip (parser)
  "Parse and pretend no input was consumed."
  (lambda (pstream eok cok efail cfail)
    (declare (ignore cok cfail))
    (funcall parser pstream
             eok eok
             efail efail)))

(defun skip-many (parser)
  "Keep parsing until failure and pretend no input was consumed."
  (let@ ()
    (handle (flatmap (lambda (value)
                       (declare (ignore value))
                       (@))
                     (skip parser))
            (constantly (ok nil)))))

(defun optional (parser)
  "Return a parser that tries the given parser, and returns its result on success, or NIL on an empty failure."
  (or! parser (ok nil)))

(defun lookahead (parser)
  "Return a parser that runs the given parser without consuming any input.
Requires a seekable stream."
  (lambda (pstream eok cok efail cfail)
    (let ((snapshot (save pstream)))
      (funcall parser pstream
               (lambda (pstream* value)
                 (rewind pstream* snapshot)
                 (funcall eok pstream* value))
               (lambda (pstream* value)
                 (rewind pstream* snapshot)
                 (funcall eok pstream* value))
               efail
               (lambda (pstream* expected trace)
                 (rewind pstream* snapshot)
                 (funcall efail pstream* expected trace))))))

(defun not-followed-by (parser)
  "Return a parser that succeeds if the given parser fails, and fails if it succeeds.
This parser never consumes input. Requires a seekable stream."
  (lambda (pstream eok cok efail cfail)
    (declare (ignore cok))
    (let ((snapshot (save pstream)))
      (funcall parser pstream
               (lambda (pstream* value)
                 (declare (ignore value))
                 (rewind pstream* snapshot)
                 (funcall efail pstream* "something else" ()))
               (lambda (pstream* value)
                 (declare (ignore value))
                 (rewind pstream* snapshot)
                 (funcall efail pstream* "something else" ()))
               (lambda (pstream* expected trace)
                 (declare (ignore expected trace))
                 (rewind pstream* snapshot)
                 (funcall eok pstream* nil))
               (lambda (pstream* expected trace)
                 (declare (ignore expected trace))
                 (rewind pstream* snapshot)
                 (funcall eok pstream* nil))))))

(defun chainl1 (p op)
  "Parse one or more occurrences of `p`, separated by `op`.
Returns a value obtained by a left-associative application of all functions
returned by `op` to the values returned by `p`."
  (let* ((rest (lambda (x)
                 (or! (let! ((f op)
                             (y p))
                        (funcall rest (funcall f x y)))
                      (ok x)))))
    (let! ((x p)) (funcall rest x))))

(defun chainr1 (p op)
  "Parse one or more occurrences of `p`, separated by `op`.
Returns a value obtained by a right-associative application of all functions
returned by `op` to the values returned by `p`."
  (let! ((x p)
         (rest (or! (let! ((f op)
                           (y (chainr1 p op)))
                      (ok (funcall f x y)))
                    (ok x))))
    rest))


;; Toplevel parsers

(defun digit (&optional (radix 10))
  "Consume and return the number value of a digit."
  (check-type radix (integer 2 36))
  (let! ((d (char-if (rcurry #'digit-char-p radix))))
    (ok (digit-char-p d radix))))

(defun natural (&optional (radix 10))
  "Consume and return a natural number."
  (check-type radix (integer 2 36))
  (reduce! (lambda (number d)
             (+ (* number radix) d))
           (digit radix)))

;; Misc. helpers

(defmacro defparser (name () &body (form))
  "Define a parser as a function. It can then be referenced as a function designator."
  `(let ((parser ,form))
     (defun ,name (pstream eok cok efail cfail)
       (let ((line (pstream-line pstream))
             (column (pstream-column pstream)))
         (funcall parser pstream
                  eok
                  cok
                  (lambda (pstream expected trace)
                    (funcall efail pstream expected
                             (cons (list ',name line column) trace)))
                  (lambda (pstream expected trace)
                    (funcall cfail pstream expected
                             (cons (list ',name line column)
                                   trace))))))))



;; Toplevel evaluation converts results into something a little more
;; front-facing for library consumers.

(define-condition parser-error (stream-error)
  ((line :initarg :line :reader parser-error-line :type integer
         :documentation "The (1-based) line number at which the parser failed")
   (column :initarg :column :reader parser-error-column :type integer
           :documentation "The (0-based) column number at which the parser failed")
   (expected :initarg :expected :reader parser-error-expected
             :documentation "An object describing what the parser expected to read")
   (return-trace :initarg :return-trace :reader parser-error-return-trace
                 :documentation "A list of (name line column) objects detailing the state of the parser when it failed"))
  (:report (lambda (condition stream)
             (with-accessors ((err-stream stream-error-stream)
                              (line parser-error-line)
                              (column parser-error-column)
                              (expected parser-error-expected)) condition
               (format stream "~A:~D:~D: Expected ~A on ~S"
                       (ignore-errors (pathname err-stream))
                       line
                       column
                       expected
                       err-stream))))
  (:documentation
    "If a parser fails to read text, it signals a parser-error, containing a stream,
its expected value, and a return trace of parsers."))

(defun signal-failure (pstream expected return-trace)
  "Signal an error depending on the given fail."
  (with-accessors ((stream pstream-stream)
                   (line pstream-line)
                   (column pstream-column)) pstream
    (error 'parser-error
           :stream stream
           :line line
           :column column
           :expected expected
           :return-trace return-trace)))

(defun parse (parser stream)
  "Run a parser through a given stream and raise any failures as a PARSER-ERROR."
  (check-type parser function-designator)
  (check-type stream stream)
  (flet ((take-value (pstream value)
           (declare (ignore pstream))
           value))
    (funcall parser (parse-stream stream)
             #'take-value
             #'take-value
             #'signal-failure
             #'signal-failure)))