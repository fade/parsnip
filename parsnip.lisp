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
           #:none-of
           #:string-of
           #:eof
           
           #:any-char
           #:a-space
           #:spaces
           #:newline
           #:tab
           #:upper
           #:lower
           #:alpha-num
           #:letter
           #:hex-digit
           #:oct-digit
           #:crlf
           #:end-of-line

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
           #:between

           #:collect
           #:collect1
           #:many
           #:many1
           #:collect-into-string
           #:sep
           #:sep-by
           #:end-by
           #:end-by1
           #:parse-count
           #:many-till

           #:reduce!
           #:skip
           #:skip-many

           #:optional
           #:lookahead
           #:not-followed-by
           #:chainl
           #:chainl1
           #:chainr
           #:chainr1

           #:digit
           #:natural
           #:sign
           #:an-integer
           #:a-float

           #:defparser

           #:parse

           ;; New Parsec-compatible combinators
           #:label
           #:unexpected
           #:option
           #:sep-end-by
           #:sep-end-by1
           #:skip-many1

           ;; Parser state access
           #:get-position
           #:get-line
           #:get-column
           #:get-input

           ;; Expression parser
           #:build-expression-parser
           #:infix
           #:prefix
           #:postfix
           #:assoc-left
           #:assoc-right
           #:assoc-none

           ;; Token/Lexer support
           #:make-language-def
           #:make-token-parser
           #:token-parser-identifier
           #:token-parser-reserved
           #:token-parser-operator
           #:token-parser-reserved-op
           #:token-parser-char-literal
           #:token-parser-string-literal
           #:token-parser-natural
           #:token-parser-integer
           #:token-parser-float
           #:token-parser-natural-or-float
           #:token-parser-decimal
           #:token-parser-hexadecimal
           #:token-parser-octal
           #:token-parser-symbol
           #:token-parser-lexeme
           #:token-parser-white-space
           #:token-parser-parens
           #:token-parser-braces
           #:token-parser-angles
           #:token-parser-brackets
           #:token-parser-semi
           #:token-parser-comma
           #:token-parser-colon
           #:token-parser-dot
           #:token-parser-semi-sep
           #:token-parser-semi-sep1
           #:token-parser-comma-sep
           #:token-parser-comma-sep1
           #:deftoken

           ;; Permutation parsing
           #:permute
           #:perm-opt
           #:perm-req

           ;; Debugging
           #:parser-trace
           #:parser-traced))

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

(defun none-of (charbag &optional message)
  "Return a parser that consumes a character that is not in the given charbag."
  (declare (inline char-if))
  (check-type charbag sequence)
  (char-if (lambda (c) (not (find c charbag)))
           (or message (format nil "None of ~S" charbag))))

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

(defun between (open middle close)
  "Return a parser that consumes OPEN, then MIDDLE, then CLOSE, and returns MIDDLE's result."
  (let! ((_open open)
         (m middle)
         (_close close))
    (ok m)))

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

(defun many (parser)
  "Parses zero or more occurrences of PARSER. Alias for COLLECT."
  (collect parser))

(defun many1 (parser)
  "Parses one or more occurrences of PARSER. Alias for COLLECT1."
  (collect1 parser))

(defun collect-into-string (char-parser)
  "Return a parser that runs the given character parser until failure, and collects all characters into a string."
  (let! ((chars (many char-parser)))
    (ok (coerce chars 'string))))

(defun sep (value-parser sep-parser)
  "Return a parser that accepts a sequence of VALUE-PARSER input separated by SEP-PARSER input; such as values separated by commas."
  (let! ((first value-parser)
         (rest (collect (progn! sep-parser
                                value-parser))))
    (ok (list* first rest))))

(defun sep-by (value-parser sep-parser)
  "Return a parser that accepts zero or more VALUE-PARSER inputs separated by SEP-PARSER input."
  (or! (sep value-parser sep-parser) (ok '())))

(defun end-by (p sep)
  "Parse zero or more occurrences of p, separated and ended by sep."
  (collect (prog1! p sep)))

(defun end-by1 (p sep)
  "Parse one or more occurrences of p, separated and ended by sep."
  (collect1 (prog1! p sep)))

(defun parse-count (n parser)
  "Parse N occurrences of PARSER and return a list of the results."
  (if (<= n 0)
      (ok '())
      (let! ((x parser)
             (xs (parse-count (1- n) parser)))
        (ok (cons x xs)))))

(defun many-till (parser end)
  "Return a parser that applies `parser` zero or more times until `end` succeeds.
Returns a list of the results from `parser`."
  (labels ((many-till-p ()
             (or! (let! ((_ (try! end))) (ok '())) ; If 'end' succeeds, return empty list
                  (let! ((x parser)
                         (xs (many-till-p))) ; If 'parser' succeeds, recurse
                    (ok (cons x xs))))))
    (many-till-p)))

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
  "Return a parser that tries the given parser, and returns its result on success, or NIL on an empty failure.
Uses TRY! to ensure backtracking on partial matches."
  (or! (try! parser) (ok nil)))

(defun lookahead (parser)
  "Return a parser that runs the given parser without consuming any input.
Requires a seekable stream."
  (lambda (pstream eok cok efail cfail)
    (declare (ignore cok cfail))
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

(defun chainl (p op default)
  "Like chainl1, but returns DEFAULT if p does not parse at all."
  (or! (chainl1 p op) (ok default)))

(defun chainl1 (p op)
  "Parse one or more occurrences of `p`, separated by `op`.
Returns a value obtained by a left-associative application of all functions
returned by `op` to the values returned by `p`."
  (let! ((x p) ; parse initial value
         (f-y-pairs (collect (let! ((f op) ; parse operator (returns a function)
                                    (y p))  ; parse next operand (returns a value)
                               (ok (list f y)))))) ; return a list (function function value)
    (ok (reduce (lambda (acc pair)
                  (funcall (first pair) acc (second pair)))
                f-y-pairs
                :initial-value x))))

(defun chainr (p op default)
  "Like chainr1, but returns DEFAULT if p does not parse at all."
  (or! (chainr1 p op) (ok default)))

(defun chainr1 (p op)
  "Parse one or more occurrences of `p`, separated by `op`.
Returns a value obtained by a right-associative application of all functions
returned by `op` to the values returned by `p`."
  (labels ((chainr1-p-recursive ()
             (let! ((x p)
                    (f-y-parser (optional (let! ((f op)
                                                  (y (chainr1-p-recursive)))
                                            (ok (funcall f x y))))))
               (ok (if f-y-parser f-y-parser x)))))
    (chainr1-p-recursive)))


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


;;;
;;; Additions by Brian O'Reilly <fade@deepsky.com>
;;;

(defun any-char ()
  "Parses any single character."
  (char-if (constantly t) "any character"))

(defun a-space ()
  "Parses a single whitespace character."
  (char-if #'whitespacep "space"))

(defun spaces ()
  "Parses zero or more whitespace characters, discarding them."
  (skip-many (a-space)))

(defun newline ()
  "Parses a newline character."
  (char-of #\Newline "newline"))

(defun tab ()
  "Parses a tab character."
  (char-of #\Tab "tab"))

(defun upper ()
  "Parses an uppercase letter."
  (char-if #'upper-case-p "uppercase letter"))

(defun lower ()
  "Parses a lowercase letter."
  (char-if #'lower-case-p "lowercase letter"))

(defun alpha-num ()
  "Parses an alphanumeric character."
  (char-if #'alphanumericp "letter or digit"))

(defun letter ()
  "Parses an alphabetic character."
  (char-if #'alpha-char-p "letter"))

(defun hex-digit ()
  "Parses a hexadecimal digit."
  (digit 16))

(defun oct-digit ()
  "Parses an octal digit."
  (digit 8))

(defun crlf ()
  "Parses a carriage return followed by a newline."
  (string-of (format nil "~C~C" #\Return #\Newline) "crlf"))

(defun end-of-line ()
  "Parses a newline or a carriage return-newline sequence."
  (or! (newline) (crlf)))

(defun sign ()
  "Parses a '+' or '-' character."
  (or! (char-of #\+) (char-of #\-)))

(defun an-integer ()
  "Parses a signed integer."
  (let! ((s (optional (sign)))
         (n (natural)))
    (ok (if (and s (char= s #\-))
            (- n)
            n))))

(defun a-digit-char ()
  "Parses a digit character (0-9) and returns the character."
  (char-if #'digit-char-p "digit"))

(defun digits1 ()
  "Parse one or more digits and return as a string."
  (let! ((chars (many1 (a-digit-char))))
    (ok (coerce chars 'string))))

(defun a-float ()
  "Parses a floating-point number."
  (let! ((s (optional (sign)))
         (n (digits1)))
    (let! ((f (optional (let! ((_dot (char-of #\.))
                               (frac (digits1)))
                          _dot  ; suppress unused warning
                          (ok frac))))
           (e (optional (let! ((_e-char (char-in "eE"))
                               (e-sign (optional (sign)))
                               (e-val (digits1)))
                          _e-char  ; suppress unused warning
                          (ok (concatenate 'string "e" (if e-sign (string e-sign) "") e-val))))))
      (if (and (not f) (not e))
          (fail "a-float")
          (let ((str (concatenate 'string (if s (string s) "") n (if f (concatenate 'string "." f) "") (or e ""))))
            (ok (read-from-string str)))))))

;; Misc. helpers

(defun whitespacep (char)
  "Return T if CHAR is a whitespace character."
  (member char '(#\Space #\Tab #\Newline #\Return #\Page #\Linefeed)
          :test #'char=))

;;; ============================================================
;;; New Parsec-compatible combinators
;;; ============================================================

;; Error labeling

(defun label (parser message)
  "Return a parser that, on failure without consuming input, replaces the expected message with MESSAGE.
This is the equivalent of Parsec's <?> operator."
  (check-type message string)
  (lambda (pstream eok cok efail cfail)
    (funcall parser pstream
             eok
             cok
             (lambda (pstream* expected trace)
               (declare (ignore expected))
               (funcall efail pstream* message trace))
             cfail)))

(defun unexpected (message)
  "Return a parser that always fails with an 'unexpected' message.
Use this to report what was found rather than what was expected."
  (check-type message string)
  (lambda (pstream eok cok efail cfail)
    (declare (ignore eok cok cfail))
    (funcall efail pstream (format nil "unexpected ~A" message) ())))

;; Option combinator

(defun option (default parser)
  "Return a parser that tries PARSER and returns its result on success, or DEFAULT on empty failure.
Unlike OPTIONAL which returns NIL, this returns a user-specified default value."
  (or! parser (ok default)))

;; Separator combinators

(defun sep-end-by (value-parser sep-parser)
  "Parse zero or more occurrences of VALUE-PARSER separated by SEP-PARSER, with optional trailing separator.
Handles both 'a,b,c' and 'a,b,c,' equally."
  (or! (sep-end-by1 value-parser sep-parser)
       (ok '())))

(defun sep-end-by1 (value-parser sep-parser)
  "Parse one or more occurrences of VALUE-PARSER separated by SEP-PARSER, with optional trailing separator."
  (let! ((first value-parser)
         (rest (collect (try! (progn! sep-parser value-parser))))
         (_ (optional sep-parser)))
    (ok (cons first rest))))

;; Skip combinator

(defun skip-many1 (parser)
  "Parse one or more occurrences of PARSER, discarding results. Pretend no input was consumed."
  (progn! (skip parser)
          (skip-many parser)))

;;; ============================================================
;;; Parser state access
;;; ============================================================

(defun get-position ()
  "Return a parser that returns the current stream position as a list (position line column)."
  (lambda (pstream eok cok efail cfail)
    (declare (ignore cok efail cfail))
    (funcall eok pstream (save pstream))))

(defun get-line ()
  "Return a parser that returns the current line number (1-based)."
  (lambda (pstream eok cok efail cfail)
    (declare (ignore cok efail cfail))
    (funcall eok pstream (pstream-line pstream))))

(defun get-column ()
  "Return a parser that returns the current column number (0-based)."
  (lambda (pstream eok cok efail cfail)
    (declare (ignore cok efail cfail))
    (funcall eok pstream (pstream-column pstream))))

(defun get-input ()
  "Return a parser that returns the underlying stream."
  (lambda (pstream eok cok efail cfail)
    (declare (ignore cok efail cfail))
    (funcall eok pstream (pstream-stream pstream))))

;;; ============================================================
;;; Expression parser
;;; ============================================================

;; Associativity specifiers
(defconstant +assoc-left+ :left
  "Left associativity for operators.")
(defconstant +assoc-right+ :right
  "Right associativity for operators.")
(defconstant +assoc-none+ :none
  "No associativity for operators.")

;; For convenience in defpackage exports
(defun assoc-left () +assoc-left+)
(defun assoc-right () +assoc-right+)
(defun assoc-none () +assoc-none+)

(defstruct (operator (:constructor nil)
                     (:copier nil)
                     (:predicate nil))
  "Base structure for expression operators.")

(defstruct (infix-op (:include operator)
                     (:constructor make-infix (parser assoc))
                     (:copier nil)
                     (:predicate infix-op-p))
  "An infix binary operator with associativity."
  parser
  assoc)

(defstruct (prefix-op (:include operator)
                      (:constructor make-prefix (parser))
                      (:copier nil)
                      (:predicate prefix-op-p))
  "A prefix unary operator."
  parser)

(defstruct (postfix-op (:include operator)
                       (:constructor make-postfix (parser))
                       (:copier nil)
                       (:predicate postfix-op-p))
  "A postfix unary operator."
  parser)

;; Convenience constructors matching Parsec's API
(defun infix (parser assoc)
  "Create an infix operator. PARSER should return a binary function.
ASSOC should be +assoc-left+, +assoc-right+, or +assoc-none+."
  (make-infix parser assoc))

(defun prefix (parser)
  "Create a prefix operator. PARSER should return a unary function."
  (make-prefix parser))

(defun postfix (parser)
  "Create a postfix operator. PARSER should return a unary function."
  (make-postfix parser))

(defun build-expression-parser (operators term)
  "Build an expression parser from an operator table and a term parser.

OPERATORS is a list of operator lists, ordered from highest to lowest precedence.
Each operator list contains operators at the same precedence level.
Operators are created with INFIX, PREFIX, or POSTFIX.

TERM is a parser for the atomic terms of the expression.

Example:
  (build-expression-parser
    (list
      (list (prefix (progn! (char-of #\\-) (ok #'negate))))
      (list (infix (progn! (char-of #\\*) (ok #'*)) +assoc-left+)
            (infix (progn! (char-of #\\/) (ok #'/)) +assoc-left+))
      (list (infix (progn! (char-of #\\+) (ok #'+)) +assoc-left+)
            (infix (progn! (char-of #\\-) (ok #'-)) +assoc-left+)))
    number)"
  ;; Build from highest precedence to lowest - each level uses the previous
  ;; level as its term parser, so higher precedence binds tighter
  (reduce (lambda (term ops) (make-level-parser ops term))
          operators
          :initial-value term))

(defun make-level-parser (ops term)
  "Create a parser for one precedence level with operators OPS and sub-expression parser TERM."
  (let ((prefix-ops (remove-if-not #'prefix-op-p ops))
        (postfix-ops (remove-if-not #'postfix-op-p ops))
        (infix-ops (remove-if-not #'infix-op-p ops)))
    (let ((prefix-p (if prefix-ops
                        (option #'identity (choice (mapcar #'prefix-op-parser prefix-ops)))
                        (ok #'identity)))
          (postfix-p (if postfix-ops
                         (option #'identity (choice (mapcar #'postfix-op-parser postfix-ops)))
                         (ok #'identity))))
      (if infix-ops
          (make-infix-level-parser infix-ops prefix-p postfix-p term)
          ;; No infix operators, just apply prefix and postfix
          (let! ((pre prefix-p)
                 (x term)
                 (post postfix-p))
            (ok (funcall post (funcall pre x))))))))

(defun make-infix-level-parser (infix-ops prefix-p postfix-p term)
  "Create a parser for a precedence level with infix operators."
  (let* ((left-ops (remove-if-not (lambda (op) (eq (infix-op-assoc op) +assoc-left+)) infix-ops))
         (right-ops (remove-if-not (lambda (op) (eq (infix-op-assoc op) +assoc-right+)) infix-ops))
         (none-ops (remove-if-not (lambda (op) (eq (infix-op-assoc op) +assoc-none+)) infix-ops))
         ;; Parser for a single term with prefix/postfix
         (term-with-fix (let! ((pre prefix-p)
                               (x term)
                               (post postfix-p))
                          (ok (funcall post (funcall pre x))))))
    (cond
      ;; Only left-associative operators
      ((and left-ops (null right-ops) (null none-ops))
       (let ((op-parser (choice (mapcar #'infix-op-parser left-ops))))
         (chainl1 term-with-fix op-parser)))
      ;; Only right-associative operators
      ((and right-ops (null left-ops) (null none-ops))
       (let ((op-parser (choice (mapcar #'infix-op-parser right-ops))))
         (chainr1 term-with-fix op-parser)))
      ;; Only non-associative operators
      ((and none-ops (null left-ops) (null right-ops))
       (let ((op-parser (choice (mapcar #'infix-op-parser none-ops))))
         (let! ((x term-with-fix)
                (rest (optional (let! ((op op-parser)
                                       (y term-with-fix))
                                  (ok (list op y))))))
           (if rest
               (ok (funcall (first rest) x (second rest)))
               (ok x)))))
      ;; Mixed associativity - handle them all
      (t
       (let ((all-op-parsers (mapcar #'infix-op-parser infix-ops)))
         ;; Fall back to left-associative behavior for mixed
         (chainl1 term-with-fix (choice all-op-parsers)))))))

;;; ============================================================
;;; Token/Lexer support
;;; ============================================================

(defstruct (language-def (:constructor make-language-def)
                         (:copier nil))
  "Definition of a language for token parsing."
  (comment-start "" :type string)
  (comment-end "" :type string)
  (comment-line "" :type string)
  (nested-comments nil :type boolean)
  (ident-start nil)           ; parser for identifier start char
  (ident-letter nil)          ; parser for identifier continuation
  (op-start nil)              ; parser for operator start char
  (op-letter nil)             ; parser for operator continuation
  (reserved-names nil :type list)
  (reserved-op-names nil :type list)
  (case-sensitive t :type boolean))

(defstruct (token-parser (:constructor %make-token-parser)
                         (:copier nil))
  "A collection of token parsers generated from a language definition."
  identifier
  reserved
  operator
  reserved-op
  char-literal
  string-literal
  natural
  integer
  float
  natural-or-float
  decimal
  hexadecimal
  octal
  symbol
  lexeme
  white-space
  parens
  braces
  angles
  brackets
  semi
  comma
  colon
  dot
  semi-sep
  semi-sep1
  comma-sep
  comma-sep1)

(defun make-token-parser (lang-def)
  "Create a token parser from a language definition.
Returns a TOKEN-PARSER structure with parser functions for common token types."
  (let* ((white-space (make-white-space-parser lang-def))
         (lexeme-fn (lambda (p)
                      (prog1! p white-space)))
         (symbol-fn (lambda (s)
                      (funcall lexeme-fn (string-of s)))))
    ;; Build individual parsers
    (let ((semi (funcall symbol-fn ";"))
          (comma (funcall symbol-fn ","))
          (colon (funcall symbol-fn ":"))
          (dot (funcall symbol-fn ".")))
      (%make-token-parser
       :identifier (make-identifier-parser lang-def lexeme-fn)
       :reserved (lambda (name) (make-reserved-parser lang-def name lexeme-fn))
       :operator (make-operator-parser lang-def lexeme-fn)
       :reserved-op (lambda (name) (make-reserved-op-parser lang-def name lexeme-fn))
       :char-literal (funcall lexeme-fn (make-char-literal-parser))
       :string-literal (funcall lexeme-fn (make-string-literal-parser))
       :natural (funcall lexeme-fn (natural))
       :integer (funcall lexeme-fn (an-integer))
       :float (funcall lexeme-fn (a-float))
       :natural-or-float (funcall lexeme-fn (or! (try! (a-float)) (an-integer)))
       :decimal (funcall lexeme-fn (natural 10))
       :hexadecimal (funcall lexeme-fn (progn! (string-of "0x") (natural 16)))
       :octal (funcall lexeme-fn (progn! (string-of "0o") (natural 8)))
       :symbol symbol-fn
       :lexeme lexeme-fn
       :white-space white-space
       :parens (lambda (p) (between (funcall symbol-fn "(") p (funcall symbol-fn ")")))
       :braces (lambda (p) (between (funcall symbol-fn "{") p (funcall symbol-fn "}")))
       :angles (lambda (p) (between (funcall symbol-fn "<") p (funcall symbol-fn ">")))
       :brackets (lambda (p) (between (funcall symbol-fn "[") p (funcall symbol-fn "]")))
       :semi semi
       :comma comma
       :colon colon
       :dot dot
       :semi-sep (lambda (p) (sep-by p semi))
       :semi-sep1 (lambda (p) (sep p semi))
       :comma-sep (lambda (p) (sep-by p comma))
       :comma-sep1 (lambda (p) (sep p comma))))))

(defun make-white-space-parser (lang-def)
  "Create a parser that skips whitespace and comments."
  (let ((line-comment (language-def-comment-line lang-def))
        (block-start (language-def-comment-start lang-def))
        (block-end (language-def-comment-end lang-def)))
    (skip-many
     (or! (a-space)
          (if (and line-comment (plusp (length line-comment)))
              (progn! (string-of line-comment)
                      (skip-many (char-if (lambda (c) (char/= c #\Newline))))
                      (or! (newline) (eof)))
              (fail ""))
          (if (and block-start (plusp (length block-start)))
              (make-block-comment-parser block-start block-end
                                         (language-def-nested-comments lang-def))
              (fail ""))))))

(defun make-block-comment-parser (start end nested)
  "Create a parser for block comments."
  (declare (ignore nested)) ; Simplified: not handling nested comments yet
  (progn! (string-of start)
          (many-till (any-char) (string-of end))))

(defun make-identifier-parser (lang-def lexeme-fn)
  "Create an identifier parser."
  (let ((ident-start (or (language-def-ident-start lang-def) (letter)))
        (ident-letter (or (language-def-ident-letter lang-def) (or! (alpha-num) (char-of #\_)))))
    (funcall lexeme-fn
             (let! ((c ident-start)
                    (cs (collect-into-string (collect ident-letter))))
               (let ((name (concatenate 'string (string c) cs)))
                 (if (member name (language-def-reserved-names lang-def)
                             :test (if (language-def-case-sensitive lang-def)
                                       #'string=
                                       #'string-equal))
                     (fail (format nil "reserved word ~S used as identifier" name))
                     (ok name)))))))

(defun make-reserved-parser (lang-def name lexeme-fn)
  "Create a parser for a specific reserved word."
  (let ((ident-letter (or (language-def-ident-letter lang-def) (or! (alpha-num) (char-of #\_)))))
    (funcall lexeme-fn
             (try! (progn! (if (language-def-case-sensitive lang-def)
                               (string-of name)
                               ;; Case insensitive matching would need more work
                               (string-of name))
                           (not-followed-by ident-letter))))))

(defun make-operator-parser (lang-def lexeme-fn)
  "Create an operator parser."
  (let ((op-start (or (language-def-op-start lang-def)
                      (char-in "!#$%&*+./<=>?@\\^|-~")))
        (op-letter (or (language-def-op-letter lang-def)
                       (char-in "!#$%&*+./<=>?@\\^|-~"))))
    (funcall lexeme-fn
             (let! ((c op-start)
                    (cs (collect-into-string (collect op-letter))))
               (let ((name (concatenate 'string (string c) cs)))
                 (if (member name (language-def-reserved-op-names lang-def) :test #'string=)
                     (fail (format nil "reserved operator ~S used as operator" name))
                     (ok name)))))))

(defun make-reserved-op-parser (lang-def name lexeme-fn)
  "Create a parser for a specific reserved operator."
  (let ((op-letter (or (language-def-op-letter lang-def)
                       (char-in "!#$%&*+./<=>?@\\^|-~"))))
    (funcall lexeme-fn
             (try! (progn! (string-of name)
                           (not-followed-by op-letter))))))

(defun make-char-literal-parser ()
  "Create a character literal parser."
  (between (char-of #\')
           (or! (progn! (char-of #\\) (escape-char))
                (char-if (lambda (c) (and (char/= c #\') (char/= c #\\)))))
           (char-of #\')))

(defun escape-char ()
  "Parse an escape character."
  (or! (progn! (char-of #\n) (ok #\Newline))
       (progn! (char-of #\t) (ok #\Tab))
       (progn! (char-of #\r) (ok #\Return))
       (progn! (char-of #\\) (ok #\\))
       (progn! (char-of #\') (ok #\'))
       (progn! (char-of #\") (ok #\"))
       (progn! (char-of #\0) (ok #\Null))))

(defun make-string-literal-parser ()
  "Create a string literal parser."
  (between (char-of #\")
           (let! ((chars (many (or! (progn! (char-of #\\) (escape-char))
                                   (char-if (lambda (c) (and (char/= c #\") (char/= c #\\))))))))
             (ok (coerce chars 'string)))
           (char-of #\")))

(defmacro deftoken (name lang-def accessor)
  "Define a function NAME that returns the ACCESSOR parser from a token-parser."
  `(defun ,name ()
     (,accessor ,lang-def)))

;;; ============================================================
;;; Permutation parsing
;;; ============================================================

(defstruct (perm-parser (:constructor %make-perm-parser)
                        (:copier nil))
  "A permutation parser that can parse elements in any order."
  ;; List of (parser . required-p) pairs
  parsers
  ;; Function to combine results
  combiner)

(defun perm-req (parser)
  "Create a required element for a permutation parser."
  (cons parser t))

(defun perm-opt (default parser)
  "Create an optional element for a permutation parser with a default value."
  (cons (cons default parser) nil))

(defun permute (&rest elements)
  "Create a parser that parses all ELEMENTS in any order.
Each element is created with PERM-REQ (required) or PERM-OPT (optional with default).
Returns a list of results in the order the elements were specified.

Example:
  (permute (perm-req name-parser)
           (perm-opt 0 age-parser)
           (perm-req id-parser))
  ;; Parses name, age (optional), and id in any order
  ;; Returns (name-value age-value id-value)"
  (if (null elements)
      (ok '())
      (let ((n (length elements)))
        (labels ((get-parser (elem)
                   (if (cdr elem) (car elem) (cdar elem)))
                 (get-default (elem)
                   (if (cdr elem) nil (caar elem)))
                 (is-required (elem)
                   (cdr elem))
                 (try-elements (remaining-indices results)
                   (if (null remaining-indices)
                       ;; All done, return results in original order
                       (ok (loop for i from 0 below n
                                 collect (cdr (assoc i results))))
                       ;; Try to parse one of the remaining elements
                       (let ((parsers
                               (loop for idx in remaining-indices
                                     for elem = (nth idx elements)
                                     collect
                                     (let ((i idx)
                                           (e elem))
                                       (let! ((val (try! (get-parser e))))
                                         (try-elements
                                          (remove i remaining-indices)
                                          (acons i val results)))))))
                         ;; Check if all remaining are optional
                         (let ((all-optional (every (lambda (idx)
                                                      (not (is-required (nth idx elements))))
                                                    remaining-indices)))
                           (if all-optional
                               ;; Can succeed with defaults for remaining
                               (apply #'or!
                                      (append parsers
                                              (list (ok (loop for i from 0 below n
                                                              collect (or (cdr (assoc i results))
                                                                          (get-default (nth i elements))))))))
                               ;; Must parse at least one
                               (if parsers
                                   (apply #'or! parsers)
                                   (fail "permutation element"))))))))
          (try-elements (loop for i from 0 below n collect i) '())))))

;;; ============================================================
;;; Debugging combinators
;;; ============================================================

(defun parser-trace (label)
  "Return a parser that prints the current parse position and remaining input.
Useful for debugging. This parser always succeeds and consumes no input."
  (lambda (pstream eok cok efail cfail)
    (declare (ignore cok efail cfail))
    (format *trace-output* "~&[~A] line ~D, column ~D: next char = ~S~%"
            label
            (pstream-line pstream)
            (pstream-column pstream)
            (peek pstream))
    (funcall eok pstream nil)))

(defun parser-traced (label parser)
  "Return a parser that traces PARSER's execution.
Prints when the parser starts and whether it succeeds or fails."
  (lambda (pstream eok cok efail cfail)
    (format *trace-output* "~&[~A] entering at line ~D, column ~D~%"
            label (pstream-line pstream) (pstream-column pstream))
    (funcall parser pstream
             (lambda (pstream* value)
               (format *trace-output* "~&[~A] succeeded (no consume) with ~S~%"
                       label value)
               (funcall eok pstream* value))
             (lambda (pstream* value)
               (format *trace-output* "~&[~A] succeeded (consumed) with ~S~%"
                       label value)
               (funcall cok pstream* value))
             (lambda (pstream* expected trace)
               (format *trace-output* "~&[~A] failed (no consume), expected ~S~%"
                       label expected)
               (funcall efail pstream* expected trace))
             (lambda (pstream* expected trace)
               (format *trace-output* "~&[~A] failed (consumed), expected ~S~%"
                       label expected)
               (funcall cfail pstream* expected trace)))))

;;; ============================================================
;;; Macro for defining named parsers with tracing
;;; ============================================================

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

;;; ============================================================
;;; Top-level evaluation and error handling
;;; ============================================================

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
