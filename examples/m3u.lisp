;;; examples/m3u.lisp - M3U Playlist Parser Example

;;; Copyright 2026 Brian O'Reilly <fade@deepsky.com>
;;; BSD-3-Clause
;;;
;;; This example demonstrates a more complex parser for the M3U playlist format,
;;; showcasing a wide range of `parsnip` combinators, including the newly added
;;; ones for expression parsing, lookahead, and optional parsing.

(defpackage #:xyz.shunter.parsnip.examples.m3u
  (:nicknames #:m3u)
  (:use #:cl #:parsnip)
  (:export #:parse-m3u
           #:parse-m3u-file
           #:m3u-playlist
           #:m3u-playlist-metadata
           #:m3u-playlist-tracks
           #:m3u-track
           #:m3u-track-duration
           #:m3u-track-title
           #:m3u-track-path))

(in-package #:xyz.shunter.parsnip.examples.m3u)

;;; --- Data Structures ---

(defstruct m3u-playlist
  (metadata nil :type list)
  (tracks nil :type list))

(defstruct m3u-track
  (duration 0 :type integer)
  (title "" :type string)
  (path "" :type string))

;;; --- Basic & Utility Parsers ---

(defun whitespace ()
  "Parse zero or more spaces or tabs (not newlines)."
  (skip-many (char-in #(#\Space #\Tab))))

(defparser comment-line ()
  (prog2! (char-of #\;)
          (let! ((chars (many-till (any-char) (lookahead (end-of-line)))))
            (ok (coerce chars 'string)))
          (end-of-line)))

;;; --- Arithmetic Expression Parser (for EXTINF duration) ---

(defun parens (parser)
  "Parse content surrounded by parentheses with optional whitespace."
  (between (progn! (char-of #\() (whitespace))
           parser
           (progn! (whitespace) (char-of #\)))))

(defparser number-expr ()
  (let! ((_ (whitespace))
         (num (natural))
         (_ (whitespace)))
    (ok num)))

;; Operator parsers - parse the operator character and return the function
(defun add-op ()
  (progn! (char-of #\+) (whitespace) (ok #'+)))

(defun sub-op ()
  (progn! (char-of #\-) (whitespace) (ok #'-)))

(defun mul-op ()
  (progn! (char-of #\*) (whitespace) (ok #'*)))

(defun div-op ()
  (progn! (char-of #\/) (whitespace) (ok #'/)))

;; `term` handles multiplication and division (higher precedence)
(defparser term-expr ()
  (chainl1 (or! (parens 'expr-parser)
                'number-expr)
           (or! (mul-op) (div-op))))

;; `expr` handles addition and subtraction (lower precedence)
(defparser expr-parser ()
  (chainl1 'term-expr
           (or! (add-op) (sub-op))))

;;; --- M3U Specific Parsers ---

(defparser extm3u-header ()
  (prog1!
   (string-of "#EXTM3U")
   (end-of-line)))

;; Parse a metadata key: uppercase letters, optionally with underscores/hyphens.
(defparser metadata-key ()
  (let! ((chars (collect1 (char-if (lambda (c)
                                     (or (upper-case-p c)
                                         (char= c #\_)
                                         (char= c #\-)))))))
    (ok (intern (coerce chars 'string) :keyword))))

;; Parse a metadata line like #KEY:value (e.g., #PLAYLIST:, #DURATION:, #CURATOR:).
(defparser metadata-line ()
  (let! ((_ (char-of #\#))
         (_ (not-followed-by (string-of "EXTINF")))
         (_ (not-followed-by (string-of "EXTM3U")))
         (key 'metadata-key)
         (_ (char-of #\:))
         (chars (many-till (any-char) (lookahead (end-of-line)))))
    (ok (cons key (coerce chars 'string)))))

(defparser extinf-line ()
  (let! ((_ (string-of "#EXTINF:"))
         (duration (or! 'expr-parser
                        (let! ((_ (char-of #\-)) (n (natural)))
                          (ok (- n)))))
         (_ (char-of #\,))
         (chars (many-till (any-char) (lookahead (end-of-line)))))
    (ok (list :duration duration :title (coerce chars 'string)))))

(defparser path-line ()
  (let! ((_ (not-followed-by (char-of #\#)))
         (chars (many-till (any-char) (lookahead (end-of-line)))))
    (ok (coerce chars 'string))))

(defparser track-entry ()
  (let! ((inf 'extinf-line)
         (_ (end-of-line))
         (path (optional 'path-line)))
    (ok (make-m3u-track :duration (getf inf :duration)
                        :title (getf inf :title)
                        :path (or path "")))))

;; Parse a blank line (only whitespace before end-of-line).
(defparser blank-line ()
  (prog1! (whitespace) (lookahead (end-of-line)) (ok :blank)))

(defparser playlist-line ()
  (choice (list (try! 'track-entry)
                (try! 'metadata-line)
                'comment-line
                'blank-line)))

(defparser m3u-parser ()
  (let! ((_ 'extm3u-header)
         (lines (many-till (let! ((line 'playlist-line)
                                  (_ (optional (end-of-line))))
                             (ok line))
                           (eof))))
    (let ((playlist (make-m3u-playlist)))
      (dolist (line lines playlist)
        (cond
          ((consp line) ; Metadata
           (push line (m3u-playlist-metadata playlist)))
          ((typep line 'm3u-track)
           (push line (m3u-playlist-tracks playlist)))))
      (setf (m3u-playlist-metadata playlist) (nreverse (m3u-playlist-metadata playlist)))
      (setf (m3u-playlist-tracks playlist) (nreverse (m3u-playlist-tracks playlist)))
      (ok playlist))))

;;; --- Toplevel Function ---

(defun parse-m3u (string)
  "Parses an M3U playlist string into an M3U-PLAYLIST struct."
  (parse 'm3u-parser (make-string-input-stream string)))

(defun parse-m3u-file (pathname)
  "Parses an M3U playlist file into an M3U-PLAYLIST struct."
  (with-open-file (stream pathname :direction :input)
    (parse 'm3u-parser stream)))

;;; --- Example Usage ---

(defun run-example ()
  (let ((m3u-string "#EXTM3U
#PLAYLIST:My Awesome Mix
#EXTINF:(3*60)+45,Artist - Song 1
/music/song1.mp3
#EXTINF:-1,Artist - Song 2
/music/song2.flac
#EXTINF:180,Artist - Song 3
/music/song3.ogg
"))
    (let ((playlist (parse-m3u m3u-string)))
      (format t "--- Parsed M3U Playlist ---~%")
      (format t "Metadata:~%")
      (dolist (meta (m3u-playlist-metadata playlist))
        (format t "  ~A: ~A~%" (car meta) (cdr meta)))
      (format t "Tracks:~%")
      (dolist (track (m3u-playlist-tracks playlist))
        (format t "  Title: ~A~%" (m3u-track-title track))
        (format t "    Duration: ~A seconds~%" (m3u-track-duration track))
        (format t "    Path: ~A~%" (m3u-track-path track)))
      (assert (= (m3u-track-duration (first (m3u-playlist-tracks playlist))) 225))
      (assert (= (m3u-track-duration (second (m3u-playlist-tracks playlist))) -1))
      (assert (= (m3u-track-duration (third (m3u-playlist-tracks playlist))) 180))
      (format t "~%--- Example Finished Successfully ---~%"))))

