;;; examples/m3u.lisp - M3U Playlist Parser Example
;;;
;;; This example demonstrates a more complex parser for the M3U playlist format,
;;; showcasing a wide range of `parsnip` combinators, including the newly added
;;; ones for expression parsing, lookahead, and optional parsing.

(defpackage #:xyz.shunter.parsnip.examples.m3u
  (:use #:cl #:parsnip)
  (:export #:parse-m3u
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

(defparser whitespace ()
  (skip-many (char-in #(#\Space #\Tab))))

(defparser end-of-line ()
  (or! (string-of #.(string #\Newline))
       (string-of #.(concatenate 'string (string #\Return) (string #\Newline)))))

(defparser comment-line ()
  (prog2! (char-of #\;)
          (collect-into-string (many-till (char-if (constantly t)) (lookahead (end-of-line))))
          (end-of-line)))

;;; --- Arithmetic Expression Parser (for EXTINF duration) ---

(defparser parens (parser)
  (prog2! (char-of #\()
          (whitespace)
          parser
          (whitespace)
          (char-of #\))))

(defparser number-expr ()
  (let! ((ws (whitespace))
         (num (natural))
         (ws2 (whitespace)))
    (declare (ignore ws ws2))
    (ok num)))

(defparser add-op () (ok #'+))
(defparser sub-op () (ok #'-))
(defparser mul-op () (ok #'*))
(defparser div-op () (ok #'/))

;; `term` handles multiplication and division (higher precedence)
(defparser term-expr ()
  (chainl1 (or! (parens (lambda (pstream eok cok efail cfail)
                          (funcall (expr-parser) pstream eok cok efail cfail)))
                (number-expr))
           (or! (prog2! (char-of #\*) (whitespace) (mul-op))
                (prog2! (char-of #\/) (whitespace) (div-op)))))

;; `expr` handles addition and subtraction (lower precedence)
(defparser expr-parser ()
  (chainl1 (term-expr)
           (or! (prog2! (char-of #\+) (whitespace) (add-op))
                (prog2! (char-of #\-) (whitespace) (sub-op)))))

;;; --- M3U Specific Parsers ---

(defparser extm3u-header ()
  (prog1!
   (string-of "#EXTM3U")
   (end-of-line)))

(defparser metadata-line ()
  (let! ((_ (try! (string-of "#PLAYLIST:")))
         (value (collect-into-string (many-till (char-if (constantly t)) (lookahead (end-of-line))))))
    (ok (cons :playlist value))))

(defparser extinf-line ()
  (let! ((_ (string-of "#EXTINF:"))
         (duration (or! (expr-parser) (let! (_ (char-of #\-)) (natural))))
         (_ (char-of #\,))
         (title (collect-into-string (many-till (char-if (constantly t)) (lookahead (end-of-line))))))
    (ok (list :duration duration :title title))))

(defparser path-line ()
  (let! ((path (not-followed-by (char-of #\#)))
         (line (collect-into-string (many-till (char-if (constantly t)) (lookahead (end-of-line))))))
    (declare (ignore path))
    (ok line)))

(defparser track-entry ()
  (let! ((inf (extinf-line))
         (_ (end-of-line))
         (path (optional (path-line))))
    (ok (make-m3u-track :duration (getf inf :duration)
                        :title (getf inf :title)
                        :path (or path "")))))

(defparser playlist-line ()
  (choice (list (metadata-line)
                (track-entry)
                (comment-line))))

(defparser m3u-parser ()
  (let! ((_ (extm3u-header))
         (lines (many-till (let! (line (playlist-line))
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
  (parse (m3u-parser) (make-string-input-stream string)))

;;; --- Example Usage ---

(defun run-example ()
  (let ((m3u-string
         "#EXTM3U
#PLAYLIST:My Awesome Mix
; This is a comment
#EXTINF:(3*60)+45,Artist - Song 1
/music/song1.mp3
#EXTINF:-1,Artist - Song 2
/music/song2.flac
#EXTINF:180,Artist - Song 3
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
      (assert (= (m3u-track-duration (second (m3u-playlist-tracks playlist))) 1))
      (assert (= (m3u-track-duration (third (m3u-playlist-tracks playlist))) 180))
      (format t "~%--- Example Finished Successfully ---~%"))))

