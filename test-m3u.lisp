;;; test-m3u.lisp - Tests for the M3U playlist parser example

(defpackage #:xyz.shunter.parsnip.test-m3u
  (:use #:cl #:parachute)
  (:local-nicknames (#:m3u #:xyz.shunter.parsnip.examples.m3u)))

(in-package #:xyz.shunter.parsnip.test-m3u)

(define-test m3u-parser
  :parent NIL)

(define-test simple-playlist
  :parent m3u-parser
  (let ((playlist (m3u:parse-m3u "#EXTM3U
#EXTINF:180,Test Song
/path/to/song.mp3
")))
    (is = 1 (length (m3u:m3u-playlist-tracks playlist)))
    (let ((track (first (m3u:m3u-playlist-tracks playlist))))
      (is = 180 (m3u:m3u-track-duration track))
      (is string= "Test Song" (m3u:m3u-track-title track))
      (is string= "/path/to/song.mp3" (m3u:m3u-track-path track)))))

(define-test playlist-metadata
  :parent m3u-parser
  (let ((playlist (m3u:parse-m3u "#EXTM3U
#PLAYLIST:My Favorite Songs
#EXTINF:120,Song One
/music/one.mp3
")))
    (is = 1 (length (m3u:m3u-playlist-metadata playlist)))
    (is equal '(:playlist . "My Favorite Songs")
        (first (m3u:m3u-playlist-metadata playlist)))))

(define-test expression-duration
  :parent m3u-parser
  (let ((playlist (m3u:parse-m3u "#EXTM3U
#EXTINF:(3*60)+45,Long Song
/path.mp3
")))
    (is = 225 (m3u:m3u-track-duration (first (m3u:m3u-playlist-tracks playlist))))))

(define-test negative-duration
  :parent m3u-parser
  (let ((playlist (m3u:parse-m3u "#EXTM3U
#EXTINF:-1,Unknown Duration
/path.mp3
")))
    ;; -1 indicates indefinite duration (typically a live stream)
    (is = -1 (m3u:m3u-track-duration (first (m3u:m3u-playlist-tracks playlist))))))

(define-test multiple-tracks
  :parent m3u-parser
  (let ((playlist (m3u:parse-m3u "#EXTM3U
#EXTINF:180,Song A
/a.mp3
#EXTINF:240,Song B
/b.mp3
#EXTINF:300,Song C
/c.mp3
")))
    (is = 3 (length (m3u:m3u-playlist-tracks playlist)))
    (is = 180 (m3u:m3u-track-duration (first (m3u:m3u-playlist-tracks playlist))))
    (is = 240 (m3u:m3u-track-duration (second (m3u:m3u-playlist-tracks playlist))))
    (is = 300 (m3u:m3u-track-duration (third (m3u:m3u-playlist-tracks playlist))))))

(define-test complex-expression
  :parent m3u-parser
  (let ((playlist (m3u:parse-m3u "#EXTM3U
#EXTINF:(2+3)*60,Five Minutes
/path.mp3
")))
    (is = 300 (m3u:m3u-track-duration (first (m3u:m3u-playlist-tracks playlist))))))

(define-test blank-lines
  :parent m3u-parser
  (let ((playlist (m3u:parse-m3u "#EXTM3U
#PLAYLIST:With Blanks

#EXTINF:100,Song One
/one.mp3

#EXTINF:200,Song Two
/two.mp3
")))
    (is = 1 (length (m3u:m3u-playlist-metadata playlist)))
    (is = 2 (length (m3u:m3u-playlist-tracks playlist)))))

(define-test extended-metadata
  :parent m3u-parser
  (let ((playlist (m3u:parse-m3u "#EXTM3U
#PLAYLIST:Test Playlist
#CURATOR:Test DJ
#DESCRIPTION:A test playlist
#EXTINF:100,Song
/song.mp3
")))
    (is = 3 (length (m3u:m3u-playlist-metadata playlist)))
    (is equal '(:PLAYLIST . "Test Playlist") (first (m3u:m3u-playlist-metadata playlist)))
    (is equal '(:CURATOR . "Test DJ") (second (m3u:m3u-playlist-metadata playlist)))
    (is equal '(:DESCRIPTION . "A test playlist") (third (m3u:m3u-playlist-metadata playlist)))))

;;; --- Integration Test: Real M3U File ---

(define-test underworld-playlist-file
  :parent m3u-parser
  (let* ((file-path (asdf:system-relative-pathname :parsnip/examples
                                                    "examples/underworld-and-friends.m3u"))
         (playlist (m3u:parse-m3u-file file-path)))
    ;; Check that we got a playlist
    (true (typep playlist 'm3u:m3u-playlist))
    ;; Check metadata - should have PLAYLIST, PHASE, DURATION, CURATOR, DESCRIPTION
    (is = 5 (length (m3u:m3u-playlist-metadata playlist)))
    (is equal '(:PLAYLIST . "Underworld & Friends")
        (first (m3u:m3u-playlist-metadata playlist)))
    ;; Check tracks - should have 45 tracks
    (is = 45 (length (m3u:m3u-playlist-tracks playlist)))
    ;; Verify first track
    (let ((first-track (first (m3u:m3u-playlist-tracks playlist))))
      (is string= "Underworld - Born Slippy (Nuxx)" (m3u:m3u-track-title first-track))
      (is = -1 (m3u:m3u-track-duration first-track))) ; -1 = indefinite/live stream
    ;; Verify last track
    (let ((last-track (alexandria:lastcar (m3u:m3u-playlist-tracks playlist))))
      (is string= "Underworld - Jumbo" (m3u:m3u-track-title last-track)))))
