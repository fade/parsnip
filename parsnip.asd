;;; parsnip.asd - Parsnip library system definitions

;;; Copyright 2021 Samuel Hunter <samuel (at) shunter (dot) xyz>
;;; Copyright 2026 Brian O'Reilly <fade@deepsky.com>
;;; BSD-3-Clause

(asdf:defsystem #:parsnip
  :description "Parser combinator library"
  :author "Samuel Hunter"
  :license  "BSD 3-Clause"
  :version "0.1.0"

  :homepage "https://sr.ht/~shunter/parsnip/"
  :source-control (:git "https://git.sr.ht/~shunter/parsnip")
  :bug-tracker "https://todo.sr.ht/~shunter/parsnip"
  :mailto "\~\s\h\u\n\t\e\r\/\p\u\b\l\i\c\-\i\n\b\o\x\@\l\i\s\t\s\.\s\r\.\h\t"

  :depends-on (#:alexandria)
  :components ((:file "parsnip"))
  :in-order-to ((asdf:test-op (asdf:test-op :parsnip/test))))

(asdf:defsystem #:parsnip/examples
  :description "Parsnip library examples"
  :author "Samuel Hunter"
  :license "BSD 3-Clause"
  :version "0.1.0"

  :depends-on (#:parsnip
               #:alexandria)
  :components ((:module :examples
                :components ((:file "json")
                             (:file "tiny-c")
                             (:file "m3u"))))
  :in-order-to ((asdf:test-op (asdf:test-op :parsnip/test))))

(asdf:defsystem #:parsnip/test
  :description "Parsnip library test suite"
  :author "Samuel Hunter"
  :license "BSD 3-Clause"
  :version "0.1.0"

  :depends-on (#:parsnip
               #:parachute)
  :components ((:file "test"))
  :perform (asdf:test-op (op c)
             (uiop:symbol-call :parachute :test :xyz.shunter.parsnip.test)))

(asdf:defsystem #:parsnip/test-json
  :description "Test suite for the Parsnip JSON example"
  :author "Brian O'Reilly <fade@deepsky.com>"
  :license "BSD 3-Clause"
  :version "0.1.0"

  :depends-on (#:parsnip/examples
               #:parachute)
  :components ((:file "test-json"))
  :perform (asdf:test-op (op c)
             (uiop:symbol-call :parachute :test :xyz.shunter.parsnip.test-json)))

(asdf:defsystem #:parsnip/test-literals
  :description "Test suite for the new literal parsers"
  :author "Brian O'Reilly <fade@deepsky.com>"
  :license "BSD 3-Clause"
  :version "0.1.0"

  :depends-on (#:parsnip
               #:parachute)
  :components ((:file "test-literals"))
  :perform (asdf:test-op (op c)
             (uiop:symbol-call :parachute :test :xyz.shunter.parsnip.test-literals)))

(asdf:defsystem #:parsnip/test-parsec
  :description "Test suite for Parsec-compatible combinators"
  :author "Brian O'Reilly <fade@deepsky.com>"
  :license "BSD 3-Clause"
  :version "0.1.0"

  :depends-on (#:parsnip
               #:parachute)
  :components ((:file "test-parsec"))
  :perform (asdf:test-op (op c)
             (uiop:symbol-call :parachute :test :xyz.shunter.parsnip.test-parsec)))

(asdf:defsystem #:parsnip/test-m3u
  :description "Test suite for the M3U playlist parser example"
  :author "Brian O'Reilly <fade@deepsky.com>"
  :license "BSD 3-Clause"
  :version "0.1.0"

  :depends-on (#:parsnip/examples
               #:parachute)
  :components ((:file "test-m3u"))
  :perform (asdf:test-op (op c)
             (uiop:symbol-call :parachute :test :xyz.shunter.parsnip.test-m3u)))

(asdf:defsystem #:parsnip/test-tiny-c
  :description "Test suite for the tiny-c parser example"
  :author "Brian O'Reilly <fade@deepsky.com>"
  :license "BSD 3-Clause"
  :version "0.1.0"

  :depends-on (#:parsnip/examples
               #:parachute)
  :components ((:file "test-tiny-c"))
  :perform (asdf:test-op (op c)
             (uiop:symbol-call :parachute :test :xyz.shunter.parsnip.test-tiny-c)))

(asdf:defsystem #:parsnip/test-all
  :description "Run all Parsnip test suites"
  :author "Brian O'Reilly <fade@deepsky.com>"
  :license "BSD 3-Clause"
  :version "0.1.0"

  :depends-on (#:parsnip/test
               #:parsnip/test-json
               #:parsnip/test-literals
               #:parsnip/test-parsec
               #:parsnip/test-m3u
               #:parsnip/test-tiny-c)
  :perform (asdf:test-op (op c)
             (uiop:symbol-call :parachute :test
                               '(:xyz.shunter.parsnip.test
                                 :xyz.shunter.parsnip.test-json
                                 :xyz.shunter.parsnip.test-literals
                                 :xyz.shunter.parsnip.test-parsec
                                 :xyz.shunter.parsnip.test-m3u
                                 :xyz.shunter.parsnip.test-tiny-c))))
