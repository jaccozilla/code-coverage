#lang setup/infotab
(define name "Multi-File Code Coverage")

(define drracket-tools (list (list "tool.rkt")))
(define drracket-name name)

;keep 
(define tool-name name)
(define button-label "Multi-File Coverage")
(define open-with-label "Open With Uncovered Lines Dialog")
(define coverage-suffix ".rktcov")

;Stuff for PLanet
(define blurb
  `("Extends code coverage highlighting to multiple files"))
(define categories `(devtools))
(define primary-file '("main.rkt"))
(define release-notes 
  `("Locked the uncovered lines dialog box's editor"))
(define version "0.3")
(define repositories '("4.x"))
(define scribblings '(("code-coverage.scrbl" ())))
