#lang setup/infotab
(define name "Multi-File Code Coverage")

(define drracket-tools (list (list "tool.rkt")))
(define drracket-name name)

(define multi-file-code-coverage-info-file #t) ;used by info-helper to find this file

;Names, labels, and other items that the docs and source code will have in common
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
  `("Updated Documentation"))
(define version "0.4")
(define repositories '("4.x"))
(define scribblings '(("code-coverage.scrbl" ())))
