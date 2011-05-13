#lang setup/infotab
(define drracket-name "Multi-File Code Coverage")
(define drracket-tools (list (list "tool.rkt")))

(define name "Multi-File Code Coverage")

(define blurb
  `("Extends code coverage highlighting to multiple files"))
(define categories `(devtools))
(define primary-file '("main.rkt"))
(define release-notes 
  `("Rewording of dialog boxes to be more clear"))
(define version "0.2")
(define repositories '("4.x"))
(define scribblings '(("code-coverage.scrbl" ())))
