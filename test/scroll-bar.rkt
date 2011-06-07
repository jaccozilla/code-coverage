#lang racket
(require racket/gui/base)

(define choices (list "choice1" "choice2" "choice3" "choice4"))
;(define choices (list "choice1" "choice2" "choice3" "choice4" "choice5" "choice6" "choice7" "choice8"))

;test 1 - has the bug
(define dialog (instantiate dialog% ("test 1")))
(new message% [parent dialog] [label "here is a long message for test 1"])
(define list-box (new list-box% 
                      [label ""]
                      [choices choices]
                      [parent dialog]
                      [style '(multiple)]
                      ))
(send dialog show #t)
