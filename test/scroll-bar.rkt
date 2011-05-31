#lang racket
(require racket/gui/base)

(define choices (list "choice1" "choice2" "choice3" "choice4"))
;(define choices (list "choice1" "choice2" "choice3" "choice4" "choice5" "choice6" "choice7" "choice8"))

(define dialog (instantiate dialog% ("dialog")))
(new message% [parent dialog] [label "here is a long message"])
(define list-box (new list-box% 
                      [label ""]
                      [choices choices]
                      [parent dialog]
                      [style '(multiple)]
                      ;[min-height (get-listbox-min-height (length choices))]
                      ;[callback (λ (c e) 
                      ;            (if (> (length (send list-box get-selections)) 0)
                      ;                (if (eq? (send e get-event-type) 'list-box-dclick)
                      ;                    ((button-callback 'open) null null)
                      ;                    (enable-open-buttons #t))
                      ;                (enable-open-buttons #f)))
                      ;          ]
                      ))

(send dialog show #t)