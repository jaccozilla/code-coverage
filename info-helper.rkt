#lang racket
;Provides the names of labels and other items that are used across info.rkt, tool.rkt, 
; and code-coverage.scrbl so that to change the value in all 3 locations only info.rkt
; needs to be updated
(require setup/getinfo)
(provide info-look-up 
         coverage-suffix
         tool-name
         button-label
         open-with-label)

(define info-proc (get-info (list "code-coverage")))
(define (info-look-up name) (info-proc name (λ () (symbol->string name))))

(define coverage-suffix (info-look-up 'coverage-suffix))
(define tool-name (info-look-up 'tool-name))
(define button-label (info-look-up 'button-label))
(define open-with-label (info-look-up 'open-with-label))
