#lang racket
;Provides the names of labels and other items that are defined in info.rkt and used across
; tool.rkt and code-coverage.scrbl so that to change the value in all 3 locations only 
; info.rkt needs to be updated
(require setup/getinfo)
(require syntax/location)
(provide info-look-up 
         coverage-suffix
         tool-name
         button-label
         open-with-label)

(define info-proc (get-info/full 
                   (first (find-relevant-directories '(multi-file-code-coverage-info-file)))))

(define (info-look-up name) (if info-proc
                                (info-proc name (λ () (symbol->string name)))
                                 ;return something if we fail to find the info file so we dont crash
                                (symbol->string name)))

(define coverage-suffix (info-look-up 'coverage-suffix))
(define tool-name (info-look-up 'tool-name))
(define button-label (info-look-up 'button-label))
(define open-with-label (info-look-up 'open-with-label))
