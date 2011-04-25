#lang racket/base
  (require drracket/tool
           drracket/tool-lib
           drracket/private/debug
           drracket/private/rep
           drracket/private/get-extend
           racket/class
           racket/gui/base
           racket/unit
           racket/serialize
           racket/port
           racket/list
           mrlib/switchable-button
           errortrace/errortrace-lib
           compiler/cm)
  (provide tool@)
  
  
  
  (define tool@
    (unit
      (import drracket:tool^ );drracket:debug^)
      (export drracket:tool-exports^)
 
      
      (define coverage-button-mixin
        (mixin (drracket:unit:frame<%>) ()
          (super-new)
          (inherit get-button-panel
                   get-definitions-text)
          (inherit register-toolbar-button register-toolbar-buttons get-tabs get-current-tab get-interactions-text)
          
          (begin 
        
            (define (get-coverage)
              (begin
                (define frame (new frame% [label "Example"]))
                (send frame show #t)
                )
              )
            
            (define (load-coverage)
              ;(if (send (get-current-tab) get-test-coverage-info-visible?)
              (let* ([current-tab (get-current-tab)]
                     [interactions-text (get-interactions-text)]
                     [test-coverage-info-ht (send interactions-text get-test-coverage-info)])
                (if test-coverage-info-ht
                    (begin
                      ;display test coverage to each tab
                      (map (λ (t) (send t show-test-coverage-annotations test-coverage-info-ht #f #f #f)) (get-tabs))
                     
                      (get-choices-from-user
                       "Code Coverage"
                       "Covered Files:"
                       (make-coverage-report test-coverage-info-ht)
                       )
                      )
                    (message-box "Code Coverage" "Run the program before attempting to load Code Coverage Information"))
                )
              
              ;(message-box "Code Coverage" "Run the program before attempting to load Code Coverage Information"))
              )
            
            (let* ((save-button
                    (new switchable-button%
                         (label "Save Code Coverage")
                         (callback (λ (button)
                                     ;(run-with-coverage)))
                                     (get-coverage)))
                         (parent (get-button-panel))
                         (bitmap code-coverage-bitmap)
                         ))
                   (load-button
                    (new switchable-button%
                         (label "Load Code Coverage")
                         (callback (λ (button)
                                     (load-coverage)))
                         (parent (get-button-panel))
                         (bitmap code-coverage-bitmap)
                         ))
                   )
              
              ;(register-toolbar-button save-button)
              (register-toolbar-buttons (list save-button load-button))
              (send (get-button-panel) change-children
                    (λ (l)
                      (cons load-button (remq load-button l))))
              
              (send (get-button-panel) change-children
                    (λ (l)
                      (cons save-button (remq save-button l))))
              
              ))))
      
      
  
      (define code-coverage-bitmap 
        (let* ((bmp (make-bitmap 16 16))
               (bdc (make-object bitmap-dc% bmp)))
          (send bdc erase)
          (send bdc set-smoothing 'smoothed)
          (send bdc set-pen "black" 1 'transparent)
          (send bdc set-brush "forest green" 'solid)
          (send bdc draw-rectangle 2 5 12 9)
          (send bdc set-brush "maroon" 'solid)
          (send bdc draw-rectangle 11 5 14 9)
          (send bdc set-bitmap #f)
          bmp))
     
      
      (define (get-temp-coverage-file source-file)
        (begin
          (define-values (file-base file-name must-be-dir) (split-path source-file))
          (define temp-coverage-file-name (string-append (path->string file-name) ".txt"))
          (define temp-coverage-file (build-path (find-system-path 'temp-dir) "drracket" temp-coverage-file-name))
          temp-coverage-file))
      
      (define (test-coverage-info-file->test-covearge-info coverage-file)
        (make-hasheq (map (lambda (element)
                            (let* ([key (car element)]
                                   [value (cdr element)])
                              (cons (datum->syntax #f (void) (list (deserialize (car key)) (cadddr key) 1 (cadr key) (caddr key))) (mcons (car value) (cdr value)))))
                          ;(cons (datum->syntax #f (void) (list (deserialize (car key)) 1 1 (cadr key) (caddr key))) (mcons #t #f))))
                          (read (open-input-file coverage-file)))))
      
      
      (define (make-coverage-report test-coverage-info-ht)
        (let* ([file->lines-ht (make-hash)])
          (begin
            (hash-for-each test-coverage-info-ht 
                           (λ (key value)
                             (let* ([line (syntax-line key)]
                                    [source (format "~a" (syntax-source key))]
                                    [covered? (mcar value)]
                                    [file->lines-value (hash-ref file->lines-ht source (list))])
                               (hash-set! file->lines-ht source 
                                          (if (or covered? (member line file->lines-value))
                                              file->lines-value
                                              (sort (append file->lines-value (list line)) <))))
                             ))
            (let* ([test-coverage-info-list (sort (sort (hash->list file->lines-ht) 
                                                        (λ (a b) (string<? (first a) (first b))))
                                                  (λ (a b) (eq? 0 (length (rest b)))))]                                         
                   [test-coverage-info-strings (map (λ (item) (format "~a: ~a" (first item) (rest item))) test-coverage-info-list)])
              test-coverage-info-strings))))

      
      (define (read-syntax-from-string str) 
        (with-input-from-string str 
                                (λ () 
                                  (let ([stx (read-syntax 'read-from-a-string)]) 
                                    stx))))
      
      (define orig-load/use-compiled; (parameterize ([manager-trace-handler "yes"])
                                       ;(fprintf (current-error-port) "before: ~a\n" (manager-trace-handler))
                                       (current-load/use-compiled)
                                       ;(fprintf (current-error-port) "after: ~a\n" (manager-trace-handler)))
        )
      
  
      (define (phase1) (void))
      (define (phase2) (void))
  
      (drracket:get/extend:extend-unit-frame coverage-button-mixin)))