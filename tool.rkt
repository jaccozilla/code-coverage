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

(define coverage-suffix ".rktcov")
(define coverage-label "Code Coverage")
(define button-label "Load Code Coverage")

(define tool@
  (unit
    (import drracket:tool^ )
    (export drracket:tool-exports^)

    
    (define coverage-button-mixin
      (mixin (drracket:unit:frame<%>) ()
        (super-new)
        (inherit get-button-panel
                 get-definitions-text 
                 register-toolbar-button 
                 get-tabs 
                 get-current-tab
                 get-interactions-text)
        
        ;Applies code coverage highlighting to all currently open tabs. Displays 
        ; a dialog with a list of files covered by the currently in focus file
        ; and allows the user to select a file to open and jump to. Coverage
        ; information is first looked for in DrRacket (the program has been run)
        ; If the program has not been run, or has been modified to invalidate it's
        ; coverage information, look in the compiled directory for coverage info
        ; and display that.
        (define (load-coverage)
          (let* ([current-tab (get-current-tab)]
                 [current-frame (send current-tab get-frame)]
                 [interactions-text (get-interactions-text)]
                 [test-coverage-info-ht (send interactions-text get-test-coverage-info)]
                 [source-file (send (send current-tab get-defs) get-filename)]
                 [coverage-file (get-temp-coverage-file source-file)])
            ;(fprintf (current-error-port) "coverage file: ~a\n" coverage-file)
            (if test-coverage-info-ht
                (let* ([coverage-report-list (make-coverage-report test-coverage-info-ht)]
                       [choice-index-list (get-choices-from-user
                                           coverage-label
                                           "Covered Files:"
                                           (map (λ (item) (format "~a" (first item))) coverage-report-list))])
                  (when choice-index-list
                    (map (λ (choice-index) 
                           (let* ([coverage-report-item (list-ref coverage-report-list choice-index)])
                             (send current-frame open-in-new-tab (first coverage-report-item))
                             (uncoverd-lines-dialog (rest coverage-report-item))
                             )) 
                         choice-index-list))
                  ;(map (λ (t) (send t show-test-coverage-annotations test-coverage-info-ht #f #f #f)) (get-tabs))
                  (save-test-coverage-info test-coverage-info-ht coverage-file)
                  (map (λ (t) (send t show-test-coverage-annotations test-coverage-info-ht #f #f #f)) (get-tabs))
                  
                  )
                (message-box coverage-label "Run the program before attempting to load Code Coverage Information"))
            )
          )
        
        ;Create the load coverage button and add it to the menu bar in DrRacket
        (define load-button (new switchable-button%
                                 (label button-label)
                                 (callback (λ (button)
                                             (load-coverage)))
                                 (parent (get-button-panel))
                                 (bitmap code-coverage-bitmap)
                                 ))
        
        (register-toolbar-button load-button)
        (send (get-button-panel) change-children
              (λ (l)
                (cons load-button (remq load-button l))))     
        ))


    ;Get the name and location of a code coverage file based on the name of a source file.
    ;Also creates the code coverage dir if it does not exisit
    ;path? -> path?
    (define (get-temp-coverage-file source-file)
      (begin
        (define-values (file-base file-name must-be-dir) (split-path source-file))
        (define temp-coverage-file-name (path-replace-suffix file-name coverage-suffix))
        (define temp-coverage-dir (build-path file-base "compiled"))
        (define temp-coverage-file (build-path temp-coverage-dir temp-coverage-file-name))
        (when (not (directory-exists? temp-coverage-dir))
          (make-directory temp-coverage-dir))
        temp-coverage-file
        ))
    
    ;writes the test-coiverage-info hash table to the given file so that "load-test-coverage-info"
    ;can reconstruct the hash table
    ;hasheq path -> void
    (define (save-test-coverage-info test-coverage-info coverage-file)
      (with-output-to-file coverage-file
        (lambda () (begin 
                     (write (hash-map test-coverage-info 
                                      (λ (key value)
                                        (cons (list (serialize (syntax-source key)) 
                                                    (syntax-position key) 
                                                    (syntax-span key)
                                                    (syntax-line key)) 
                                              value))
                                      
                                      ))))
          #:mode 'text
          #:exists 'replace
        ))
    
    ;Convert the coverage file to a test-coverage-info hash table
    ;path -> hasheq
    (define (load-test-coverage-info coverage-file)
      (make-hasheq (map (lambda (element)
                          (let* ([key (car element)]
                                 [value (cdr element)])
                            (cons (datum->syntax #f (void) (list (deserialize (car key)) (cadddr key) 1 (cadr key) (caddr key))) (mcons (car value) (cdr value)))))
                        (read (open-input-file coverage-file)))))
    
    
    
    
    ;Takes a test-coverage-info-ht and returns a sorted (alphibetical, but with uncovered
    ;files first) list of file name - uncoverd lines pairs.
    ;hasheq -> (list (pair string? (list integer?)))
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
                 )
            test-coverage-info-list))))
    
    ; the dialog that displays the uncovered lines. Not a message box so the user can interact
    ; with DrRacket without having to close the dialog.
    (define (uncoverd-lines-dialog lines)
      (let* ([dialog (instantiate frame% (coverage-label))])
        (new message% [parent dialog]
             [label (format "Uncoverd Lines: ~a" lines)]	 
             [min-width 200]	 
             [min-height 40])
        (new button% [parent dialog] [label "Close"]
             [callback (λ (b e) (send dialog show #f))])
        (send dialog show #t)))
    
    ; Graphic for the code coverage button
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
    
    
    (define (phase1) (void))
    (define (phase2) (void))
    
    (drracket:get/extend:extend-unit-frame coverage-button-mixin)))