; Ripped from https://github.com/klutometis/call-with-environment-variables
; bc I am too lazy to port `hahn` :P
; Copyright (C), 2015 Peter Danenberg
(module call-with-environment-variables (call-with-environment-variables)
  (import (chicken base) (chicken process-context) scheme)
  (define (call-with-environment-variables variables thunk)
    (let ((pre-existing-variables
            (map (lambda (var-value)
                   (let ((var (car var-value)))
                     (cons var (get-environment-variable var))))
                 variables)))
      (dynamic-wind
        (lambda () (void))
        (lambda ()
          (for-each (lambda (var-value)
                      (set-environment-variable! (car var-value) (cdr var-value)))
                    variables)
          (thunk))
        (lambda ()
          (for-each (lambda (var-value)
                      (let ((var (car var-value))
                            (value (cdr var-value)))
                        (if value
                            (set-environment-variable! var value)
                            (unset-environment-variable! var))))
                    pre-existing-variables))))))
