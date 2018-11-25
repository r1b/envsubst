(use envsubst)

(define (main args)
  (begin
    (unless (null? args)
      (set-variables! args))
    (let ((line (read-line)))
      (unless (eof-object? line)
        (begin (write-line (parse-line line))
               (main _)
               (exit))))))
