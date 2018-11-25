(use envsubst)

(define (parse-lines)
  (let ((line (read-line)))
    (unless (eof-object? line)
      (begin (write-line (parse-line line))
             (parse-lines)
             (exit)))))

(define (main args)
  (begin
    (unless (null? args)
      (set-variables! args))
    (parse-lines)))
