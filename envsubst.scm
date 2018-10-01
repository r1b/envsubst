(use matchable posix utf8)

(define (getenv name)
  (let ((value (get-environment-variable name)))
    (if (not value)
      (error "no such environment variable" name)
      value)))

(define (parse-identifier tokens)
  (let loop ((identifier '())
             (tokens tokens))
    (if (null? tokens)
      identifier
      (match tokens
             ((#\} tail ...) (append (string->list (getenv (list->string identifier))) (parse tail)))
             ((token tail ...) (loop (append identifier (list token)) tail))))))

(define (parse tokens)
  (if (null? tokens)
    tokens
    (match tokens
           ((#\$ #\{ tail ...) (parse-identifier tail))
           ((token tail ...)
            (cons token (parse tail))))))

(define (main _)
  (let ((line (read-line)))
    (unless (eof-object? line)
      (begin (write-line (list->string (parse (string->list line))))
             (main _)))))
