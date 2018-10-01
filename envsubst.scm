(use matchable posix utf8)

(define (getenv name-chars)
  (let* ((name (list->string name-chars))
         (value (get-environment-variable name)))
    (if (not value)
      (error "no such environment variable" name)
      (string->list value))))

(define (parse-identifier tokens parameter-expansion?)
  (let loop ((identifier '())
             (tokens tokens))
    (if (null? tokens)
      (if parameter-expansion?
        (error "unexpected EOF while looking for matching `}")
        (getenv identifier))
      (match tokens
             ((or (and parameter-expansion? (#\} tail ...))
                  (and tail ((? char-whitespace?) (? char?) ...)))
              (append (getenv identifier) (parse tail)))
             ((token tail ...) (loop (append identifier (list token)) tail))))))

(define (parse tokens)
  (if (null? tokens)
    tokens
    (match tokens
           ((#\$ #\{ tail ...) (parse-identifier tail #t))
           ((#\$ tail ...) (parse-identifier tail #f))
           ((token tail ...)
            (cons token (parse tail))))))

(define (main _)
  (let ((line (read-line)))
    (unless (eof-object? line)
      (begin (write-line (list->string (parse (string->list line))))
             (main _)))))
