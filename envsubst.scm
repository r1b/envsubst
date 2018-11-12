(module envsubst (parse-line)
  (import scheme chicken extras)
  (use matchable posix utf8)

  (define (getenv tokens)
    (let* ((name (list->string tokens))
           (value (get-environment-variable name)))
      (if (not value)
          (error "no such environment variable" name)
          (string->list value))))

  (define (parse-variable-with-parameter-expansion tokens)
    (parse-variable tokens))

  (define (parse-variable tokens)
    (let loop ((identifier '())
               (tokens tokens))
      (match tokens
        ((and tail ((? char-whitespace?) (? char?) ...))
         (append (getenv identifier) (parse tail)))
        ((token tail ...) (loop (append identifier (list token)) tail))
        ('() (getenv identifier)))))

  (define (parse-identifier tokens parameter-expansion?)
    (let loop ((identifier '())
               (tokens tokens))
      (match tokens
        ((or (and parameter-expansion? (#\} tail ...))
             (and tail ((? char-whitespace?) (? char?) ...)))
         (append (getenv identifier) (parse tail)))
        ((token tail ...) (loop (append identifier (list token)) tail))
        ('() (if parameter-expansion?
                (error "unexpected EOF while looking for matching `}")
                (getenv identifier))))))

  (define (parse tokens)
    (match tokens
      ((#\$ #\{ tail ...) (parse-variable-with-parameter-expansion tail))
      ((#\$ tail ...) (parse-variable tail))
      ((token tail ...)
       (cons token (parse tail)))
      ('() tokens)))

  (define (parse-line line)
    (list->string (parse (string->list line))))

  (define (main _)
    (let ((line (read-line)))
      (unless (eof-object? line)
        (begin (write-line (parse-line line))
               (main _))))))
