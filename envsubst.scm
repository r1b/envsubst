(module envsubst (parse-line parse-variables set-variables!)
  (import scheme chicken extras)
  (use matchable posix utf8)

  ; ---------------------------------------------------------------------------

  (define VARIABLES '())

  ; ---------------------------------------------------------------------------

  (define (unset-error tokens)
    (error "environment variable is unset" (list->string tokens)))

  (define (get-environment-variable-from-whitelist name)
    (let ((variable (assoc name VARIABLES)))
      (and variable (cdr variable))))

  (define (getenv tokens unset-handler)
    (let* ((name (list->string tokens))
           (value (if (null? VARIABLES)
                      (get-environment-variable name)
                      (get-environment-variable-from-whitelist name))))
      (if (not value)
          (unset-handler tokens)
          (string->list value))))

  (define (getenv-strict tokens)
    (getenv tokens (lambda (tokens) (unset-error tokens))))

  (define (getenv-unset tokens)
    (getenv tokens (lambda (_) #f)))

  ; ---------------------------------------------------------------------------

  ; ${parameter-word}
  (define (use-default-if-unset parameter word)
    (let ((value (getenv-unset parameter)))
      (if (not value)
          word
          value)))

  ; ${parameter:-word}
  (define (use-default-if-unset-or-null parameter word)
    (let ((value (getenv-unset parameter)))
      (if (or (not value) (null? value))
          word
          value)))

  ; ${parameter?word}
  (define (indicate-error-if-unset parameter word)
    (let ((value (getenv-unset parameter)))
      (if (not value)
          (if (null? word) (unset-error parameter) (error word))
          value)))

  ; ${parameter:?word}
  (define (indicate-error-if-unset-or-null parameter word)
    (let ((value (getenv-unset parameter)))
      (if (or (not value) (null? value))
          (if (null? word) (unset-error parameter) (error word))
          value)))

  ; ${parameter+word}
  (define (use-alternative-value-if-set-or-null parameter word)
    (let ((value (getenv-unset parameter)))
      (if (or value (null? value))
          word
          '())))

  ; ${parameter:+word}
  (define (use-alternative-value-if-set parameter word)
    (let ((value (getenv-unset parameter)))
      (if (and value (not (null? value)))
          word
          '())))

  ; ---------------------------------------------------------------------------

  (define (parse-parameter-expansion tokens)
    (let loop ((parameter '())
               (tokens tokens))
      (match tokens
        ((#\- word ...) (use-default-if-unset parameter word))
        ((#\: #\- word ...) (use-default-if-unset-or-null parameter word))
        ((#\? word ...) (indicate-error-if-unset parameter word))
        ((#\: #\? word ...) (indicate-error-if-unset-or-null parameter word))
        ((#\+ word ...) (use-alternative-value-if-set-or-null parameter word))
        ((#\: #\+ word ...) (use-alternative-value-if-set parameter word))
        ((token tail ...) (loop (append parameter (list token)) tail))
        ('() (getenv-strict parameter)))))

  (define (parse-variable-with-parameter-expansion tokens)
    (let loop ((expression '())
               (tokens tokens))
      (match tokens
        ((#\} tail ...)
         (append (parse-parameter-expansion expression) (parse tail)))
        ((token tail ...) (loop (append expression (list token)) tail))
        ('() (error "unexpected EOF while looking for matching `}")))))

  (define (parse-variable tokens)
    (let loop ((identifier '())
               (tokens tokens))
      (match tokens
        ((and tail ((or (? char-whitespace?) #\$) (? char?) ...))
         (append (getenv-strict identifier) (parse tail)))
        ((token tail ...) (loop (append identifier (list token)) tail))
        ('() (getenv-strict identifier)))))

  (define (parse tokens)
    (match tokens
      ; FIXME pull out this "lookahead"
      ((#\$ #\{ (and head (not (or (? char-whitespace?) #\$ #\{ #\}))) tail ...)
       (parse-variable-with-parameter-expansion (cons head tail)))
      ((#\$ (and head (not (or (? char-whitespace?) #\$ #\{ #\}))) tail ...)
       (parse-variable (cons head tail)))
      ((token tail ...)
       (cons token (parse tail)))
      ('() tokens)))

  (define (parse-line line)
    (list->string (parse (string->list line))))

  ; ---------------------------------------------------------------------------

  (define (parse-variables variables)
    (map (lambda (name) (cons name (get-environment-variable name)))
         variables))

  ; ---------------------------------------------------------------------------

  (define (set-variables! variables)
    (set! VARIABLES (parse-variables variables))))
