; Lexer state
(define READ-TEXT 'text)
(define READ-IDENTIFIER 'identifier)

(define-record lexer chars status tokens)
(define-record token value)

(define (parse tokens)
  (let ((env (get-environment-variables)))
    (let loop ((line "")
               (tokens tokens)) ; FIXME
      (if (null? tokens)
        line
        (match tokens
               [(($ token '$') ($ token '{') ($ token identifier) ($ token '}') tail)
                (loop (string-append line (assq (list->string identifier) env)) tail)]
               [(($ token v) tail)
                (loop (string-append line (string v)) tail)])))))


(define (lex-text l)
  (let* ((chars (lexer-chars l))
         (head (car chars))
         (tail (cdr chars)))
    (if (and (char=? head '$') (not (null? tail)) (char=? (caar chars) '{'))
      (lexer (cddr chars) READ-IDENTIFIER (append (lexer-tokens l) (list (token '$') (token '{'))))
      (lexer (cdr chars) READ-TEXT (append (lexer-tokens l) (list (token head)))))))

(define (lex-identifier l)
  (let loop ((identifier '())
             (l l)) ; FIXME
    (let* ((chars (lexer-chars l))
           (head (car chars))
           (tail (cdr chars)))
      (cond [(char=? head '}') (lexer tail READ-TEXT (append (lexer-tokens l) (list (token identifier) (token '}'))))]
            [(null? tail) (lexer tail READ-TEXT (append (lexer-tokens l) (map token identifier)))]
            [else (loop (append identifier (list cur)) (lexer tail READ-IDENTIFIER (lexer-tokens l)))]))))

(define (lex-finish l)
  (append (lexer-tokens l) (token #\newline)))

(define (lex line)
  (let loop ((l (lexer (string->list line) READ-TEXT '())))
    (if (null? (lexer-chars l))
      (lex-finish l)
      (match (lexer-state l)
             ['READ-TEXT (loop (lex-text l))]
             ['READ-IDENTIFIER (loop (lex-identifier l))]))))

(define (envsubst)
  (let ((line (read-line)))
    (unless (eof-object? line)
      (begin (write-string (parse (lex line)))
             (envsubst)))))
