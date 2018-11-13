(use test)
(use call-with-environment-variables envsubst)

(test-begin "envsubst")

(test-group "substitution"
  (test
    "beginning of line"
    "hello world"
    (call-with-environment-variables
      '(("HELLO" . "hello"))
      (lambda () (parse-line "$HELLO world"))))
  (test
    "middle of line"
    "hello there world"
    (call-with-environment-variables
      '(("THERE" . "there"))
      (lambda () (parse-line "hello $THERE world"))))
  (test
    "end of line"
    "hello world"
    (call-with-environment-variables
      '(("WORLD" . "world"))
      (lambda () (parse-line "hello $WORLD"))))
  (test
    "multiple"
    "hello world"
    (call-with-environment-variables
      '(("HELLO" . "hello") ("WORLD" . "world"))
      (lambda () (parse-line "$HELLO $WORLD"))))
  (test
    "adjacent"
    "helloworld"
    (call-with-environment-variables
      '(("HELLO" . "hello") ("WORLD" . "world"))
      (lambda () (parse-line "$HELLO$WORLD"))))
  ; FIXME
  (test
    "no variable at end"
    "hello$"
    (call-with-environment-variables
      '(("HELLO" . "hello"))
      (lambda () (parse-line "$HELLO$"))))
  ; FIXME
  (test
    "no variables at all"
    "$$$$$$"
    (call-with-environment-variables
      '()
      (lambda () (parse-line "$$$$$$"))))
  ; FIXME
  (test
    "lone $"
    "$ $ $ $ $ $"
    (call-with-environment-variables
      '()
      (lambda () (parse-line "$ $ $ $ $ $")))))

(test-group "parameter expansion"
  (test
    "trivial"
    "hello world"
    (call-with-environment-variables
      '(("HELLO" . "hello"))
      (lambda () (parse-line "${HELLO} world"))))
  (test
    "use-default-if-unset: unset"
    "hello world"
    (call-with-environment-variables
      '()
      (lambda () (parse-line "${HELLO-hello} world"))))
  (test
    "use-default-if-unset: null"
    " world"
    (call-with-environment-variables
      '(("HELLO" . ""))
      (lambda () (parse-line "${HELLO-hello} world"))))
  (test
    "use-default-if-unset: not null"
    "hello world"
    (call-with-environment-variables
      '(("HELLO" . "hello"))
      (lambda () (parse-line "${HELLO-goodbye} world"))))
  (test
    "use-default-if-unset-or-null: unset"
    "hello world"
    (call-with-environment-variables
      '()
      (lambda () (parse-line "${HELLO:-hello} world"))))
  (test
    "use-default-if-unset-or-null: null"
    "hello world"
    (call-with-environment-variables
      '(("HELLO" . ""))
      (lambda () (parse-line "${HELLO:-hello} world"))))
  (test
    "use-default-if-unset-or-null: not null"
    "hello world"
    (call-with-environment-variables
      '(("HELLO" . "hello"))
      (lambda () (parse-line "${HELLO:-goodbye} world"))))
  (test-error
    "indicate-error-if-unset: unset"
    (call-with-environment-variables
      '()
      (lambda () (parse-line "${HELLO?} world"))))
  (test
    "indicate-error-if-unset: null"
    " world"
    (call-with-environment-variables
      '(("HELLO" . ""))
      (lambda () (parse-line "${HELLO?goodbye} world"))))
  (test
    "indicate-error-if-unset: not null"
    "hello world"
    (call-with-environment-variables
      '(("HELLO" . "hello"))
      (lambda () (parse-line "${HELLO?goodbye} world"))))
  (test-error
    "indicate-error-if-unset-or-null: unset"
    (call-with-environment-variables
      '()
      (lambda () (parse-line "${HELLO:?} world"))))
  (test-error
    "indicate-error-if-unset-or-null: null"
    (call-with-environment-variables
      '(("HELLO" . ""))
      (lambda () (parse-line "${HELLO:?goodbye} world"))))
  (test
    "indicate-error-if-unset-or-null: not null"
    "hello world"
    (call-with-environment-variables
      '(("HELLO" . "hello"))
      (lambda () (parse-line "${HELLO:?goodbye} world"))))
  (test-error
    "missing trailing brace"
    (call-with-environment-variables
      '(("HELLO" . "hello"))
      (lambda () (parse-line "${HELLO world"))))
  ; FIXME this should be `bad substitution`
  (test-error
    "empty identifier"
    (call-with-environment-variables
      '()
      (lambda () (parse-line "${}"))))
  ; FIXME this should be `bad substitution`
  (test-error
    "invalid identifier"
    (call-with-environment-variables
      '()
      (lambda () (parse-line "${{whoops}")))))

(test-end "envsubst")

(test-exit)
