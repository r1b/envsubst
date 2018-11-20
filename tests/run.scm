(use call-with-environment-variables envsubst test)

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
  (test
    "no variable at end"
    "hello$"
    (call-with-environment-variables
      '(("HELLO" . "hello"))
      (lambda () (parse-line "$HELLO$"))))
  (test
    "no variables at all"
    "$$$$$$"
    (call-with-environment-variables
      '()
      (lambda () (parse-line "$$$$$$"))))
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
    "use-default-if-unset: set"
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
    "use-default-if-unset-or-null: set"
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
    "indicate-error-if-unset: set"
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
    "indicate-error-if-unset-or-null: set"
    "hello world"
    (call-with-environment-variables
      '(("HELLO" . "hello"))
      (lambda () (parse-line "${HELLO:?goodbye} world"))))
  (test
    "use-alternative-value-if-set-or-null: set"
    "goodbye world"
    (call-with-environment-variables
      '(("HELLO" . "hello"))
      (lambda () (parse-line "${HELLO+goodbye} world"))))
  (test
    "use-alternative-value-if-set-or-null: unset"
    " world"
    (call-with-environment-variables
      '()
      (lambda () (parse-line "${HELLO+goodbye} world"))))
  (test
    "use-alternative-value-if-set-or-null: null"
    "goodbye world"
    (call-with-environment-variables
      '(("HELLO" . ""))
      (lambda () (parse-line "${HELLO+goodbye} world"))))
  (test
    "use-alternative-value-if-set: set"
    "goodbye world"
    (call-with-environment-variables
      '(("HELLO" . "hello"))
      (lambda () (parse-line "${HELLO:+goodbye} world"))))
  (test
    "use-alternative-value-if-set: unset"
    " world"
    (call-with-environment-variables
      '()
      (lambda () (parse-line "${HELLO:+goodbye} world"))))
  (test
    "use-alternative-value-if-set: null"
    " world"
    (call-with-environment-variables
      '(("HELLO" . ""))
      (lambda () (parse-line "${HELLO:+goodbye} world"))))
  (test-error
    "missing trailing rbrace"
    (call-with-environment-variables
      '(("HELLO" . "hello"))
      (lambda () (parse-line "${HELLO world"))))
  (test
    "extra trailing rbrace"
    "hello}"
    (call-with-environment-variables
      '(("HELLO" . "hello"))
      (lambda () (parse-line "${HELLO}}"))))
  (test
    "empty"
    "${}"
    (call-with-environment-variables
      '(("HELLO" . "hello"))
      (lambda () (parse-line "${}"))))
  (test
    "extra leading lbrace"
    "${{HELLO}"
    (call-with-environment-variables
      '(("HELLO" . "hello"))
      (lambda () (parse-line "${{HELLO}"))))
  (test
    "nested expansion"
    "${hello}"
    (call-with-environment-variables
      '(("HELLO" . "hello"))
      (lambda () (parse-line "${${HELLO}}")))))

(test-end "envsubst")

(test-exit)
