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
      (lambda () (parse-line "hello $WORLD")))))

(test-end "envsubst")

(test-exit)
