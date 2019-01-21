# envsubst

Clone of [envsubst](https://www.gnu.org/software/gettext/manual/html_node/envsubst-Invocation.html). Supports [parameter expansion](http://pubs.opengroup.org/onlinepubs/009695399/utilities/xcu_chap02.html#tag_02_06_02).

## usage

```bash
envsubst [VARIABLE] ... < INPUT > OUTPUT
```

## semantics

I try to be true to the semantics of the original `envsubst` which are slightly
different from the semantics in a shell. These differences include:

* Most "bad substitutions" are allowed
    * `${}` -> `${}`
    * `${{foo}` -> `${{foo}`
* Nested expansions are allowed:
    * `export foo=bar ${${foo}}` -> `${bar}`
    * ...but they only expand at depth=1 ;p

Differences from the original `envsubst` include:

* Shell builtin variables like `$$` and `$0` are not supported
* Unset variables throw an error by default (use `${parameter-}` or `${parameter+}` to ignore unset variables)
* Instead of using [SHELL-FORMAT](https://unix.stackexchange.com/questions/294378/replacing-only-specific-variables-with-envsubst) whitelisted variable names are passed as arguments

## parameter expansion

The following expansions are supported:

| form | set and not null | set but null | unset |
| ---- | ---------------- | ------------ | ----- |
| `${parameter:-word}` | substitute parameter | substitute word | substitute word |
| `${parameter-word}` | substitute parameter | substitute null | substitute word |
| `${parameter:?[word]}` | substitute parameter | error, exit | error, exit |
| `${parameter?[word]}` | substitute parameter | substitute null | error, exit |
| `${parameter:+word}` | substitute word | substitute null | substitute null |
| `${parameter+word}` | substitute word | substitute word | substitute null |

## testing

```bash
chicken-install -n && csi tests/run.scm
```

## todo if ur bored

* Escapes
* Inplace
* Add reference tests against envsubst
* Add --posix
